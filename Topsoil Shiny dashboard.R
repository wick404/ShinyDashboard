library(shiny)
library(shinydashboard)
library(tmap)
library(sf)
library(DT)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(clValid)
library(clusterSim)
library(ClustGeo)
library(tidyr)
library(eurostat)
library(Rcpp)

##LOADING DATASET

lucasdf <- read.csv('LUCAS-SOIL-2018.csv')

##filtering out only croplands, barelands ang grasslands
filtered_df <- lucasdf %>% 
  filter(LC0_Desc %in% c("Cropland","Bareland","Grassland"))

##deleting certain columns
short_df1 <- filtered_df %>%
  dplyr::select( - c(NUTS_0, NUTS_1, NUTS_3, TH_LAT, TH_LONG, SURVEY_DATE, Elev, LC, LU, LC1_Desc,LU1_Desc, OC..20.30.cm., POINTID, CaCO3..20.30.cm., LC0_Desc, Depth  ))

##replacing LOD with NAs
short_df1 <- short_df1 %>%
  mutate(across(-NUTS_2, .fns = ~na_if(as.character(.), "<LOD"))) %>%
  mutate(across(-NUTS_2, as.numeric))

##replacing NAs with means of each NUTS2 Group
short_df1 <- short_df1 %>%
  group_by(NUTS_2) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

##grouping the rows by NUTS2 regions and taking the mean of the soil feature 
grouped <- aggregate(. ~ NUTS_2, data = short_df1, FUN = function(x) mean(x, na.rm = TRUE))

##renaming NUT_2 to geo so it would match the other dataset
full_df <- grouped %>% 
  rename(
    geo = NUTS_2
  )

##creating a country code based on the nuts2 region name, it will be useful later when i want to group by countries
full_df$country_code <- substr(full_df$geo,1,2)

## Adding the crop yield data to the dataset
croplist <- list("barley","soya","sugar beet","common wheat and spelt","rice","sunflowerseed","potatoes","common winter wheat and spelt")

##i cleaned the data previously, now just appending these datasets 
for (i in croplist) {
  file_path <- paste0(i, '.csv')
  if (file.exists(file_path)) {
    df_new <- read.csv(file_path)
    colnames(df_new)[colnames(df_new) == "OBS_VALUE"] <- i
    df_new <- df_new %>%
      dplyr::select(-c("DATAFLOW","LAST.UPDATE","freq","crops","strucpro","TIME_PERIOD","OBS_FLAG"))
    full_df <- left_join(full_df, df_new, by = "geo")
  }
}

##downloading geospatial data from GISCO
geodata <- get_eurostat_geospatial(nuts_level = 2, year = 2016)
map_data <- inner_join(geodata, full_df, by = "geo")

## FUNCTIONS - in the following part I am creating the functions

# creating a function by rcpp which later helps to find the index number of the max yield of every crop type
# so they can be used as cluster centroids
Rcpp::sourceCpp(code = '
#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
List getMaxYield(DataFrame kmeans_clean, CharacterVector croplist) {
  List max_yield;
  for (int i = 0; i < croplist.size(); i++) {
    std::string crop = Rcpp::as<std::string>(croplist[i]);
    NumericVector crop_data = kmeans_clean[crop];
    int max_index = std::distance(crop_data.begin(), std::max_element(crop_data.begin(), crop_data.end())) + 1;
    max_yield[crop] = max_index;
  }
  return max_yield;
}
')

#creating k means clustering function
perform_kmeans <- function(data, cols, n_centers, n_starts = 25, features) {
  ##cols variable takes certain columns from our data and the clustering will be done only on those
  kmeans_data <- data[, cols]
  km_result <- kmeans(kmeans_data, centers = n_centers, nstart = n_starts)
  data$cluster <- km_result$cluster
  
  ##finding the max crop yield of the crop selected by the user
  max_yield <- which.max(data[[features]])
  
  ##finding which cluster it belongs to
  max_yield_cluster <- data$cluster[max_yield]
  
  ##filtering out and returning every data point that belongs to the same cluster so it could be later displayed on the map and in the output table
  cluster_data <- data[data$cluster == max_yield_cluster, ]
  return(cluster_data)
}

##function with defensive programming
get_top_entities <- function(group_var, feature) {
  if (!exists("full_df")) {
    stop("Data frame 'full_df' does not exist.")
  }
  
  if (!is.data.frame(full_df)) {
    stop("'full_df' must be a data frame.")
  }
  
  if (!group_var %in% names(full_df)) {
    stop(paste("Group variable", group_var, "is not present in 'full_df'."))
  }
  if (!feature %in% names(full_df)) {
    stop(paste("Feature", feature, "is not present in 'full_df'."))
  }
  
  ##aggregating data, grouping by countries so data can be plotted not only based on regions but also countries, and yearly crop yield can also be aggregated by countries
  aggregated_data <- full_df %>%
    group_by(!!sym(group_var)) %>%
    summarise(total_yield = sum(!!sym(feature), na.rm = TRUE))
  
  if (nrow(aggregated_data) == 0) {
    stop("Aggregated data is empty. Check the input data and variables.")
  }
  
  ##filtering out the top5 countries/regions
  top_entities <- aggregated_data %>%
    top_n(5, total_yield)
  
  return(top_entities)
}




##UI

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Crop Yield Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Crop Yield", tabName = "Crop_Yield", icon = icon("chart-line")),
      menuItem("Top Regions", tabName = "Top_Regions", icon = icon("map-marked-alt")),
      menuItem("Crop to Grow", tabName = "Crop_to_grow", icon = icon("seedling")),
      menuItem("Explore Data", tabName = "Explore_Data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Crop_Yield",
              fluidRow(
                box(title = "Select Crop Type", status = "success", solidHeader = TRUE, 
                    selectInput("features", "Crop:", 
                                choices = c("barley", "soya", "sugar beet", "common wheat and spelt", "rice", "sunflowerseed", "potatoes", "common winter wheat and spelt")),
                    width = 3),
                valueBoxOutput("total_yield", width = 3),
                valueBoxOutput("biggest_producer", width = 3)
              ),
              fluidRow(
                box(title = "Map View", status = "success", solidHeader = TRUE, tmapOutput("map1"), width = 7),
                box(title = "Top Crop Yields", status = "success", solidHeader = TRUE, plotOutput("best_regions"), width = 5)
              )
      ),
      tabItem(tabName = "Explore_Data",
              fluidRow(
                box(title = "Data Exploration", status = "success", solidHeader = TRUE, dataTableOutput("full_df"), width = 12)
              )
      ),
      tabItem(tabName = "Top_Regions",
              
              fluidRow(
                box(title = "Select Crop Type", status = "success", solidHeader = TRUE, 
                    selectInput("features_top_regions", "Crop:", 
                                choices = c("barley", "soya", "sugar beet", "common wheat and spelt", "rice", "sunflowerseed", "potatoes", "common winter wheat and spelt")),
                    width = 4),
                box(title = "Select Number of Clusters", status = "success", solidHeader = TRUE,
                    sliderInput("num_clusters", "Number of Clusters:", min = 1, max = 30, value = 20, step = 1),
                    p("The higher number of clusters you choose, fewer but more accurate regions you may get."),
                    width = 4)
              ),
              fluidRow(
                box(title = "Map View", status = "success", solidHeader = TRUE, tmapOutput("map2"), width = 6),
                box(title = "Suitable Regions", status = "success", solidHeader = TRUE, dataTableOutput("cluster_data"), width = 6)
              )
      ),
      tabItem(tabName = "Crop_to_grow",
              fluidRow(
                box(title = "Enter Soil Properties", status = "success", solidHeader = TRUE, width = 3,
                    numericInput("input_pH_CaCl2", "pH (CaCl2):", value = 10, min = 0, max = 1000),
                    numericInput("input_pH_H2O", "pH (H2O):", value = 6.0, min = 0, max = 1000),
                    numericInput("input_EC", "EC (dS/m):", value = 5, min = 0, max = 1000),
                    numericInput("input_OC", "Organic Carbon (%):", value = 10, min = 0, max = 1000),
                    numericInput("input_CaCO3", "Calcium Carbonate (%):", value = 15, min = 0, max = 1000),
                    numericInput("input_P", "Phosphorus (mg/kg):", value = 1, min = 0, max = 1000),
                    numericInput("input_N", "Nitrogen (%):", value = 1.5, min = 0, max = 1000),
                    numericInput("input_K", "Potassium (mg/kg):", value = 200, min = 0, max = 1000)
                ),
                column(width = 6,
                       valueBoxOutput("crop_to_grow", width = 12),
                       #box(title = "Crop to Grow", status = "primary", solidHeader = TRUE, textOutput("crop_to_grow"), width = 12),
                       box(title = "Yearly Yield By Country", status = "success", solidHeader = TRUE, plotOutput("country_dist"), width = 12)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ##reactive function used to create the a valuebox about yearly total crop yield of a country and also a pie chart about the top 5 biggest crop producer country
  top_countries <- reactive({
    get_top_entities("country_code", input$features)
  })
  
  ##same as top_countries but not for countries, for NUTS2 regions (regions of EU countries)
  top_regions <- reactive({
    get_top_entities("geo", input$features)
  })
  
  ##reactive function, value is used in valueboxes and plots,
  selected_crop <- reactive({
    croplist <- list("barley","sugar beet","common wheat and spelt","rice","sunflowerseed","potatoes")
    
    ##clustering the data based on topsoil features
    cols_to_check <- c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")
    kmeans_clean <- full_df[!rowSums(is.na(full_df[cols_to_check])), ]
    kmeans_data <- scale(kmeans_clean[, cols_to_check])
    
    ##saving mean and sd for user input standardization later
    train_mean<- colMeans(full_df[, cols_to_check], na.rm = TRUE)
    train_sd <- apply(full_df[, cols_to_check], 2, sd, na.rm = TRUE)
    
    ##using the getMaxYield function which was created in rcpp to find the index of every crop's max crop yield
    max_yield <- getMaxYield(kmeans_clean, as.character(croplist))
    
    #previously, before rewriting in in rcpp, thats how i found the max indexes
    #max_yield <- lapply(croplist, function(crop) which.max(kmeans_clean[[crop]]))
    
    ## doing the clustering
    centroids <- data.frame(
      pH_CaCl2 = numeric(),
      pH_H2O = numeric(),
      EC = numeric(),
      OC = numeric(),
      CaCO3 = numeric(),
      P = numeric(),
      N = numeric(),
      K = numeric()
    )
    
    ##creating a dataframe out of the soil features of each crop's max yield territory
    centroids <- do.call(rbind, lapply(croplist, function(crop) kmeans_data[max_yield[[crop]],]))
    
    ##renaming centroids columns
    colnames(centroids) <- c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")
    
    ##user's input of soil properties
    user_input <- data.frame(
      pH_CaCl2 = input$input_pH_CaCl2, 
      pH_H2O = input$input_pH_H2O, 
      EC = input$input_EC, 
      OC = input$input_OC, 
      CaCO3 = input$input_CaCO3, 
      P = input$input_P, 
      N = input$input_N, 
      K = input$input_K
    )
    
    ##scaling the user input data with the previously saved mean and sd
    scaled_user_input <- scale(user_input[, cols_to_check], center = train_mean, scale = train_sd)
    
    ##calculating eucledian distance between all centroids and scaled user input to see what soil property it is the closest to
    distances <- apply(centroids, 1, function(x) sqrt(sum((x - scaled_user_input)^2)))
    
    ##finding the index of the nearest centroid based on the eucledian distance
    nearest_centroid_index <- which.min(distances)
    
    ##returning the respective crop name from the crop list as the best crop based on users soil properties
    bestcrop = croplist[[nearest_centroid_index]]
    return(bestcrop)
  })
  
  ##this value box returns the best crop result from above
  output$crop_to_grow <- renderValueBox({
    valueBox(
      "Most suitable crop:",
      selected_crop(),
      icon = icon('leaf'),
      color = "green"
    )
  })
  
  ##based on users input and kmeans clustering, this maps shows which regions would be the most suitable for the selected crop
  ##after the user selects a crop, it finds the territory where the yearly crop yield is the biggest and displays its cluster on the map
  output$map2 <- renderTmap({
    cols_to_check <- c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")
    kmeans_clean <- full_df[!rowSums(is.na(full_df[cols_to_check])), ]
    cluster_data <- perform_kmeans(kmeans_clean, cols_to_check, input$num_clusters, features = input$features_top_regions)
    
    geodata <- get_eurostat_geospatial(nuts_level = 2, year = 2016)
    best_region_map <- inner_join(geodata, cluster_data, by = "geo")
    
    tm_shape(geodata,
             projection = "EPSG:3035",
             xlim = c(2400000, 7800000),
             ylim = c(1320000, 5650000)
    ) +
      tm_fill("lightgrey") +
      tm_shape(best_region_map) +
      tm_polygons("cluster",
                  title = paste("Most suitable regions to grow", input$features_top_regions),
                  palette = "Greens")
  })
  
  ##this table just shows the soil properties of the territories displayed on the map above
  output$cluster_data <- renderDataTable({
    cols_to_check <- c("pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N", "K")
    kmeans_clean <- full_df[!rowSums(is.na(full_df[cols_to_check])), ]
    cluster_data <- perform_kmeans(kmeans_clean, cols_to_check, input$num_clusters, features = input$features_top_regions)
  })
  
  ##this map just simply displays the yearly crop yield of each NUTS2 region of the selected crop
  output$map1 <- renderTmap({
    tm_shape(geodata,
             projection = "EPSG:3035",
             xlim = c(2400000, 7800000),
             ylim = c(1320000, 5650000)
    ) +
      tm_fill("lightgrey") +
      tm_shape(map_data) +
      tm_polygons(input$features,
                  title = paste(input$features, "crop yield in 2018 by NUTS2 regions"),
                  palette = "Greens")
  })
  
  ##this is the full dataframe for the Data Exploration tab
  output$full_df <- renderDataTable({
    full_df
  })
  
  ##this value box, based on the selected crop, displays the total yearly yield of that crop in the EU.
  output$total_yield <- renderValueBox({
    req(full_df)
    #sum up the selected crop yield across all regions
    total_yield <- sum(full_df[[input$features]], na.rm = TRUE)  
    
    valueBox(
      format(total_yield, big.mark = ","),
      paste("Total Yearly Yield of", input$features, "in the EU."),  #dynamic title
      icon = icon("leaf"), 
      color = "green" 
    )
  })
  
  ##this value box displays the country code of the country where the most amount of selected crop was grown
  output$biggest_producer <- renderValueBox({
    top_countries <- top_countries()
    ##finding the biggest producer of the selected crop
    top1 <- top_countries[which.max(top_countries$total_yield),]  
    
    valueBox(
      format(top1$total_yield, big.mark = ","),  
      paste("Most amount of", input$features, "was grown in", top1$country_code), 
      icon = icon("leaf"), 
      color = "green"  
    )
  })
  
  ##this plot displays the yearly crop yield of the selected crop in the 5 best regions on a bar chart
  output$best_regions <- renderPlot({
    req(full_df)
    
    ##based on user input, and k means clustering, after standardization the function calculates
    ##the smallest eucledian distance to determine the most suitable crop
    crop <- selected_crop()
    
    ##function to aggregate data and group by regions
    top_regions <- top_regions()
    
    ##finding 5 regions with the biggest crop yield of the selected crop
    top5 <- full_df %>%
      filter(geo %in% top_regions$geo) %>%
      group_by(geo) %>%
      summarise(total_yield = sum(!!sym(crop), na.rm = TRUE))
    
    color_palette <- colorRampPalette(c("#000000", "#333333", "#666666", "#999999", "#CCCCCC"))
    
    ##plotting it
    ggplot(top5, aes(x = geo, y = total_yield, fill = geo)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(values = color_palette(5)) +
      labs(title = paste("Crop Yield for the Top 5 Regions for", crop),
           x = "Region",
           y = "Total Yield") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ##it does the same as above, but instead of regions, it groups by countries and instead of a barchart it displays on a piechart
  output$country_dist <- renderPlot({
    ##based on user input, and k means clustering, after standardization the function calculates
    ##the smallest eucledian distance to determine the most suitable crop
    crop <- selected_crop()
    
    ##function to aggregate data and group by regions
    top_countries <- top_countries()
    
    yield_country <- full_df %>%
      filter(country_code %in% top_countries$country_code) %>%
      group_by(country_code) %>%
      summarise(total_yield = sum(!!sym(crop), na.rm = TRUE))
    
    top_countries <- top_countries %>%
      inner_join(yield_country, by = "country_code") %>%
      rename(total_yield = total_yield.y)
    
    pie_chart <- ggplot(top_countries, aes(x = "", y = total_yield, fill = country_code)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = paste("Top 5 Countries for", crop, "Yield"), 
           fill = "Country") +
      scale_fill_grey() +
      theme_minimal() +
      theme_void()
    
    pie_chart
  })
  
}


shinyApp(ui = ui, server = server)
