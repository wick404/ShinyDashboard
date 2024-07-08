# R ShinyDashboard for Topsoil Features

## About The Project:

The project is a Shiny dashboard designed to analyze and visualize crop yield data in the NUTS2 EU Regions.

### Dashboard Tabs:

#### Crop Yield:
Displays yearly crop yield of selected crop based on user input, it displays the respective crop's yearly yield on a map, displays the top 5 regions for the selected crop. It also provides insights into aggregated data through interactive value boxes.

![Crop_yield](https://github.com/wick404/ShinyDashboard/assets/173910609/a1697cc8-7918-466e-9c73-3de23a8cf1e7)

#### Top Regions:
This tab helps find the most suitable regions in the EU for a selected crop.
Based on user input (selected crop type and number of clusters) kmeans clustering is being performed on topsoil features. After clustering, it finds the maximum yearly crop yield of the selected crop type and displays the regions from the same cluster on a map and also provides a table output of the most similar regions.

![Top_regions](https://github.com/wick404/ShinyDashboard/assets/173910609/5cb5f59d-47eb-4544-9bc7-4947e68ffc85)

#### Crop to Grow:
This tab identifies crops with the highest potential for a given soil based on the user's soil properties.
It takes the highest crop yield regions as centroids of each cluster (every crop type has a cluster) and calculates the Euclidean distance between the standardized user input (soil properties) and the cluster centroids. It returns the name of the crop whose cluster centroid the user input was the closest to.

![Crop_to_grow](https://github.com/wick404/ShinyDashboard/assets/173910609/86926b61-afa6-458a-bb36-fd8d35dd3b58)

#### Explore Data:
Allows users to explore the dataset directly.

## R packages used:
shiny, shinydashboard, tmap, sf, DT, ggplot2, dplyr, cluster, factoextra, flexclust, fpc, clustertend, ClusterR, clValid, clusterSim, ClusterGeo, tidyr, eurostat, Rcpp

## Prerequisites:
- Knowledge of R programming and familiarity with Shiny applications.
- Understanding of vectorization and defensive programming techniques.
- Familiarity with reactive functions.
- Knowledge of clustering algorithms like k-means.
- Knowledge of Rcpp (optional, the project can be executed without Rcpp)


