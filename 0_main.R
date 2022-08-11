###
# primary script for the rendering of ecogeographic charactersation of lactuca data 
# carverd@colostate.edu
# 20220810
### 

pacman::p_load("terra", "sf", "dplyr", "readr", "stringr","raster")

source("src/dms_dd.R")

# clean input data --------------------------------------------------------

d1 <- read_csv("data/analysis/lactuca2022.csv")%>%
  dplyr::filter(!is.na(latitude))%>%
  dms_dd(colname = "latitude")%>%
  dms_dd(colname = "longitude")%>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "4326", remove = FALSE)

# run extraction ----------------------------------------------------------
# variable names 
names <- read_csv("data/analysis/variableNames.csv")
# read in predictors 
r1 <- readRDS("data/analysis/parameters.RDS")
r2 <- terra::rast(r1)
names(r2) <- names$`Current title`



## once there is a spatial object of the points crop this feature to the extent 
sp1 <- sf::st_set_crs(d1,value = crs(r2))

r2 <- terra::crop(x = r2, y = sp1)

d2 <- terra::extract(r2, sp1) 

df <- dplyr::left_join(as.data.frame(d1), d2, by = c("Coll. site for map" = "ID")) 

# generate box plots 

# generate pca 

# generate maps 

