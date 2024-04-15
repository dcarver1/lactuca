###
# primary script for the rendering of ecogeographic charactersation of lactuca data 
# carverd@colostate.edu
# 20220810
### 

pacman::p_load("terra", "sf", "dplyr", "readr", "stringr","raster", "ggplot2",
               "googlesheets4","googledrive", "tmap", "geodata", rnaturalearth, leaflet)
# authorize account
googlesheets4::gs4_auth()
# set tmap mode to interactive map
tmap_mode("view")

# source functions --------------------------------------------------------
source("src/dms_dd.R")

# read in the 2.5 arc sec data for solar radiation and wind speed 
bioNames <- read_csv("~/Documents/cwr_wildgrapes/data/geospatial_datasets/bioclim_layers/variableNames.csv")
bioVars <- readRDS("~/Documents/cwr_wildgrapes/data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")
names(bioVars) <- bioNames$`Current title`
# sub set for the layer not present at higher resolution 
bioVarsTrim <- bioVars[[20:22]]
rm(bioVars)

# 2024 data generation ----------------------------------------------------

## pull in data 
d1 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1c1VBw33SVNr7NdQMoL7wS4grjoX2VxOyCWHO_6QDs5w/edit#gid=1459564859"))
## drop existing geometry column 
d2 <- d1 |>
  dplyr::select(-geometry)


## Spatial object 
### drop the new addition 
sp1 <- d2 |>
  sf::st_as_sf(coords = c("longitude","latitude"), remove = FALSE)

## copy to compare the selection against 
sp2 <- sp1 |>
  dplyr::select("Id","species","location","latitude","longitude","altitude") |>
  terra::vect()


# set the path 
## does some automatic file detection testing with is nice 
geodata_path("downloads")
### grab elevation data outside of the loop as it is by country 
geodata::elevation_30s(country = "AR")

### try downloading tiles. 
for(i in 1:nrow(sp1)){
  val <- sp1[i,]  
  # grab lat lon
  lat <- val$latitude[1] |> unlist()
  lon <- val$longitude[1] |> unlist()
  # grab bioclim
  geodata::worldclim_tile(var = "bio",res = "0.5", lon = lon, lat = lat )

}

### view the outputs 
wc <- terra::rast("downloads/TRUE/wc2.1_tiles/tile_52_wc2.1_30s_bio.tif")
plot(wc[[1]])
elev <- terra::rast("downloads/TRUE/ARG_elv_msk.tif") |>
  terra::crop(terra::ext(wc))
plot(elev)
# slope 
slope <- terra::terrain(elev, v= "slope", unit = "degrees" , neighbors = 8) 
# aspect
aspect <- terra::terrain(elev, v= "aspect", unit = "degrees" , neighbors = 8)

# mask the plot area 
wc2 <- wc |>
  terra::crop(y = slope)

allFeatures <- c(wc2,elev, slope, aspect)

# Extract values to points  -----------------------------------------------
exVal1 <- terra::extract(x = allFeatures, y = sp2)
names(exVal1) <- c("ID",bioNames$`Current title`[1:19], "elevation", "slope", "aspect")
exVal2 <- terra::extract(x = bioVarsTrim, y = sp2)
# join and re order
exVals <- dplyr::left_join(exVal1, exVal2, by = "ID")

# bring back into the species data 
d3 <- d2 |>
  dplyr::select("ID" = "Id","species"  ,"location","latitude" ,"longitude","altitude")
d4 <- dplyr::left_join(d3, y = exVals, by = "ID")

# write_csv(d4,file = "outputs/ecogeographicDescription.csv")

# temp <- d4 |>
  # dplyr::select("species","Annual mean temperature","Mean diurnal temperature range","Isothermality" )
# temp


# generata a map ----------------------------------------------------------
countries <- rnaturalearth::ne_countries(scale = 110, type = "countries",continent = "south america")

sp1 <- sp1 |>
  dplyr::mutate(color = case_when(
    species == "Lvir" ~ "#4daf4a",
    species == "Lser" ~ "#377eb8",
    species == "Lser, Lvir" ~ "#e41a1c",
      ),
    popup = paste0("<strong>", as.character(species),"</strong>", # needs to be text
                   "<br/><strong>Record ID :</strong> ", Id,
                   "<br/><strong>altitude :</strong> ", altitude,
                  "<br/><strong> Location Description: </strong>", location)
      )

leaflet()|>
  addProviderTiles(provider = providers$OpenStreetMap,
                   group = "OSM") |>
  addProviderTiles(provider = providers$Esri.WorldImagery,
                   group = "Imagery")|>
  addLayersControl(
    position = "topleft",
    baseGroups = c("OSM", "Imagery")
  )|>
  addCircleMarkers(
    data = sp1,
    group = "records",
    color = ~color,
    popup = ~popup
  ) |>
  # single legend for the GBIF features
  addLegend(
    position = "topright",
    colors = c("#4daf4a", "#377eb8","#e41a1c"),
    labels = c("Lvir","Lser","Lser, Lvir"),
    title = "Species",
    opacity = 1,
    group = "records"
  )


### generate jitters --------------------------------------------------------
tbls <- d4[,c(2,7:31)]
tbls <- tbls[!is.nan(tbls$`Annual mean temperature`),]
varList <- names(tbls)[-1]

for(i in seq_along(varList)){
  g <- tbls %>%
    ggplot2::ggplot(aes(x = species, y =varList[i], color = species)) +
    ggplot2::geom_jitter(width = 0.20)+
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.y = ggplot2::element_text(face =  "bold.italic"))
  # save files 
  ggsave(filename = paste0("outputs/jitters/", varList[i], "_2024.png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)
  
}


# generate boxplots -------------------------------------------------------
#remove features with only one sample 
tbls$species <- as.factor(tbls$species)

t2 <- gather(tbls,key = "Variable", value = "Value", -species )

for(i in seq_along(varList)){
  print(i)
  t3 <- dplyr::filter(t2, Variable == varList[i])
  
  g <- ggplot(data = t3, aes(x = factor(species) , y = Value, color = species))+
    ggplot2::geom_boxplot()+
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab(varList[i]) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y     = ggplot2::element_text(face = "italic"),
                   legend.position = "none")
  
  ggsave(filename = paste0("outputs/boxplots/", varList[i], "_2024.png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)
}



# 2022 data generation ----------------------------------------------------


# clean input data --------------------------------------------------------
d1 <- read_csv("data/analysis/lactuca2022.csv")%>%
  dplyr::filter(!is.na(latitude))%>%
  dms_dd(colname = "latitude")%>%
  dms_dd(colname = "longitude")

names(d1) <- c("Id","species","location","latitude","longitude","altitude")
d1 <- d1 %>%
  st_as_sf(coords = c( "longitude","latitude"), crs = "4326", remove = FALSE)

# run extraction ----------------------------------------------------------
# variable names 
names <- read_csv("data/analysis/variableNames.csv")
# output file 
file <- "data/analysis/cropRaster.tif"
if(!file.exists(file)){
    # read in predictors 
    r1 <- readRDS("data/analysis/parameters.RDS")
    r2 <- terra::rast(r1)
    names(r2) <- names$`Current title`
    
    
    # # create point object 
    sp1 <- sf::st_set_crs(d1,value = crs(r2))
    
    # buffer to get a little bit of boarder room and ensure the extraction 
    sp2 <- st_buffer(x = sp1, dist = 50000) ## didn't seem to make a difference 
    # crop raster 
    r2 <- terra::crop(x = r2, y = sp2)
    # write out raster
    terra::writeRaster(r2, file)
  
    d2 <- terra::extract(r2, sp1) 
    df <- dplyr::left_join(as.data.frame(d1), d2, by = c("Id" = "ID"))
    write_csv(df, "outputs/ecoGeoData.csv")
  }else{
    r2 <- terra::rast(file)
    # create point object 
    sp1 <- sf::st_set_crs(d1,value = crs(r2))
    # gather data 
    d2 <- terra::extract(r2, sp1) 
    df <- dplyr::left_join(as.data.frame(d1), d2, by = c("Id" = "ID"))
  }


# generate jitters --------------------------------------------------------
tbls <- df[,c(1,2,8:33)]
tbls <- tbls[!is.nan(tbls$`Annual mean temperature`),]
varList <- names$`Current title`

for(i in seq_along(varList)){
  g <- tbls %>%
    ggplot2::ggplot(aes(x = species, y =varList[i])) +
    ggplot2::geom_jitter(colour = "gray", width = 0.20)+
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.y = ggplot2::element_text(face =  "bold.italic"))
  # save files 
  ggsave(filename = paste0("outputs/jitters/", varList[i], "_2024.png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)

}



# box plots 
# Boxplots across species data per variable




  
  

# generate pca 



# generate maps 

