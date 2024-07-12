###
# primary script for the rendering of ecogeographic charactersation of lactuca data 
# carverd@colostate.edu
# 20220810
### 

pacman::p_load("terra", "sf", "dplyr", "readr", "stringr","raster", "ggplot2",
               "googlesheets4","googledrive", "tmap", "geodata", "rnaturalearth", "leaflet",
               "tidyr","FactoMineR","factoextra")


# source functions --------------------------------------------------------
source("src/dms_dd.R")

# authorize account
googlesheets4::gs4_auth(email = "carver.dan1@gmail.com")
# set tmap mode to interactive map
tmap_mode("view")



# read in the 2.5 arc sec data for solar radiation and wind speed 
bioNames <- read_csv("~/Documents/cwr_wildgrapes/data/geospatial_datasets/bioclim_layers/variableNames.csv")
bioVars <- readRDS("~/Documents/cwr_wildgrapes/data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")
names(bioVars) <- bioNames$`Current title`
names(bioVars) <- bioNames$full_title
# sub set for the layer not present at higher resolution 
bioVarsTrim <- bioVars[[20:22]]
rm(bioVars)

# 2024 data generation ----------------------------------------------------
# original Data
d1 <- read.csv("originalData/originalLactucaData.csv")
d1$Lactuca.species.at.coll..site[1] <- "Lvir"
# convert the decimal degree 
d2 <- d1 |>
  dplyr::filter(!is.na(latitude))|>
  dms_dd(colname = "latitude")|>
  dms_dd(colname = "longitude") |>
  dplyr::select(
    Id = "Coll..site.for.map",
    species = "Lactuca.species.at.coll..site",
    location = "place..location",
    "latitude",
    "longitude",
    altitude = "altitude..m.a.s.l.."
  )

# export the update dataset 
write.csv(d2, file = "outputs/dataInDD.csv")

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
names(exVal1) <- c("Id",bioNames$shortName[1:19], "elevation", "slope", "aspect")
# exVal2 <- terra::extract(x = bioVarsTrim, y = sp2)
# join and re order
exVals <- dplyr::left_join(as.data.frame(sp1), exVal1, by = "Id")

# subset for only require data for box plots 
exVals2 <- exVals |>
  dplyr::select( 
    "Id",
    "species",
    "location",
    "latitude",
    "longitude",
    "Annual mean temperature (\u00B0C)" = "bio_01",                         
    "Mean diurnal temperature range (\u00B0C)" = "bio_02", 
    "Isothermality" = "bio_03",
    "Temperature seasonality (standard deviation) (\u00B0C)" = "bio_04"   ,
    "Maximum temperature of warmest month (\u00B0C)" = "bio_05",
    "Minimum temperature of coldest month (\u00B0C)" = "bio_06",
    "Temperature annual range (\u00B0C)" = "bio_07",          
    "Mean temperature of wettest quarter (\u00B0C)" = "bio_08",
    "Mean temperature of driest quarter (\u00B0C)"  = "bio_09",
    "Mean temperature of warmest quarter (\u00B0C)" =  "bio_10",
    "Mean temperature of coldest quarter (\u00B0C)" = "bio_11",
    "Annual precipitation (mm)" = "bio_12",
    "Precipitation of wettest month (mm)"= "bio_13",
    "Precipitation of driest month (mm)" = "bio_14",
    "Precipitation seasonality (coefficient of variation) (%)" = "bio_15",
    "Precipitation of wettest quarter (mm)" = "bio_16",
    "Precipitation of driest quarter (mm)"  ="bio_17",
    "Precipitation of warmest quarter (mm)" = "bio_18",
    "Precipitation of coldest quarter (mm)" = "bio_19",
    "elevation",
    "slope",
    "aspect" )

# the  degree C symbology causes some issues.... 
write_csv(exVals,file = "outputs/ecogeographicDescription.csv")




# generata a map ----------------------------------------------------------
countries <- rnaturalearth::ne_countries(scale = 110, type = "countries",continent = "south america") |> sf::st_as_sf()

## export data for map 
sf::st_write(obj = sp1, "outputs/dataForMap/pointObject.gpkg")
sf::st_write(obj = countries, "outputs/dataForMap/countries.gpkg")

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
    fillOpacity = 0.8,
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
  ggsave(filename = paste0("outputs/jitters/", varList[i], "_2024.png"), 
         plot = g, units = "in", width = 7, height = 3)
  rm(g)
  
}


# generate boxplots -------------------------------------------------------
#remove features with only one sample
tbls <- exVals


index <- 15
shortNames <- names(exVals[8:29])
fullNames <- names(exVals2)[6:27]
exportNames <- c(bioNames$`Current title`[1:19], "elevation","slope","aspect")
data <- gather(tbls, key = "Variable", value = "Value", -species )|>
  dplyr::mutate(factor = case_when(
    species == "Lvir" ~ 1,
    species == "Lser" ~ 2, 
    species == "Lser, Lvir" ~ 3
  ))

createBoxPlot <- function(index, shortNames, fullNames,exportNames, data){
  # select variable of interest 
  var <- shortNames[index]
  fullName <- fullNames[index]
  exportName <- exportNames[index]
  # filter the dataset
  t3 <- data |> dplyr::filter(Variable == var)
  # reassign data 
  t3$Value <- unlist(t3$Value)
  
  # assign a variable for the title 
  t3$tempvar <- fullName
  # labels 
  labels <-c("L. virosa", "L. serriola","L. serriola & L. virosa")
  
  t3 |> 
    group_by(species)|>
    dplyr::summarise(mean = median(Value))

  
  # generate the plot 
  p1 <- ggplot(data =t3, aes(x = factor(species, levels = c( "Lvir","Lser", "Lser, Lvir" )),
                             y = Value, 
                             color = factor(species, levels = c( "Lvir","Lser", "Lser, Lvir" )))) +
    geom_boxplot()+
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    scale_x_discrete(label = labels)+
    ggplot2::ylab("") +
    scale_color_manual(values = c( "#619CFF", "#00BA38","#F8766D" ))+
    theme_gray() + 
    theme(legend.position="none",
          aspect.ratio=1/3)+
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#d1d9d8"),
          strip.text = element_text(size=10, colour="black"))
  
  # export the image
  ggsave(filename = paste0("outputs/boxplots/", exportName, "_2024_refined.png"),
         plot = p1, 
         units = "in", width = 6, height = 2)
  
}

lapply(X = 1:22, FUN = createBoxPlot,
       shortNames = shortNames, 
       fullNames = fullNames,
       exportNames = exportNames,
       data =   data)

# # change species name 
# tbls <- tbls |>
#   dplyr::mutate(species = case_when(
#     species == "Lvir" ~ "L. virosa",
#     species == "Lser" ~ "L. serriola",
#     species == "Lser, Lvir" ~ "L. serriola & L. virosa",
# 
#   ))
# # create an ordered factor
# tbls$species <- factor(tbls$species,
#                           levels = c("L. serriola & L. virosa", "L. serriola", "L. virosa"),
#                           ordered = TRUE)

# convert to a long table 
t2 <- gather(tbls,key = "Variable", value = "Value", -species )
varList <- names(exVals[8:29])
fullNamList <- names(exVals2)[6:27]
                  

# select variable of interest 
var <- varList[1]
fullName <- fullNamList[1]
# filter the dataset
t3 <- t2 |> dplyr::filter(Variable == var)
# unlist the data values 
t3$Value <- unlist(t3$Value)
# order the species column 
t3$species2 <- factor(t3$species, levels = c("L. serriola & L. virosa", "L. serriola","L. virosa"             ))
# generate the plot 
# object labels 
labels <-c("L. virosa", "L. serriola","L. serriola & L. virosa")
# color scale 
c


ggplot(data = t3, aes(x = factor(species, 
                                 levels = c("Lvir","Lser","Lser, Lvir")), y = Value, color = species)) +
  geom_boxplot()+
  ggplot2::coord_flip()+
  ggplot2::xlab("") +
  ggplot2::ylab(fullName) +
  theme_gray()

t3$tempvar <- fullName


ggplot(data = t3, aes(x = species, y = Value, color = species)) +
  geom_boxplot()+
  ggplot2::coord_flip()+
  ggplot2::xlab("") +
  scale_x_discrete(label = labels)+
  ggplot2::ylab("") +
  scale_color_manual(values = c( "#619CFF", "#00BA38","#F8766D" ))+
  theme_gray() + 
  theme(legend.position="none",
        aspect.ratio=1/3)+
  facet_grid(. ~ tempvar) +
  theme(strip.background = element_rect(fill="#d1d9d8"),
        strip.text = element_text(size=15, colour="black"))



  ggsave(filename = paste0("outputs/boxplots/", varList[i], "_2024.png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)
}


# pca process  ------------------------------------------------------------



## read in the dataset with geographic reference data attached 
attData <- read_csv("outputs/ecogeographicDescription.csv")
## drop some reference data 
attData <- attData[,c(2,8:29)]
## change the name of the species 
attData <- attData |> 
  dplyr::mutate(fullSpecies = case_when(
    species == "Lvir" ~ "L. virosa",
    species == "Lser" ~ "L. serriola", 
    species == "Lser, Lvir" ~ "L. serriola & L. virosa"
  ))


visualizeAndSave <- function(pcaplot, title, subtitle){
  # more direct visualization of the plots 
  p1 <- ggpubr::ggpar(pcaplot,
                title = title,
                subtitle = subtitle,
                legend.title = "Taxon Group",
                legend.position = "top",
                ggtheme = theme_gray()
                )
  p1
  #save result 
  # ggexport(plotlist = p1, 
  #          filename = paste0("outputs/pca/july2024exports/",subtitle,".png"))
           # width = 800,
           # height = 800,
           # res = 300)
}

# for the PCA we want to drop the species name colum 
names(attData)
# generate the PCA 
pca <- PCA(attData[,-c(1,24)],scale.unit = TRUE, graph = TRUE)

# pca elispe with groups 
pcaEllipse <- fviz_pca_ind(pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = attData$fullSpecies, # color by groups
             palette = c(  "#00BA38","#F8766D","#619CFF"),
             addEllipses = TRUE, # Concentration ellipses
             mean.point = FALSE,
             legend.title = "Taxon Groups"
)
visualizeAndSave(pcaplot = pcaEllipse,
                 title = "Principal Component Analysis",
                 subtitle = "Ellipse")



# Add confidence ellipses
pcaEllipseConfidence <-fviz_pca_ind(pca, 
             geom.ind = "point", 
             col.ind = attData$fullSpecies, 
             palette = c(  "#00BA38","#F8766D","#619CFF"),
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             mean.point = FALSE,
             legend.title = "Groups"
)
visualizeAndSave(pcaplot = pcaEllipseConfidence,
                 title = "Principal Component Analysis",
                 subtitle = "Confidence Ellipse")
# Convex hull
convexHull <- fviz_pca_ind(pca,
                           geom.ind = "point",
             col.ind = attData$fullSpecies, # color by groups
             palette =  c(  "#00BA38","#F8766D","#619CFF"),
             addEllipses = TRUE,
             ellipse.type = "convex",
             legend.title = "Groups"
)
convexHull
visualizeAndSave(pcaplot = convexHull,
                 title = "Principal Component Analysis",
                 subtitle = "Convex Hull Ellipse")


# more direct visualization of the plots 
ggpubr::ggpar(convexHull,
              title = "Principal Component Analysis",
              subtitle = "Convex hull ellipse",
              legend.title = "Species Group",
              legend.position = "top",
              ggtheme = theme_gray()
)

# biplot with elipse 
biplot <- fviz_pca_biplot(pca, 
                col.ind = attData$fullSpecies,
                palette =  c(  "#00BA38","#F8766D","#619CFF"),
                addEllipses = TRUE, 
                label = "var",
                col.var = "black",
                repel = TRUE,
                legend.title = "Species Group") 

visualizeAndSave(pcaplot = biplot,
                 title = "Principal Component Analysis",
                 subtitle = "Biplot")
