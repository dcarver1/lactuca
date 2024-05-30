###
# primary script for the rendering of ecogeographic charactersation of lactuca data 
# carverd@colostate.edu
# 20220810
### 

pacman::p_load("terra", "sf", "dplyr", "readr", "stringr","raster", "ggplot2",
               "googlesheets4","googledrive", "tmap", "geodata", rnaturalearth, leaflet,
               tidyr)


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
# sub set for the layer not present at higher resolution 
bioVarsTrim <- bioVars[[20:22]]
rm(bioVars)

# 2024 data generation ----------------------------------------------------
# original Data
d1 <- read.csv("originalData/originalLactucaData.csv")
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
names(exVal1) <- c("ID",bioNames$`Current title`[1:19], "elevation", "slope", "aspect")
exVal2 <- terra::extract(x = bioVarsTrim, y = sp2)
# join and re order
exVals <- dplyr::left_join(exVal1, exVal2, by = "ID")

# bring back into the species data 
d3 <- d2 |>
  dplyr::select("ID" = "Id","species"  ,"location","latitude" ,"longitude","altitude")
d4 <- dplyr::left_join(d3, y = exVals, by = "ID")

write_csv(d4,file = "outputs/ecogeographicDescription.csv")

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

#pca process 
### 2024 testing
df <- d4[2:nrow(d4),]

# PCA using all variables
pca1 <- df %>%
  dplyr::select("Annual mean temperature":wind) %>% 
  FactoMineR::PCA(scale.unit = T, graph = F)

pca1$ind$coord %>%
  data.frame %>%
  mutate(Species = df$species)|>  # tbls$species[intersect(rownames(df), tbls$id)]) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, colour = Species)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

outD <- "outputs/pca"

# Eigen values
fviz_screeplot(pca1, addlabels = TRUE, ylim = c(0, 50)) %>%
  ggsave(filename = paste0(outD, "/eigen_vals.png"), plot = ., units = "in", width = 10, height = 8)

# Control variable colors using their contributions
fviz_pca_var(pca1, col.var = "cos2", # "contrib"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) %>%
  ggsave(filename = paste0(outD, "/cos2.png"), plot = ., units = "in", width = 10, height = 8)

# Contributions of variables to PC1
fviz_contrib(pca1, choice = "var", axes = 1, top = 20) %>%
  ggsave(filename = paste0(outD, "/contribution_axis1.png"), plot = ., units = "in", width = 10, height = 8)
# Contributions of variables to PC2
fviz_contrib(pca1, choice = "var", axes = 2, top = 20) %>%
  ggsave(filename = paste0(outD, "/contribution_axis2.png"), plot = ., units = "in", width = 10, height = 8)


# Biplot of individuals and variables
fviz_pca_biplot(pca1, repel = TRUE)

species <-unique(df$species)


res.pca <- prcomp(df[,7:31],scale. = TRUE)

fviz_pca_biplot(res.pca,
                label = "var",
                habillage = df$species, # tbls$speies[intersect(rownames(df), tbls$id)],
                addEllipses = TRUE,
                ellipse.level = 0.95,
                repel = TRUE
                ) %>%
  ggsave(filename = paste0(outD, "/biplot_species.png"), 
         plot = ., units = "in", width = 10, height = 8)

# we don't have a type 
# fviz_pca_biplot(pca1,
#                 label = "var",
#                 habillage = tbls$type[intersect(rownames(df), tbls$id)],
#                 addEllipses = TRUE,
#                 ellipse.level = 0.95) %>%
#   ggsave(filename = paste0(outD, "/biplot_type.png"), plot = ., units = "in", width = 10, height = 8)

fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage =df$species,  # tbls$taxon_final[intersect(rownames(df), tbls$id)], # color by groups
             palette = "Paired",
             addEllipses = TRUE # Concentration ellipses
) %>%
  ggsave(filename = paste0(outD, "/individualpca.png"), plot = ., units = "in", width = 10, height = 8)


clust_pca <- FactoMineR::HCPC(res = pca1, nb.clust = -1, graph = F)

# Cluster interpretation
cl1 <- round(clust_pca$desc.var$quanti$`1`, 2) %>% data.frame
cl1$Variable <- rownames(cl1)
cl1$Cluster <- 1
cl2 <- round(clust_pca$desc.var$quanti$`2`, 2) %>% data.frame
cl2$Variable <- rownames(cl2)
cl2$Cluster <- 2
cl3 <- round(clust_pca$desc.var$quanti$`3`, 2) %>% data.frame
cl3$Variable <- rownames(cl3)
cl3$Cluster <- 3
cls <- rbind(cl1, cl2, cl3); rm(cl1, cl2, cl3)
rownames(cls) <- 1:nrow(cls)

g <- cls %>% filter(Cluster == 3) %>% ggplot(aes(x = reorder(Variable, v.test, FUN = median), y = v.test)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  xlab("Variable") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text =  element_text(size = 15))
ggsave(filename = paste0(outD, "/interpretation_cluster3.png"), plot = g, units = "in", width = 8, height = 10)

# Dendrogram
fviz_dend(clust_pca, show_labels = FALSE)
# Individuals facor map
g <- fviz_cluster(clust_pca, geom = "point", main = "Factor map") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal()
ggsave(filename = paste0(outD, "/clustering.png"), plot = g, units = "in", width = 10, height = 8)

cross_tab <- as.data.frame(table(tbls$taxon_final[intersect(rownames(df), tbls$id)], clust_pca$data.clust$clust))
cross_tab <- as.data.frame(table(tbls$taxon_final[intersect(rownames(df), tbls$id)], clust_pca$data.clust$clust)/rowSums(table(tbls$taxon_final[intersect(rownames(df), tbls$id)], clust_pca$data.clust$clust)))

colnames(cross_tab) <- c("Species", "Cluster", "Freq")

g <- cross_tab %>% ggplot(aes(x = factor(Cluster), y = Species, fill = Freq)) +
  geom_tile(color = "white", size = 0.3) +
  coord_equal() +
  scale_fill_viridis(name = "Frequency", option = "C") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text =  element_text(size = 15))
ggsave(filename = paste0(outD, "/pca/species_by_cluster.png"), plot = g, units = "in", width = 8, height = 10)

clust_raw <- clValid::clValid(obj        = df %>% dplyr::select(bio_1:vapr) %>% scale(),
                              nClust     = 2:10,
                              clMethods  = c("hierarchical","kmeans","pam"), # "hierarchical"
                              validation = c("internal", "stability")) # "internal"
clValid::summary(clust_raw)
cutree(tree = clust_raw@clusterObjs$hierarchical, k = 2) %>% length
clust_raw@clusterObjs$pam$`10`$clustering %>% length

hc.cut <- hcut(df %>% dplyr::select(bio_1:vapr) %>% scale(center = T, scale = T), k = 2, hc_method = "complete")

fviz_silhouette(clust_raw@clusterObjs$pam$`10`, palette = "jco", ggtheme = theme_classic())
fviz_silhouette(hc.cut, palette = "jco", ggtheme = theme_classic())


fviz_nbclust(df %>% dplyr::select(bio_1:vapr) %>% scale(), kmeans, method = "gap_stat")


table(cutree(tree = clust_raw@clusterObjs$hierarchical, k = 2),
      clust_raw@clusterObjs$pam$`10`$clustering)


clust_tsn <- Rtsne(df %>% dplyr::distinct() %>% scale(center = T, scale = T), dims = 2, perplexity = 50)
plot(clust_tsn$Y, pch = 20)



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

