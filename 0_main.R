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

for(i in seq_along(varList)){
  g <- tbls %>%
    ggplot2::ggplot(aes(x = species, y =varList[i])) +
    ggplot2::geom_jitter(colour = "gray", width = 0.20) +
    ggplot2::geom_boxplot()+
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.y = ggplot2::element_text(face =  "bold.italic"))
  # save files 
  ggsave(filename = paste0("outputs/jitters/", varList[i], ".png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)

}



# box plots 
# Boxplots across species data per variable


#remove features with only one sample 
tbls <- tbls[tbls$species != "Lvir x Lser?, Lvir", ]
tbls$species <- as.factor(tbls$species)

t2 <- gather(tbls,key = "Variable", value = "Value", -Id, -species )

for(i in seq_along(varList)){
  print(i)
  t3 <- dplyr::filter(t2, Variable == varList[i])
  
  g <- ggplot(data = t3, aes(x = factor(species) , y = Value), colour =  factor(species))+
    ggplot2::geom_boxplot()+
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab(varList[i]) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y     = ggplot2::element_text(face = "italic"),
                   legend.position = "none")
  
  ggsave(filename = paste0("outputs/boxplots/", varList[i], ".png"), plot = g, units = "in", width = 7, height = 3)
  rm(g)
}

  
  

# generate pca 



# generate maps 

