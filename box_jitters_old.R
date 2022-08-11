# R options
options(scipen = 999, warn = -1)

# Load libraries
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, gtools, FactoMineR, factoextra, Rtsne, psych, viridis))
# install.packages('raster')
# install.packages('rgdal')
# install.packages("corrplot")
library(raster)
library(rgdal)
library(corrplot)
library(tidyverse)
library(dplyr)
library(ggplot2)



# set base directories 
base_dir <- "D:/usda/sorghum"
data_dir <- paste0(base_dir, "/dataFromHarry")
outD <- "D:/usda/sorghum/ecogeographicCharacterization"

# read in data 
data <- read.csv(paste0(data_dir, "/All_occurrences.csv" ))

# load raster data 
rasters <- readRDS("D:/cwrNA/parameters/bioLayer_2.5/climate_vx.RDS")
rasters <- rasters$as.RasterStack()


# load species occurence data 
data <- read.csv(paste0(data_dir, "/All_occurrences.csv" ))

# remove all values with no lat long 
d1 <- data[complete.cases(data[2:3]),]
# create a spatial points object 
latLong <- d1[2:3]
spPoint <<- SpatialPointsDataFrame(latLong,
                                   data = d1)

# reduce size to speed up processing 
rasterStack <- rasters %>% 
  raster::crop(extent(spPoint))

values <- as.data.frame(raster::extract(x = rasterStack,y = spPoint))
#add species data back to extracted values 
values1 <- cbind(d1,values)

### get names of the 
tbls <- values1[,c(1:4,6:ncol(values1))]
levels(tbls$Species) <- gsub("Sorghum", "S.", levels(tbls$Species))
my_order <- rev(levels(tbls$Species))

env_names <- read.csv(paste0(outD, "/Ecogeovariable_names.csv"))

# Jitter plots across species data per variable highlighted  G accessions
varList <- names(tbls)[5:ncol(tbls)]
lapply(1:length(varList), function(i){
  eval(parse(text=paste0(
    
    'g <- tbls %>%
    dplyr::select(Species, Type, layer.1:layer.26) %>%
    dplyr::filter(Type == "H") %>%
    ggplot2::ggplot(aes(x = Species, y =', varList[i], ')) +
    ggplot2::geom_jitter(colour = "gray", width = 0.20) +
    ggplot2::geom_jitter(data = tbls %>% filter(Type == "G"), aes(x = Species, y =', varList[i], '), colour = "red", width = 0.20) +
    ggplot2::xlab("") +
    ggplot2::ylab(env_names$jitter[i]) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face =  "bold.italic")) +
    ggplot2::scale_x_discrete(limits = my_order)
    '
  )))
  
  ggsave(filename = paste0(outD,"/jitterPlots/", env_names$jitter[i], ".png"), plot = g, units = "in", width = 7, height = 3)
  rm(g, variable)
})




# set names based on variables 
colnames(tbls) <-  c("Species","Longitude", "Latitude","Type",
                     "Annual mean temperature (�C)",
                     "Mean diurnal temperature range (�C)",
                     "Isothermality",
                     "Temperature seasonality (std dev) (�C)",
                     "Maximum temperature of warmest month (�C)",
                     "Minimum temperature of coldest month (�C)",
                     "Temperature annual range (�C)",
                     "Mean temperature of wettest quarter (�C)",
                     "Mean temperature of driest quarter (�C)",
                     "Mean temperature of warmest quarter (�C)",
                     "Mean temperature of coldest quarter (�C)",
                     "Annual precipitation (mm)",
                     "Precipitation of wettest month (mm)",
                     "Precipitation of driest month (mm)",
                     "Precipitation seasonality (percent)",
                     "Precipitation of wettest quarter (mm)",
                     "Precipitation of driest quarter (mm)",
                     "Precipitation of warmest quarter (mm)",
                     "Precipitation of coldest quarter (mm)",
                     "Solar radiation (kJ m-2 day-1)",
                     "Water vapor pressure (kPa)",
                     "Wind speed (m s-1)",
                     "Altitude (m)",
                     "Aspect (North-South)",
                     "Aspect (East-West)",
                     "Slope (�)"
)


ecoValues <- tbls[,c(5:ncol(tbls))]


# Descriptive statistics
psych::describe(ecoValues) %>%
  write.csv(., file=paste0(outD,"/descriptive_stats.csv"), row.names = T)

# Correlation analysis
ecoValues %>% cor(., use = "pairwise.complete.obs", method = "spearman") %>% corrplot::corrplot()


# Boxplots across species data per variable
g <- tbls %>%
  tidyr::gather(., key = "Variable", value = "Value", -(Species:Type))%>%
  ggplot2::ggplot(aes(x = factor(Species, my_order), y = Value, colour = Species)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.y     = ggplot2::element_text(face = "italic"),
                 legend.position = "none") +
  ggplot2::facet_wrap(~ factor(Variable, levels = names(tbls)),
                      scales = "free_x",
                      labeller = labeller(`factor(...)` = names(tbls)))

ggsave(filename = paste0(outD,"/individual_boxplots3.png"), plot = g, units = "in", width = 18, height = 14)



