# R options
options(scipen = 999, warn = -1)

# Load libraries
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, gtools, FactoMineR, factoextra, Rtsne, psych, viridis, corrplot))
# install.packages('raster')
# install.packages('rgdal')
# install.packages("corrplot")
# library(raster)
# library(rgdal)
library(corrplot)

# # Load data
# root <- choose.dir()
# tbls <- list.files(path = root, full.names = T) %>% purrr::map(.x = ., .f = read.csv) %>% do.call(dplyr::bind_rows, .); rm(root)
# 
# # Load climate data
clim <- "//dapadfs/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2"
lyrs <- list.files(clim, pattern = "bio_", full.names = T) 
lyrs <- lyrs[-c(2,3) ]%>% gtools::mixedsort() %>% raster::stack(.) #there are some .aux.xml and .ovr files within here. Indexing to drop
# 
srad <- list.files(clim, pattern = "srad", full.names = T) 
srad <- srad[-c(2,3)] %>% gtools::mixedsort() %>% raster::stack(.)
wind <- list.files(clim, pattern = "wind", full.names = T) %>% gtools::mixedsort() %>% raster::stack(.)
vapr <- list.files(clim, pattern = "vapr", full.names = T) %>% gtools::mixedsort() %>% raster::stack(.)
# 
lyrs <- raster::stack(c(lyrs, srad, wind, vapr)); rm(srad, wind, vapr)
altitude <- raster::raster("//dapadfs/data_cluster_4/observed/gridded_products/srtm/Altitude_30s/alt")

# Add climate data to species data
#outD <- choose.dir()
outD <- "//dapadfs/workspace_cluster_9/Aichi13/KhouryCWR_aichiExtensions/Lactuca"
if(!file.exists(paste0(outD, "/Lactuca_occ_clim.csv"))){
  tbls <- read.csv(paste0(outD, "/Lactuca_newdataset_2019_2_28BothAddation.csv"))
  tbls <- dplyr::select(tbls, ID,PNM,Updated.,multiple.species.pop,species,LAT_DEC_new, LON_DEC_new,LAT_DEC,LON_DEC,SOURCE)
  #levels(tbls$species) <- gsub("Cucurbita_xscabridifolia", "Cucurbita_x_scabridifolia", levels(tbls$Taxon_final))
  tbls <- cbind(tbls, raster::extract(x = lyrs, y = tbls[,c("LON_DEC_new", "LAT_DEC_new")]))
  tbls <- cbind(tbls, data.frame(Altitude = raster::extract(x = altitude, y = tbls[,c("LON_DEC_new", "LAT_DEC_new")])))
  write.csv(tbls, paste0(outD, "/Lactuca_occ_clim.csv"), row.names = F)
} else {
  tbls <- read.csv(paste0(outD, "/Lactuca_occ_clim.csv"))
}

tbls$srad <- tbls %>% dplyr::select(srad_1:srad_12) %>% apply(., 1, median)
tbls$wind <- tbls %>% dplyr::select(wind_1:wind_12) %>% apply(., 1, median)
tbls$vapr <- tbls %>% dplyr::select(vapr_1:vapr_12) %>% apply(., 1, median)

tbls <- tbls %>% dplyr::select(species, LON_DEC_new, LAT_DEC_new,bio_1:bio_19, srad, wind, vapr, Altitude)
levels(tbls$species) <- gsub("Lactuca", "L.", levels(tbls$species))
levels(tbls$species) <- gsub("Both_serriola_virosa", "L. serriola & L. virosa", levels(tbls$species))
levels(tbls$species) <- gsub("_", " ", levels(tbls$species))
my_order <- rev(levels(tbls$species))

# Descriptive statistics
psych::describe(tbls %>% dplyr::select(bio_1:Altitude)) %>%
  write.csv(., file=paste0(outD,"/descriptive_stats.csv"), row.names = T)

# Correlation analysis
tbls %>% dplyr::select(bio_1:Altitude) %>% cor(., use = "pairwise.complete.obs", method = "spearman") %>% corrplot::corrplot()

env_names <- read.csv(paste0(outD, "/Ecogeovariable_names.csv"))

# Boxplots across species data per variable
env.labs <- as.character(env_names$boxplot)
names(env.labs) <- c(paste0("bio_", 1:19), "srad", "wind", "vapr", "Altitude")
g <- tbls %>%
  dplyr::select(species, LAT_DEC_new, LON_DEC_new, bio_1:Altitude) %>%
  tidyr::gather(., key = "Variable", value = "Value", -(species:LON_DEC_new)) %>%
  ggplot2::ggplot(aes(x = factor(species, my_order), y = Value, colour = species)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.y     = ggplot2::element_text(face = "italic"),
                 legend.position = "none") +
  ggplot2::facet_wrap(~ factor(Variable, levels = c(paste0("bio_", 1:19), "srad", "wind", "vapr", "Altitude")),
                      scales = "free_x",
                      labeller = labeller(`factor(...)` = env.labs))
ggsave(filename = paste0(outD,"/ecogeographicalOutputs/individual_boxplots3.png"), plot = g, units = "in", width = 15, height = 7)

#editing name 
# tbl2 <- tbls
# levels(tbl2$Taxon_final) <- gsub("Cucurbita", "C.", levels(tbls$Taxon_final))


# Jitter plots across species data per variable highlighted  G accessions
varList <- names(tbls)[11:ncol(tbls)]
lapply(1:length(varList), function(i){
  eval(parse(text=paste0(
    
    'g <- tbls %>%
    dplyr::select(Taxon_final, type, bio_1:Altitude) %>%
    dplyr::filter(type == "H") %>%
    ggplot2::ggplot(aes(x = Taxon_final, y =', varList[i], ')) +
    ggplot2::geom_jitter(colour = "gray", width = 0.20) +
    ggplot2::geom_jitter(data = tbls %>% filter(type == "G"), aes(x = Taxon_final, y =', varList[i], '), colour = "red", width = 0.20) +
    ggplot2::xlab("") +
    ggplot2::ylab(env_names$jitter[i]) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face =  "bold.italic")) +
    ggplot2::scale_x_discrete(limits = my_order)
    '
    
  )))
  
  ggsave(filename = paste0(outD,"/ecogeographicalOutputs/jitter_plots/", varList[i], ".png"), plot = g, units = "in", width = 7, height = 3)
  rm(g, variable)
})


# Removing NULL records
tbls$id <- 1:nrow(tbls)
df <- tbls %>% dplyr::select(id, LON_DEC_new, LAT_DEC_new, bio_1:Altitude) %>% tidyr::drop_na()
#df_tz <- tbls %>% dplyr::filter(country %in% c("TUN", "Tunisia")) %>% dplyr::select(id, longitude, latitude, bio_1:Altitude) %>% tidyr::drop_na()
rownames(df) <- df$id; df$id <- NULL
#rownames(df_tz) <- df_tz$id; df_tz$id <- NULL


### 2024 testing
df <- d4

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
  ggsave(filename = paste0(outD, "/pca/eigen_vals.png"), plot = ., units = "in", width = 10, height = 8)

# Control variable colors using their contributions
fviz_pca_var(pca1, col.var = "cos2", # "contrib"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) %>%
  ggsave(filename = paste0(outD, "/pca/cos2.png"), plot = ., units = "in", width = 10, height = 8)

# Contributions of variables to PC1
fviz_contrib(pca1, choice = "var", axes = 1, top = 20) %>%
  ggsave(filename = "../_processed/pca_climate_coords_altitude/contribution_axis1.png", plot = ., units = "in", width = 10, height = 8)
# Contributions of variables to PC2
fviz_contrib(pca1, choice = "var", axes = 2, top = 20) %>%
  ggsave(filename = paste0(outD, "/pca/contribution_axis2.png"), plot = ., units = "in", width = 10, height = 8)


# Biplot of individuals and variables
# fviz_pca_biplot(pca1, repel = TRUE)

fviz_pca_biplot(pca1,
                label = "var",
                habillage = tbls$speies[intersect(rownames(df), tbls$id)],
                addEllipses = TRUE,
                ellipse.level = 0.95) %>%
  ggsave(filename = paste0(outD, "/pca/biplot_species.png"), plot = ., units = "in", width = 10, height = 8)

fviz_pca_biplot(pca1,
                label = "var",
                habillage = tbls$type[intersect(rownames(df), tbls$id)],
                addEllipses = TRUE,
                ellipse.level = 0.95) %>%
  ggsave(filename = paste0(outD, "/pca/biplot_type.png"), plot = ., units = "in", width = 10, height = 8)

fviz_pca_ind(pca1,
             label = "none", # hide individual labels
             habillage = tbls$taxon_final[intersect(rownames(df), tbls$id)], # color by groups
             palette = "Paired",
             addEllipses = TRUE # Concentration ellipses
) %>%
  ggsave(filename = paste0(outD, "/pca/species.png"), plot = ., units = "in", width = 10, height = 8)


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
ggsave(filename = paste0(outD, "/pca/interpretation_cluster3.png"), plot = g, units = "in", width = 8, height = 10)

# Dendrogram
fviz_dend(clust_pca, show_labels = FALSE)
# Individuals facor map
g <- fviz_cluster(clust_pca, geom = "point", main = "Factor map") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal()
ggsave(filename = paste0(outD, "/pca/clustering.png"), plot = g, units = "in", width = 10, height = 8)

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

# tbls2 <- tbls %>% dplyr::select(taxon_final, bio_1:bio_19)
# tbls2 <- tbls2[complete.cases(tbls2),]
# tbls2 <- tbls2[!duplicated(tbls2),]
# tbls2 <- tbls2[!duplicated(tbls2[,-1]),]
# 
# tsne <- Rtsne(tbls2[,-1], dims = 2, perplexity = 200, verbose = T, max_iter = 500)
# tsne$Y %>%
#   as.data.frame %>%
#   dplyr::mutate(Species = tbls2$taxon_final) %>%
#   ggplot(aes(V1, V2, colour = Species)) +
#   geom_point()

# ------------------------------------------------------------------------------------------------------------------ #
# PCA Tunisia
# ------------------------------------------------------------------------------------------------------------------ #

# PCA using all variables
pca1 <- df_tz %>% dplyr::select(longitude:Altitude) %>% FactoMineR::PCA(scale.unit = T, graph = F)
pca1$ind$coord %>%
  data.frame %>%
  mutate(Species = tbls$taxon_final[intersect(rownames(df_tz), tbls$id)]) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, colour = Species)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# Eigen values
fviz_screeplot(pca1, addlabels = TRUE, ylim = c(0, 50)) %>%
  ggsave(filename = "../_processed/pca_tunisia/eigen_vals.png", plot = ., units = "in", width = 10, height = 8)

# Control variable colors using their contributions
fviz_pca_var(pca1, col.var = "cos2", # "contrib"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) %>%
  ggsave(filename = "../_processed/pca_tunisia/cos2.png", plot = ., units = "in", width = 10, height = 8)

# Contributions of variables to PC1
fviz_contrib(pca1, choice = "var", axes = 1, top = 20) %>%
  ggsave(filename = "../_processed/pca_tunisia/contribution_axis1.png", plot = ., units = "in", width = 10, height = 8)
# Contributions of variables to PC2
fviz_contrib(pca1, choice = "var", axes = 2, top = 20) %>%
  ggsave(filename = "../_processed/pca_tunisia/contribution_axis2.png", plot = ., units = "in", width = 10, height = 8)


# Biplot of individuals and variables
# fviz_pca_biplot(pca1, repel = TRUE)

fviz_pca_biplot(pca1,
                label = "var",
                habillage = tbls$taxon_final[intersect(rownames(df_tz), tbls$id)],
                addEllipses = TRUE,
                ellipse.level = 0.95) %>%
  ggsave(filename = "../_processed/pca_tunisia/biplot_species.png", plot = ., units = "in", width = 10, height = 8)

fviz_pca_biplot(pca1,
                label = "var",
                habillage = tbls$type[intersect(rownames(df_tz), tbls$id)],
                addEllipses = TRUE,
                ellipse.level = 0.95) %>%
  ggsave(filename = "../_processed/pca_tunisia/biplot_type.png", plot = ., units = "in", width = 10, height = 8)

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
ggsave(filename = "../_processed/pca_tunisia/interpretation_cluster3.png", plot = g, units = "in", width = 8, height = 10)

# Individuals factor map
g <- fviz_cluster(clust_pca, geom = "point", main = "Factor map") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal()
ggsave(filename = "../_processed/pca_tunisia/clustering.png", plot = g, units = "in", width = 10, height = 8)

cross_tab <- as.data.frame(table(tbls$taxon_final[intersect(rownames(df_tz), tbls$id)], clust_pca$data.clust$clust))
cross_tab <- as.data.frame(table(tbls$taxon_final[intersect(rownames(df_tz), tbls$id)], clust_pca$data.clust$clust)/rowSums(table(tbls$taxon_final[intersect(rownames(df_tz), tbls$id)], clust_pca$data.clust$clust)))

colnames(cross_tab) <- c("Species", "Cluster", "Freq")

g <- cross_tab %>% ggplot(aes(x = factor(Cluster), y = Species, fill = Freq)) +
  geom_tile(color = "white", size = 0.3) +
  coord_equal() +
  scale_fill_viridis(name = "Frequency", option = "C") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text =  element_text(size = 15))
ggsave(filename = "../_processed/pca_tunisia/species_by_cluster.png", plot = g, units = "in", width = 8, height = 10)

