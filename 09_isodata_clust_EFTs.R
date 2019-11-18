
#### ISODATA clustering  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data from 'final PCA' (Step 08) ####

load(file = paste0(path2tempResults, "/results_Step8.RData"), verbose = TRUE)
pca_final_raster
summary(pca_final)

head(as.data.frame(pca_final_raster))
nrow(as.data.frame(pca_final_raster))
as.data.table(as.data.frame(pca_final_raster))

apply(as.data.table(as.data.frame(pca_final_raster)), 2, sd, na.rm = TRUE)
apply(as.data.table(as.data.frame(pca_final_raster)), 2, mean, na.rm = TRUE)


#initial data set to get clustered
pca_data_ini <- as.data.frame(pca_final_raster)
head(pca_data_ini)
apply(pca_data_ini, 2, function(x) sum(is.na(x)))
sum(!complete.cases(pca_data_ini))

pca_data_ini_NA <- pca_data_ini[!complete.cases(pca_data_ini), ]   #to be used at the end to fill raster stack
pca_data_ini <- pca_data_ini[complete.cases(pca_data_ini), ] 
nrow(pca_data_ini)
head(pca_data_ini)


#number of initial clusters
nclust_ini <- 15 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)




iter_num <- 1  #this needs to be a for loop

# step1: selecting random centroids of the clusters
clust_centr_ini <- pca_data_ini[sample(nrow(pca_data_ini), nclust_ini), ]
clust_centr_ini
nrow(clust_centr_ini)

pca_data_ini_noCentr <- pca_data_ini[!rownames(pca_data_ini) %in% rownames(clust_centr_ini), ]
nrow(pca_data_ini)
nrow(pca_data_ini_noCentr)
head(pca_data_ini_noCentr)
pca_data_ini[rownames(pca_data_ini) == 13222818, ]
pca_data_ini_noCentr[rownames(pca_data_ini_noCentr) == 13222818, ]


# step2: calculating the closest centroid (assigning to a cluster)
#t0 <- Sys.time()
#pca_data_ini_noCentr$closest <- apply(pca_data_ini_noCentr, 1, function(y) rownames(clust_centr_ini)[which.min(apply(clust_centr_ini, 1, function(x) dist(rbind(y, x))))])
#Sys.time() - t0
#head(pca_data_ini_noCentr)

# parallelizing
library(parallel)
cl <- makeCluster(3)
clusterExport(cl, "clust_centr_ini")
clsts <- parRapply(cl = cl, 
                   x = pca_data_ini_noCentr[, c(1:4)], 
                   #MARGIN = 1, 
                   FUN = function(y) rownames(clust_centr_ini)[which.min(apply(clust_centr_ini, 1, function(x) dist(rbind(y, x))))]
                   )
stopCluster(cl)

pca_data_ini_noCentr$closest <- as.vector(clsts)
head(pca_data_ini_noCentr)



#Step3: 






