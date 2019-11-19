
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


## Settings for clustering ####
#number of initial clusters
nclust_ini <- 15 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)
max_SD_intraclust <- 0.65 # maximum SD intracluster allowed. If some cluster has bigger, split in 2






iter_num <- 1  #this needs to be a for loop

## step1: selecting random centroids of the clusters ####
nvars <- length(pca_data_ini)

clust_centr_ini <- pca_data_ini[sample(nrow(pca_data_ini), nclust_ini), ]
clust_centr_ini
nrow(clust_centr_ini)

pca_data_ini_noCentr <- pca_data_ini[!rownames(pca_data_ini) %in% rownames(clust_centr_ini), ]
nrow(pca_data_ini)
nrow(pca_data_ini_noCentr)
head(pca_data_ini_noCentr)
pca_data_ini[rownames(pca_data_ini) == 13222818, ]
pca_data_ini_noCentr[rownames(pca_data_ini_noCentr) == 13222818, ]


## step2: calculating the closest centroid (assigning to a cluster) ####
#t0 <- Sys.time()
#pca_data_ini_noCentr$closest <- apply(pca_data_ini_noCentr, 1, function(y) rownames(clust_centr_ini)[which.min(apply(clust_centr_ini, 1, function(x) dist(rbind(y, x))))])
#Sys.time() - t0
#head(pca_data_ini_noCentr)

# parallelizing
cl <- makeCluster(3)
clusterExport(cl, c("clust_centr_ini", "nvars"))
clsts <- parRapply(cl = cl, 
                   x = pca_data_ini_noCentr[, c(1:nvars)], 
                   #MARGIN = 1, 
                   FUN = function(y) rownames(clust_centr_ini)[which.min(apply(clust_centr_ini, 1, function(x) dist(rbind(y, x))))]
                   )
stopCluster(cl)

pca_data_ini_noCentr$closest <- as.vector(clsts)
head(pca_data_ini_noCentr)



## Step3: SD intra-cluster ####
# if SD in a cluster is > 4, split it in two clusters. How? Assigning two new random centroids?? 

## Distance to closest (centroid)
cl <- makeCluster(3)
clusterExport(cl, c("clust_centr_ini", "nvars"))

t0 <- Sys.time()
clsts <- parRapply(cl = cl, 
                   x = pca_data_ini_noCentr, 
                   FUN = function(y) {
                     y1 <- clust_centr_ini[rownames(clust_centr_ini) == y[5], 1:nvars]
                     y2 <- dist(rbind(y1, y[1:nvars]))
                     return(y2)
                   }
)
Sys.time() - t0
## verification: dist(rbind(pca_data_ini_noCentr[1, 1:4], clust_centr_ini[rownames(clust_centr_ini) == 6662183, ]))

stopCluster(cl)


## Not parallel: it takes much longer
#t0 <- Sys.time()
#clsts1 <- apply(pca_data_ini_noCentr, 
#                MARGIN = 1, 
#                FUN = function(y) {
#                  y1 <- clust_centr_ini[rownames(clust_centr_ini) == y[5], 1:nvars]
#                  y2 <- dist(rbind(y1, y[1:nvars]))
#                  return(y2)
#                }
#)
#Sys.time() - t0


pca_data_ini_noCentr$dist2closest <- as.vector(clsts)
head(pca_data_ini_noCentr)


## SD intra-cluster (if bigger than a threshold, split the cluster in two)
str(pca_data_ini_noCentr)

stats_intraclustr <- as.data.frame(pca_data_ini_noCentr %>% group_by(closest) %>% summarise_at(.vars = "dist2closest", .funs = c("mean_intraclust" = mean, "SD_intraclust" = sd, "max_intraclust" = max, "min_intraclust" = min)))
stats_intraclustr
sd_intraclustr <- stats_intraclustr[, colnames(stats_intraclustr) %in% c("closest", "SD_intraclust")]
sd_intraclustr
big_clusters <- sd_intraclustr$closest[which(sd_intraclustr$SD_intraclust >= max_SD_intraclust)]


head(pca_data_ini_noCentr)
nrow(pca_data_ini_noCentr)
tail(pca_data_ini_noCentr)

clust_centr_ini[rownames(clust_centr_ini) %in% big_clusters, ]

pca_data_ini <- rbind(pca_data_ini_noCentr, )
pca_data_ini <- pca_data_ini[order(as.numeric(row.names(pca_data_ini))), ]

clust_centr_ini <- clust_centr_ini[!rownames(clust_centr_ini) %in% big_clusters, ]
head(pca_data_ini_noCentr)



#
















## Wilk's (lambda) test
library(rrcov)
head(pca_data_ini_noCentr[, colnames(pca_data_ini_noCentr) %in% c("closest", "dist2closest")])

wilks_formula <- Wilks.test(closest ~ PC1 + PC2 + PC3 + PC4, data = pca_data_ini_noCentr)

head(as.numeric(as.character(as.matrix(pca_data_ini_noCentr[, !colnames(pca_data_ini_noCentr) %in% c("dist2closest")]))))
wilks_matrix <- Wilks.test(, grouping = "closest")





