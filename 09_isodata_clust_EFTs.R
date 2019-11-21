
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
nclust_ini <- 50 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)

max_num_clustrs <- 500


max_SD_intraclust <- 0.65 # maximum SD intracluster allowed. If some cluster has bigger, split in 2
                          # Make it extremelly small if you want it not to be limitant
max_SD_intraclust <- 0.5 
max_SD_intraclust <- 0.3 
              

min_dist_interclust <- 0.4 # If the distance is smaller, then both clusters are merged
                           # Make it extremelly high if you want it not to be limitant
min_dist_interclust <- 0.5
min_dist_interclust <- 0.8


max_iter <- 5
max_iter <- 3
max_iter <- 100








## step1: selecting random centroids of the clusters ####
iter_num <- 0  #this needs to be a "for loop" or a "repeat"

nvars <- length(pca_data_ini)

clust_centr_ini <- pca_data_ini[sample(nrow(pca_data_ini), nclust_ini), ]
clust_centr_ini
nrow(clust_centr_ini)


repeat{
  iter_num <- iter_num + 1
  print(paste0("iteration # ", iter_num, " (max allowed = ", max_iter, ")"))
  print(paste0("Starting at ", Sys.time()))
  
  
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
  
  stuff2save <- c("pca_data_ini_noCentr", "clust_centr_ini")
  save(list = stuff2save, file = paste0(path2tempResults, "/results_Step9_iter", iter_num, ".RData"))
  
  print(paste0("End of calculating centroids at ", Sys.time()))
  
  
  ## Step3: SD intra-cluster ####
  # if SD in a cluster is > 4, split it in two clusters. How? Assigning two new random centroids?? 
  
  ## Distance to closest (centroid)
  cl <- makeCluster(3)
  clusterExport(cl, c("clust_centr_ini", "nvars"))
  
  clsts <- parRapply(cl = cl, 
                     x = pca_data_ini_noCentr, 
                     FUN = function(y) {
                       y1 <- clust_centr_ini[rownames(clust_centr_ini) == y[5], 1:nvars]
                       y2 <- dist(rbind(y1, y[1:nvars]))
                       return(y2)
                     }
  )
  ## verification: dist(rbind(pca_data_ini_noCentr[1, 1:4], clust_centr_ini[rownames(clust_centr_ini) == 6662183, ]))
  stopCluster(cl)
  print(paste0("End of calculating distance to centroids at ", Sys.time()))
  
  
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
  
  stuff2save <- c("clust_centr_ini", "pca_data_ini_noCentr", "iter_num")
  save(list = stuff2save, file = paste0(path2tempResults, "/results_Step9_iter", iter_num, ".RData"))
  
  
  
  ## Dist Inter-Clusters (centroids; if smaller than a threshold, merge clusters)
  dist_interclust <- dist(clust_centr_ini)
  summary(dist_interclust)
  quantile(dist_interclust, seq(0, 1, 0.05))
  
  percentile <- ecdf(dist_interclust)
  if((round(percentile(min_dist_interclust), 2) * 100) > 5) stop("Your minimum distance intercluster is bigger than 5th percentile.\nThis probably means a lot of cluster mergings. Are you happy with that???") 
  
  small_centroids <- dist_interclust[dist_interclust < min_dist_interclust] 
  dist_interclust <- as.data.frame(as.matrix(dist_interclust))
  
  clust_mergd <- c()
  if(sum(small_centroids) > 0){
    for (sml_ctrds in small_centroids){
      clusts2merge <- rownames(which(dist_interclust == sml_ctrds, arr.ind = TRUE))
      #clusts2merge <- (which(dist_interclust == sml_ctrds, arr.ind = TRUE))
      #print(sml_ctrds)
      clust_mergd <- c(clust_mergd, clusts2merge)
      centroids_back <- clust_centr_ini[rownames(clust_centr_ini) %in% clusts2merge, ]
      centroids_back$closest <- 0
      centroids_back$dist2closest <- 0
      centroids_back
      pca_data_ini_noCentr <- rbind(pca_data_ini_noCentr, centroids_back)
      pca_data_ini_noCentr <- pca_data_ini_noCentr[order(as.numeric(rownames(pca_data_ini_noCentr))), ]
      clust_centr_ini <- clust_centr_ini[!rownames(clust_centr_ini) %in% clusts2merge, ]
      
      sml_cluster2merge <- pca_data_ini_noCentr[pca_data_ini_noCentr$closest %in% clusts2merge, ]
      sml_cluster2merge <- sml_cluster2merge[sample(nrow(sml_cluster2merge), 1), ][, 1:nvars]
      clust_centr_ini <- rbind(clust_centr_ini, sml_cluster2merge)
    }
  }
  clust_mergd
  nrow(clust_centr_ini)
  
  print(paste0("End of distance inter-clusters at ", Sys.time()))
  
  #
  
  
  
  ## SD intra-cluster (if bigger than a threshold, split the cluster in two)
  #str(pca_data_ini_noCentr)
  
  stats_intraclustr <- as.data.frame(pca_data_ini_noCentr[pca_data_ini_noCentr$closest != 0, ] %>% group_by(closest) %>% summarise_at(.vars = "dist2closest", .funs = c("mean_intraclust" = mean, "SD_intraclust" = sd, "max_intraclust" = max, "min_intraclust" = min)))
  stats_intraclustr
  sd_intraclustr <- stats_intraclustr[, colnames(stats_intraclustr) %in% c("closest", "SD_intraclust")]
  sd_intraclustr
  big_clusters <- sd_intraclustr$closest[which(sd_intraclustr$SD_intraclust >= max_SD_intraclust)]
  big_clusters
  
  head(pca_data_ini_noCentr)
  nrow(pca_data_ini_noCentr)
  tail(pca_data_ini_noCentr)
  
  if(length(big_clusters) > 0){
    centroids_back <- clust_centr_ini[rownames(clust_centr_ini) %in% big_clusters, ]
    centroids_back$closest <- 0
    centroids_back$dist2closest <- 0
    centroids_back
    pca_data_ini_noCentr <- rbind(pca_data_ini_noCentr, centroids_back)
    pca_data_ini_noCentr <- pca_data_ini_noCentr[order(as.numeric(rownames(pca_data_ini_noCentr))), ]
    
    clust_centr_ini <- clust_centr_ini[!rownames(clust_centr_ini) %in% big_clusters, ]
    
    for (bg_clstr in big_clusters) {
      bg_cluster2split <- pca_data_ini_noCentr[pca_data_ini_noCentr$closest %in% bg_clstr, ]
      bg_cluster2split <- bg_cluster2split[sample(nrow(bg_cluster2split), 2), ][, 1:nvars]
      clust_centr_ini <- rbind(clust_centr_ini, bg_cluster2split)
    }
  }
  
  stuff2save <- c("clust_centr_ini", "pca_data_ini_noCentr", "iter_num", "clust_mergd", "big_clusters")
  save(list = stuff2save, file = paste0(path2tempResults, "/results_Step9_iter", iter_num, ".RData"))
  #load(file = paste0(path2tempResults, "/results_Step9_iter3.RData"), verbose = TRUE)
  print(paste0("End of SD inter-centroids at ", Sys.time()))
  
  
  ## conditions to stop the ISODATA clustering
  if(iter_num == max_iter){
    print("maximum number of iterations reached")
    break  
  } 
  if(is.null(clust_mergd) & length(big_clusters) == 0){
    print("all clusters are within defined parameters (max_SD_intraclust and min_dist_interclust)")
    break  
  }
  if(nrow(clust_centr_ini) >= max_num_clustrs){
    print("maximum number of clusters reached")
    break
  }
  
  print(paste0("End of iteration #", iter_num, " at ", Sys.time()))
}



# some checkings
iters <- c("iter1", "iter2", "iter3")

for (i in iters) {
  print(i)
  load(paste0(path2tempResults, "/results_Step9_", i, ".RData"), verbose = FALSE)
  print(paste0("clust_mergd: ", paste0(clust_mergd, collapse = ", ")))
  print(paste0("big_clusters to split: ", paste0(big_clusters, collapse = ", ")))
  assign(paste0("pca_data_ini_noCentr_", i), pca_data_ini_noCentr)
  frmla <- as.formula(paste0("closest ~ ", paste0(colnames(pca_data_ini_noCentr)[1:nvars], collapse = " + ")))
  wilks_formula <- Wilks.test(frmla, data = pca_data_ini_noCentr)
  print(paste0("Wilks' Lambda for... ", i))
  print(wilks_formula)
}




## Wilk's (lambda) test
head(pca_data_ini_noCentr)
length(unique(pca_data_ini_noCentr$closest))
#wilks_formula <- Wilks.test(closest ~ PC1 + PC2 + PC3 + PC4, data = pca_data_ini_noCentr)
frmla <- as.formula(paste0("closest ~ ", paste0(colnames(pca_data_ini_noCentr)[1:nvars], collapse = " + ")))
wilks_formula <- Wilks.test(frmla, data = pca_data_ini_noCentr)

wilks_formula
wilks_formula$statistic
wilks_formula$parameter
wilks_formula$p.value











