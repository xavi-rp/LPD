
#### ISODATA clustering  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  source("/Users/xavi_rp/Documents/D6_LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}



cat("Clustering (Step 09)... ", "\n")



## Reading in data from 'final PCA' (Step 08) ####

load(file = paste0(path2tempResults, "/results_Step8.RData"), verbose = TRUE)
rm(pca_final_brick)
#pca_final_raster <- brick(paste0(path2tempResults, "/pca_final_raster.tif"))


#initial data set to get clustered
pca_data_ini <- as.data.frame(pca_final_raster1)
#pca_data_ini <- pca_final_raster1
rm(pca_final_raster1)


sampling2test <- "Yes"
sampling2test <- "No"
if(sampling2test == "Yes"){
  nrow(pca_data_ini)
  toSample <- sample(rownames(pca_data_ini), ceiling(nrow(pca_data_ini) / 1000))
  head(toSample)
  pca_data_ini_4later <- pca_data_ini[!rownames(pca_data_ini) %in% toSample, ]
  pca_data_ini <- pca_data_ini[rownames(pca_data_ini) %in% toSample, ]
}


#pca_data_ini$rn <- as.numeric(rownames(pca_data_ini))
#pca_data_ini[, rn := rownames(pca_data_ini)]

pca_data_ini_NA <- pca_data_ini[!complete.cases(pca_data_ini), ]   #to be used at the end to fill the raster
pca_data_ini_NA$clstr <- NA
pca_data_ini_NA$clstr <- as.integer(pca_data_ini_NA$clstr)
pca_data_ini_NA$rn <- as.numeric(rownames(pca_data_ini_NA))
pca_data_ini_NA <- pca_data_ini_NA[, names(pca_data_ini_NA) %in% c("clstr", "rn")]


pca_data_ini <- pca_data_ini[complete.cases(pca_data_ini), ] 
#pca_data_ini <- na.omit(pca_data_ini, cols = names(pca_data_ini))  


## ISODATA ####
run_isodata <- "no"
if(run_isodata == "yes"){
  ## ISODATA ####
  
  #nrow(pca_data_ini)
  #head(pca_data_ini)
  
  
  ## Settings for clustering 
  #number of initial clusters
  nclust_ini <- 50 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)
  nclust_ini <- 15 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)  # isodata_test1
  #nclust_ini <- 100 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)  # world test
  
  
  max_num_clustrs <- 500
  
  
  max_SD_intraclust <- 0.65 # maximum SD intracluster allowed. If some cluster has bigger, split in 2
  # Make it extremelly small if you want it not to be limitant
  max_SD_intraclust <- 0.5 
  max_SD_intraclust <- 0.2  # isodata_test2
  max_SD_intraclust <- 0.3  # isodata_test1
  
  
  
  min_dist_interclust <- 0.4 # If the distance is smaller, then both clusters are merged
  # Make it extremelly high if you want it not to be limitant
  min_dist_interclust <- 0.5
  min_dist_interclust <- 1    # isodata_test2
  min_dist_interclust <- 0.8  # isodata_test1
  
  
  
  max_iter <- 5
  max_iter <- 3
  max_iter <- 100
  max_iter <- 50
  
  
  
  isodata_test <- isodata_clustering(data_ini = pca_data_ini,
                                     nclust_ini = nclust_ini, 
                                     max_num_clustrs = max_num_clustrs,
                                     max_SD_intraclust = max_SD_intraclust,
                                     min_dist_interclust = min_dist_interclust,
                                     max_iter = max_iter,
                                     cors2use = cors2use,
                                     path2saveResults = path2tempResults)
  
  
  
  
  
  ## Some checkings ####
  
  isodata_test$isodta_wilks
  isodata_test$centroids_isodta
  isodata_test$iterations
  isodata_test$number_clusters
  head(isodata_test$data_ini_centroids)
  length(unique(isodata_test$data_ini_centroids$centroid))
  (isodata_test$number_clusters)
  table(isodata_test$data_ini_centroids$centroid)
  
  
  
  iters <- c("iter1", "iter2", "iter3") 
  iters <- paste0("iter", 1:iter_num) 
  #iters <- paste0("iter", 1:100) 
  nvars <- length(names(pca_data_ini))
  
  wilks_clust_df <- isodata_test$isodta_wilks
  
  
  ## Plotting results in a scatterplot ####
  # Wilks ~ Iteration
  pdf(paste0(path2saveTests, "\\clusters_scatterplot_WilksIter.pdf"), width = 7, height = 7)
  
  plt <- xyplot(WilksLambda ~ Iteration, wilks_clust_df,
                #grid = TRUE,
                scales = list(x = list(draw = FALSE)),
                #type = c("p", "r"))
                #type = c("p", "l"))
                type = c("l"),
                xlab = paste0("Iterations: 1-", nrow(wilks_clust_df)),
                ylab = "Wilks' Lambda",
                panel=function(...) {
                  panel.xyplot(...)
                  panel.abline(h = min(wilks_clust_df$WilksLambda), col = "red")
                  panel.text((nrow(wilks_clust_df) * 90 / 100), (min(wilks_clust_df$WilksLambda) * 90 / 100), paste0("Min. Wilks' Lambda (iter: ", which.min(wilks_clust_df$WilksLambda), ")"), 
                             cex = 0.5, col = "red")
                  panel.points(which.min(wilks_clust_df$WilksLambda), min(wilks_clust_df$WilksLambda), col = "red")
                }
  )
  
  plt1 <- xyplot(NumberOfClusters ~ Iteration, wilks_clust_df,
                 type = c("l"),
                 ylab = "Number of Clusters")
  
  latticeExtra::doubleYScale(plt, plt1, add.ylab2 = TRUE)
  dev.off()
  
  
  
  # Canonical Correlation ~ Iteration
  pdf(paste0(path2saveTests, "\\clusters_scatterplot_CanCorIter.pdf"), width = 7, height = 7)
  plt <- xyplot(CanonicalCorrel ~ Iteration, wilks_clust_df,
                #grid = TRUE,
                scales = list(x = list(draw = FALSE)),
                #type = c("p", "r"))
                #type = c("p", "l"))
                type = c("l"),
                xlab = paste0("Iterations: 1-", nrow(wilks_clust_df)),
                ylab = "Canonical Correlation",
                panel=function(...) {
                  panel.xyplot(...)
                  panel.abline(h = max(wilks_clust_df$CanonicalCorrel), col = "red")
                  panel.text((nrow(wilks_clust_df) * 90 / 100), (max(wilks_clust_df$CanonicalCorrel) * 98 / 100), paste0("Max. Can. Corr.(iter.: ", which.max(wilks_clust_df$CanonicalCorrel), ")"), 
                             cex = 0.5, col = "red")
                  panel.points(which.max(wilks_clust_df$CanonicalCorrel), max(wilks_clust_df$CanonicalCorrel), col = "red")
                }
  )
  
  plt1 <- xyplot(NumberOfClusters ~ Iteration, wilks_clust_df,
                 type = c("l"),
                 ylab = "Number of Clusters")
  
  latticeExtra::doubleYScale(plt, plt1, add.ylab2 = TRUE)
  dev.off()
  
  
  # Wilks' LAmbda + Canonical Correlation ~ Iteration
  pdf(paste0(path2saveTests, "\\clusters_scatterplot_WilksCanCorIter.pdf"), width = 7, height = 7)
  plt <- xyplot(WilksLambda ~ Iteration, wilks_clust_df,
                #grid = TRUE,
                scales = list(x = list(draw = FALSE)),
                #type = c("p", "r"))
                #type = c("p", "l"))
                type = c("l"),
                xlab = paste0("Iterations: 1-", nrow(wilks_clust_df)),
                ylab = "Wilks' Lambda",
                panel=function(...) {
                  panel.xyplot(...)
                  panel.abline(h = min(wilks_clust_df$WilksLambda), col = "darkblue")
                  panel.text((nrow(wilks_clust_df) * 90 / 100), (min(wilks_clust_df$WilksLambda) * 90 / 100), paste0("Min. Wilks' Lambda (iter: ", which.min(wilks_clust_df$WilksLambda), ")"), 
                             cex = 0.5, col = "darkblue")
                  panel.points(which.min(wilks_clust_df$WilksLambda), min(wilks_clust_df$WilksLambda), col = "darkblue")
                }
  )
  
  plt1 <- xyplot(CanonicalCorrel ~ Iteration, wilks_clust_df,
                 grid = FALSE,
                 scales = list(x = list(draw = FALSE)),
                 #type = c("p", "r"))
                 #type = c("p", "l"))
                 type = c("l"),
                 xlab = paste0("Iterations: 1-", nrow(wilks_clust_df)),
                 ylab = "Canonical Correlation",
                 panel=function(...) {
                   panel.xyplot(...)
                   panel.abline(h = max(wilks_clust_df$CanonicalCorrel), col = "red")
                   panel.text((nrow(wilks_clust_df) * 90 / 100), (max(wilks_clust_df$CanonicalCorrel) * 98 / 100), paste0("Max. Can. Corr.(iter.: ", which.max(wilks_clust_df$CanonicalCorrel), ")"), 
                              cex = 0.5, col = "red")
                   panel.points(which.max(wilks_clust_df$CanonicalCorrel), max(wilks_clust_df$CanonicalCorrel), col = "red")
                 }
  )
  
  
  latticeExtra::doubleYScale(plt, plt1, add.ylab2 = TRUE)
  dev.off()
  
  
  
  ## Plotting clusterings (iterations) ####
  
  #jpeg(paste0(path2saveTests, "\\clusters.jpg"), width = 1500, height = 1500, res = 300)
  #pdf(paste0(path2saveTests, "\\clusters.pdf"), width = 7, height = 7)
  #pdf(paste0(path2saveTests, "\\clusters1.pdf"), width = 18, height = 18)
  #par(mfcol = c(10, 10), mar = c(2.5, 1.5, 2, 1.5))
  pdf(paste0(path2saveTests, "\\clusters1.pdf"), width = 9, height = 9)
  par(mfcol = c(4, 3), mar = c(2.5, 1.5, 2, 1.5))
  
  for (i in iters) { 
    load(paste0(path2tempResults, "/results_isodata_", i, ".RData"), verbose = FALSE)
    
    frmla <- as.formula(paste0("closest ~ ", paste0(colnames(data_ini_noCentr)[1:nvars], collapse = " + ")))
    wilks_formula <- Wilks.test(frmla, data = data_ini_noCentr)
    
    conon_corr <- cancor(data_ini_noCentr[, 1], data_ini_noCentr[, 2:nvars])
    
    pca_final_raster1 <- pca_final_raster
    
    #nrow(data_ini) == (nrow(data_ini_noCentr[, 1:5]) + nrow(clust_centr_ini))
    #(dim(pca_final_raster)[1] * dim(pca_final_raster)[2]) == (nrow(data_ini_4later) + nrow(pca_data_ini_NA) + nrow(data_ini_noCentr[, 1:4]) + nrow(clust_centr_ini))
    
    
    all_data <- rbind(clust_centr_ini, pca_data_ini_NA)#, data_ini_4later)
    all_data$closest <- NA
    head(all_data)
    
    all_data <- rbind(data_ini_noCentr[, 1:5], all_data)
    all_data <- all_data[order(as.numeric(rownames(all_data))), ]
    
    pca_final_raster1 <- setValues(pca_final_raster1, as.matrix(as.numeric(all_data$closest)))
    names(pca_final_raster1) <- "centroid"
    pca_final_raster1
    
    pal <- colorRampPalette(c("red", "wheat2", "skyblue2", "blue", "yellow"))
    par(xpd = FALSE)
    plot(pca_final_raster1, col = pal(length(unique(all_data$closest))), legend = FALSE, main = paste0(i, ": ", length(unique(all_data$closest)), " clusters")) 
    #text(-25, 55, paste0("Wilks'\n lambda: \n", round(as.vector(wilks_formula$statistic), 5)))
    #text(40, 55, paste0("Canon.\n Correl.: \n", signif(as.vector(conon_corr$cor), 3)))
    text(-20, 55, paste0("Wilks'\n lambda: \n", round(as.vector(wilks_formula$statistic), 5)), cex = 0.5)
    text(35, 55, paste0("Canon.\n Correl.: \n", signif(as.vector(conon_corr$cor), 3)), cex = 0.5)
    
  }
  
  dev.off()
  
  
  
  ## In order to reduce the number of clusters two approaches can be implemented:
  ##
  ## 1) Hierarchical ISODATA Clustering (method used in the report)
  ## 2) Hierarchical Cluster Analysis / Dendogram Analysis (method used in the Remote Sensing paper)
  
  
  ## 1) Hierarchical ISODATA clustering ####
  
  path2data_isodata1 <- paste0(path2tempResults, "/isodata_test3")
  
  head(isodata_test$data_ini_centroids)
  nrow(isodata_test$data_ini_centroids)
  head(pca_data_ini)
  
  varbles <- names(pca_data_ini)
  nvars <- length(names(pca_data_ini))
  
  
  stats_intraclustr <- as.data.frame(isodata_test$data_ini_centroids %>% group_by(centroid) %>% summarise_at(.vars = varbles, .funs = c("mean")))
  stats_intraclustr
  
  data_centroids <- isodata_test$data_ini_centroids
  data_centroids$row_nms <- rownames(data_centroids)
  head(data_centroids[, c((nvars + 2), (nvars + 1))])
  
  pca_data_ini_avrge <- merge(data_centroids[, c((nvars + 2), (nvars + 1))], stats_intraclustr, 
                              by = "centroid", all.x = TRUE)
  rownames(pca_data_ini_avrge) <- pca_data_ini_avrge$row_nms
  pca_data_ini_avrge <- pca_data_ini_avrge[order(as.numeric(pca_data_ini_avrge$row_nms)), ]
  
  pca_data_ini_avrge <- pca_data_ini_avrge[, - c(1, 2)]
  
  head(pca_data_ini_avrge)
  nrow(pca_data_ini_avrge)
  
  
  isodata_test1 <- isodata_clustering(data_ini = pca_data_ini_avrge,
                                      nclust_ini = nclust_ini, 
                                      max_num_clustrs = max_num_clustrs,
                                      max_SD_intraclust = max_SD_intraclust,
                                      min_dist_interclust = min_dist_interclust,
                                      max_iter = max_iter, 
                                      path2saveResults = path2tempResults)
  
  
  ## And this should be repeated until the number of clusters stabilizes. But this is extremely slow...
  ## and the iteration flow a bit weird (from time to time it reduces the number of clusters to 1).
  ## In case this approach wants to be used, this issue should be carefully checked.
  #
  
  
  
  ## 2) Hierarchical Cluster Analysis / Dendogram Analysis ####
  
  ## It needs to be stablished either the height of the dendrogram where to cut for the grouping (new clustering) or 
  ## the number of new clusters we want
  ## And also the iteration of previous step we select (either the last or  the one with lower Wilks' lambda)
  height2cut <- 2
  
  #this two lines need to be generalized:
  path2data_isodata1 <- paste0(path2tempResults, "/isodata_test3")
  load(paste0(path2data_isodata1, "/results_isodata_iter", 11, ".RData"), verbose = TRUE)
  
  
  varbles <- names(data_ini_noCentr)[1:4]
  nvars <- length(names(data_ini_noCentr)[1:4])
  
  stats_intraclustr <- as.data.frame(data_ini_noCentr %>% group_by(closest) %>% summarise_at(.vars = varbles, .funs = c("mean")))
  
  clust_centr_ini$closest <- rownames(clust_centr_ini)
  
  data_centroids <- data_ini_noCentr[, 1:5]
  data_centroids <- rbind(data_centroids, clust_centr_ini)
  data_centroids <- data_centroids[order(as.numeric(rownames(data_centroids))), ]
  data_centroids$row_nms <- rownames(data_centroids)
  
  pca_data_ini_avrge <- merge(data_centroids[, c((nvars + 2), (nvars + 1))], stats_intraclustr, 
                              by = "closest", all.x = TRUE)
  
  rownames(pca_data_ini_avrge) <- pca_data_ini_avrge$row_nms
  pca_data_ini_avrge <- pca_data_ini_avrge[order(as.numeric(pca_data_ini_avrge$row_nms)), ]
  pca_data_ini_avrge <- pca_data_ini_avrge[, - c(1, 2)]
  
  
  pca_data_ini_avrge_unique <- pca_data_ini_avrge[rownames(pca_data_ini_avrge) %in% unique(clust_centr_ini$closest), ]
  
  
  d <- dist(as.matrix(pca_data_ini_avrge_unique))    
  hc <- hclust(d, method = "ward.D2")               # Ward's minimum variance method (the objective function is the error sum of squares; the dissimilarities are squared before cluster updating) 
  plot(hc)                                          # plotting dendrogram
  #to plot rectangles
  rect.hclust(hc, 
              h = height2cut, #height where to cut
              #k = NULL, #exact number of clusters to produce
              border = 2, #colours for the rectangles
              #cluster = NULL
  )
  #assigning new clusters
  new_clstrs <- as.data.frame(cutree(hc, h = height2cut))
  names(new_clstrs) <- "new_clst"
  
  data_centroids <- merge(data_centroids, new_clstrs, by.x = "closest", by.y = "row.names", all.x = TRUE)
  rownames(data_centroids) <- data_centroids$row_nms
  data_centroids <- data_centroids[order(as.numeric(data_centroids$row_nms)), ]
  data_centroids <- data_centroids[, !names(data_centroids) %in% c("closest", "row_nms")]
  
  
  
}







## K-means ####

#Warning message:
#  Quick-TRANSfer stage steps exceeded maximum (= 2147483647)     #see: https://stackoverflow.com/questions/21382681/kmeans-quick-transfer-stage-steps-exceeded-maximum


## Determining number of clusters  #https://www.statmethods.net/advstats/cluster.html

## 1) Scree plot (elbow) method

wss <- (nrow(pca_data_ini) - 1) * sum(apply(pca_data_ini, 2, var))
num_clstrs <- 2:15
num_clstrs <- c(5, 10, 15, 20, 25, 30, 40, 50, 60)
for (i in 2:(length(num_clstrs) + 1)) wss[i] <- kmeans(pca_data_ini,
                                                       centers = num_clstrs[i - 1],
                                                       nstart = 1,
                                                       #iter.max = 25, # did not converge (any)
                                                       iter.max = 10,
                                                       algorithm = "MacQueen")$tot.withinss

pdf(paste0(path2tempResults, "/optim_num_clusters.pdf"))
plot(c(1, num_clstrs), wss, type = "b", 
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

dev.off() 




## 2) Statistics method (calculating one or more metrics: GAP, etc)

#library(NbClust)
#opt_clust <- NbClust(data = pca_data_ini, 
#                     distance = "euclidean",  #Error in dist(jeu, method = "euclidean") : vector is too large
#                     #distance = "ward.D2", #Error in NbClust(data = pca_data_ini, distance = "ward.D2", min.nc = 5,  :invalid distance
#                     min.nc = 5, max.nc = 40, 
#                     method = "kmeans",
#                     #index = "alllong"
#                     index = "gap"
#                     )
#opt_clust

#library(factoextra)
#opt_clust <- fviz_nbclust(pca_data_ini, FUNcluster = kmeans, method = "gap_stat",     #Error in dist(xs) : vector is too large
#                          diss = NULL, 
#                          k.max = 10, 
#                          #nboot = 100,
#                          nboot = 10
#                          )






## Clustering using optimal number of clusters

t0 <- Sys.time()
kmeans_clustring <- kmeans(pca_data_ini, 
                           centers = 20, 
                           #iter.max = 10,  # Warning: did not converge in 10 iterations 
                           #iter.max = 50,   # Warning: did not converge in 50 iterations 
                           #iter.max = 100,  # Warning: did not converge in 100 iterations (~ 15 minutes)
                           iter.max = 500,  
                           nstart = 1,
                           algorithm = "MacQueen"
                           )
t1 <- Sys.time() - t0
save(list = c("kmeans_clustring", "t1"), file = paste0(path2tempResults, "/results_Step9_kmeans.RData"))
#load(paste0(path2tempResults, "/results_Step9_kmeans.RData"), verbose = TRUE)




## Binding NA data and adding back spatial information ####

#pca_final_raster1 <- pca_final_raster

pca_data_ini$clstr <- kmeans_clustring$cluster
pca_data_ini$rn <- as.numeric(rownames(pca_data_ini)) 
pca_data_ini <- pca_data_ini[, names(pca_data_ini) %in% c("clstr", "rn")]



all_data <- bind_rows(pca_data_ini, pca_data_ini_NA)#, data_ini_4later)
rm(pca_data_ini, pca_data_ini_NA)
#gc()


#all_data <- all_data %>% 
#             as_tibble() %>%
#             arrange(rn) %>%
#             column_to_rownames(var = "rn")

all_data <- all_data[order(all_data$rn), ]
#rownames(all_data) <- all_data$rn



pca_final_raster <- raster(paste0(path2tempResults, "/pca_final_raster.tif"))

xtnt <- extent(pca_final_raster)
pca_final_clstrs_raster <- raster(nrows = pca_final_raster@nrows, ncols = pca_final_raster@ncols,
                                  crs = crs(pca_final_raster), 
                                  ext = xtnt, 
                                  #resolution, 
                                  vals = all_data$clstr)
 
names(pca_final_clstrs_raster) <- "clusterNum"






## Plotting clusters  ####

rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\clusters_EFTs.jpg"), width = 31, height = 21, units = "cm", res = 600, pointsize = 8)

  par(mfcol = c(1, 1), mar = c(2.5, 2, 2, 1.5))
  
  
  pal <- colorRampPalette(c("mistyrose1", "pink1", "violet", "blue", "skyblue2", "green", "darkolivegreen2", "yellow", "orange", "tomato3", "brown4", "coral4"))
  par(xpd = FALSE)
  plot(pca_final_clstrs_raster, 
       col = pal(length(unique(all_data$clstr)) - 1), 
       legend = TRUE, 
       main = paste0((length(unique(all_data$clstr)) - 1), " clusters")
       )
  #legend("bottom",
  #       #x = - 30, y = 30,
  #       ncol = 1,
  #       legend = unique(all_data$clstr),
  #       fill = pal(length(unique(all_data$clstr)) - 1), 
  #       inset = - 0.43
  #       )
         
  dev.off()
  
}






## Saving results ####


#quick plot to check
#jpeg(paste0(path2saveTests, "\\SpatialPatternsPCs_clstrs.jpg"))
#plot(pca_final_clstrs_raster[["new_clst"]])
#dev.off()


#stuff2save <- c("data_centroids", "pca_data_ini_clusters", "pca_final_clstrs_raster")
pca_final_clstrs <- all_data
stuff2save <- c("pca_final_clstrs", "pca_final_clstrs_raster")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step9.RData"))
writeRaster(pca_final_clstrs_raster, paste0(path2tempResults, "/SpatialPatternsPCs_clstrs.tif"), overwrite = TRUE)
#load(file = paste0(path2tempResults, "/results_Step9.RData"), verbose = TRUE)
#all_data <- pca_final_clstrs
#pca_final_clstrs_raster <- raster(paste0(path2tempResults, "/SpatialPatternsPCs_clstrs.tif"))







