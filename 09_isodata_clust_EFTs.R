
#### ISODATA clustering  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data from 'final PCA' (Step 08) ####

load(file = paste0(path2tempResults, "/results_Step8.RData"), verbose = TRUE)
#pca_final_raster
#summary(pca_final)
#
#head(as.data.frame(pca_final_raster))
#nrow(as.data.frame(pca_final_raster))
#as.data.table(as.data.frame(pca_final_raster))
#
#apply(as.data.table(as.data.frame(pca_final_raster)), 2, sd, na.rm = TRUE)
#apply(as.data.table(as.data.frame(pca_final_raster)), 2, mean, na.rm = TRUE)
#

#initial data set to get clustered
pca_data_ini <- as.data.frame(pca_final_raster)
#head(pca_data_ini)
#tail(pca_data_ini)
#range(as.numeric(rownames(pca_data_ini)))
#apply(pca_data_ini, 2, function(x) sum(is.na(x)))
#sum(!complete.cases(pca_data_ini))

sampling2test <- "Yes"
sampling2test <- "No"
if(sampling2test == "Yes"){
  nrow(pca_data_ini)
  toSample <- sample(rownames(pca_data_ini), (nrow(pca_data_ini) / 10))
  head(toSample)
  pca_data_ini_4later <- pca_data_ini[!rownames(pca_data_ini) %in% toSample, ]
  pca_data_ini <- pca_data_ini[rownames(pca_data_ini) %in% toSample, ]
}


pca_data_ini_NA <- pca_data_ini[!complete.cases(pca_data_ini), ]   #to be used at the end to fill raster stack
pca_data_ini <- pca_data_ini[complete.cases(pca_data_ini), ] 
  



#nrow(pca_data_ini)
#head(pca_data_ini)


## Settings for clustering 
#number of initial clusters
nclust_ini <- 50 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)
nclust_ini <- 15 # initial number of clusters (hard-coded; 10 < nclust_ini < 500)  # isodata_test1


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
                                   path2saveResults = path2tempResults)





## Some checkings ####

iters <- c("iter1", "iter2", "iter3") 
iters <- paste0("iter", 1:iter_num) 
#iters <- paste0("iter", 1:100) 
#afegir totes les iteracions que s han generat (max 100) i comprovar els wilks

#for (i in iters) {
#  print(i)
#  load(paste0(path2tempResults, "/results_isodata_", i, ".RData"), verbose = FALSE)
#  print(paste0("clust_mergd: ", paste0(clust_mergd, collapse = ", ")))
#  print(paste0("big_clusters to split: ", paste0(big_clusters, collapse = ", ")))
#  #assign(paste0("data_ini_noCentr_", i), data_ini_noCentr)
#  frmla <- as.formula(paste0("closest ~ ", paste0(colnames(data_ini_noCentr)[1:nvars], collapse = " + ")))
#  wilks_formula <- Wilks.test(frmla, data = data_ini_noCentr)
#  print(paste0("Wilks' Lambda for... ", i))
#  print(wilks_formula$statistic)
#}


isodata_test$isodta_wilks
isodata_test$centroids_isodta
isodata_test$iterations
isodata_test$number_clusters
head(isodata_test$data_ini_centroids)
length(unique(isodata_test$data_ini_centroids$centroid))
length(isodata_test$number_clusters)
table(isodata_test$data_ini_centroids$centroid)




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
                panel.text((nrow(wilks_clust_df) * 90 / 100), 0, paste0("Min. Wilks' Lambda (iter: ", which.min(wilks_clust_df$WilksLambda), ")"), 
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
                panel.text((nrow(wilks_clust_df) * 90 / 100), 0, paste0("Min. Wilks' Lambda (iter: ", which.min(wilks_clust_df$WilksLambda), ")"), 
                           cex = 0.5, col = "darkblue")
                panel.points(which.min(wilks_clust_df$WilksLambda), min(wilks_clust_df$WilksLambda), col = "darkblue")
              }
)

plt1 <- xyplot(CanonicalCorrel ~ Iteration, wilks_clust_df,
               grid = TRUE,
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



#for (i in iters) {
#  print(i)
#  load(paste0(path2tempResults, "/results_isodata_", i, ".RData"), verbose = FALSE)
#  #print(paste0("clust to merge unique: ", length(unique(clust_mergd))))
#  print(paste0("nrow(clust_centr_ini): ", nrow(clust_centr_ini)))
#}
#
#for (i in iters) {
#  print(i)
#  load(paste0(path2tempResults, "/results_isodata_", i, ".RData"), verbose = FALSE)
#  #print((dim(pca_final_raster)[1] * dim(pca_final_raster)[2]) == (nrow(data_ini_4later) + nrow(pca_data_ini_NA) + nrow(data_ini_noCentr[, 1:4]) + nrow(clust_centr_ini)))
#  print((dim(pca_final_raster)[1] * dim(pca_final_raster)[2]) == (nrow(pca_data_ini_NA) + nrow(data_ini_noCentr[, 1:4]) + nrow(clust_centr_ini)))
#}




## Plotting clusterings (iterations) ####

#jpeg(paste0(path2saveTests, "\\clusters.jpg"), width = 1500, height = 1500, res = 300)
#pdf(paste0(path2saveTests, "\\clusters.pdf"), width = 7, height = 7)
pdf(paste0(path2saveTests, "\\clusters1.pdf"), width = 18, height = 18)
par(mfcol = c(10, 10), mar = c(2.5, 1.5, 2, 1.5))

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








