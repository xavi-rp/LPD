
#### Final PCA  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}


## Reading in data from 'screening PCA' (first PCA - Step07) ####

load(file = paste0(path2tempResults, "/results_Step7.RData"), verbose = TRUE)

stack_rstrs_avg_noC
stack_rstrs_avg_noC_df
pca
pca_importance
pca.rotated
screeningPCA_cumul_variance
screeningPCA_variables


## Performing the Final PCA ####

stack_rstrs_avg_noC_df_final <- stack_rstrs_avg_noC_df[, names(stack_rstrs_avg_noC_df) %in% screeningPCA_variables]
#pca <- prcomp(na.omit(stack_rstrs_df), scale = TRUE)   
pca_final <- prcomp(na.omit(stack_rstrs_avg_noC_df_final),
                    retx = TRUE, 
                    center = TRUE, # variables are zero centered 
                    scale. = TRUE  # scaling variables to have unit variance
                    )   

pca_final
pca_final$rotation
head(pca_final$x)
head(stack_rstrs_avg_noC_df_final)
stack_rstrs_avg_noC_df_final[8431, ]
summary(pca_final)

## Rotating PCs to unequivocally associate each variable to a PC ####
# This is not needed because what we use from here on is the rotated variables (PCs) already calculated with prcomp()
avoid_this <- "Y"
if(avoid_this != "Y") {
  pca_final.rotated <- varimax(pca_final$rotation, normalize = TRUE) # normalize = TRUE; Should Kaiser normalization be performed? If so the rows of x are re-scaled to unit length before rotation, and scaled back afterwards.
  pca_final.rotated
  pca_final.rotated$loadings
  matrix(head(pca_final.rotated$loadings, 16), nrow = 4, ncol = 4)
  pca_final.rotated$rotmat
  
  finalPCA_variables_df <- pca_final.rotated$loadings
  finalPCA_variables_df <- as.data.frame(unclass(finalPCA_variables_df))
  finalPCA_variables_df
  finalPCA_variables_df <- round(finalPCA_variables_df, 0)[, c(1:nPCs)]
  #finalPCA_variables <- names(which(apply(finalPCA_variables_df, 1, sum) != 0))
  
  variable_PC <- t(apply(finalPCA_variables_df, 1, function(x) names(x)[which(x != 0)]))
  variable_PC
  
  pca_final$rotation
}


## Vector with loadings to multiplicate with variables ####
#Podria ser que aixo no estigui be i que el que realment es fagi servir per mapejar i continuar amb l'isodata'
#sigui directament pca_final$x. Mira notes 14 de juny
#Llavors, caldria adjuntar aquest data frame a algun raster utilitzant un merge a partir dels rownames 
avoid_this <- "Y"
if(avoid_this != "Y") {
  value2EOF <- c()
  for (i in 1:length(as.vector(variable_PC))) {
    value2EOF <- c(value2EOF, round(pca_final$rotation[i, as.vector(variable_PC)[i]], 4))
  }
  
  value2EOF <- rbind(variable_PC, value2EOF)
  rownames(value2EOF)[1] <- "PC"
  value2EOF_vals <- as.numeric(as.character(as.vector(value2EOF[2, ])))
  
  ## Calculating EOFs #### 
  # It's wrong calling this EOFs. EOF finds both time (series) and spatial patterns, and here we don't have the component time
  # Eva Ivits no longer used this term in the paper on Remote Sensing journal (2013)
  EFTs_rstrs <- stack_rstrs_avg_noC
  EFTs_rstrs <- stack(EFTs_rstrs@layers[names(EFTs_rstrs) %in% screeningPCA_variables])
  EFTs_rstrs <- EFTs_rstrs * value2EOF_vals
  EFTs_rstrs
} #end of 'avoid_this'




## Spatial Patterns of PCs ####

stack_rstrs_avg_noC

pca_final_rottd_varbles <- as.data.frame(pca_final$x)

head(pca_final_rottd_varbles)
head(as.data.frame(stack_rstrs_avg_noC[[1]]))

rstr_dt <- as.data.table(as.data.frame(stack_rstrs_avg_noC[[1]]), keep.rownames = TRUE)
rstr_dt$rn <- as.numeric(as.character(rstr_dt$rn))
setkeyv(rstr_dt, "rn")
pca_dt <- as.data.table(pca_final_rottd_varbles, keep.rownames = TRUE)
pca_dt$rn <- as.numeric(as.character(pca_dt$rn))
setkeyv(pca_dt, "rn")

rstr_pca_dt <- merge(rstr_dt[, 1], pca_dt, all.x = TRUE)
nrow(rstr_pca_dt)
rstr_pca_dt[rstr_pca_dt$rn == 8431, ]
rstr_pca_dt

#rstr_SLavrg <- stack_rstrs_avg_noC[[1]]
#rstr_SLavrg
#setValues(rstr_SLavrg, kk$PC1)
#rstr_SLavrg
#setValues(rstr_SLavrg$PC2, kk$PC2)
#rstr_SLavrg

pca_final_raster <- stack_rstrs_avg_noC
pca_final_raster <- setValues(pca_final_raster, as.matrix(rstr_pca_dt))
pca_final_raster <- pca_final_raster[[-1]]
pca_final_raster

jpeg(paste0(path2saveTests, "\\SpatialPatternsPCs.jpg"))
plot(pca_final_raster)
dev.off()





## Saving results ####

stuff2save <- c("pca_final", "pca_final_raster")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step8.RData"))



