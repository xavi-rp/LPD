
#### Final PCA  ####

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

cat("Calculating Final PCA (Step 08)... ", "\n")

## Reading in data from 'screening PCA' (first PCA - Step07) ####

load(file = paste0(path2tempResults, "/results_Step7.RData"), verbose = FALSE)
stack_rstrs_avg_noC <- stack(paste0(path2tempResults, "/stack_rstrs_avg_noC.tif"))

#stack_rstrs_avg_noC
#stack_rstrs_avg_noC_df
#pca
#pca_importance
#pca.rotated
#screeningPCA_cumul_variance
#screeningPCA_variables


## Performing the Final PCA ####

stack_rstrs_avg_noC_df_final <- stack_rstrs_avg_noC_df[, names(stack_rstrs_avg_noC_df) %in% screeningPCA_variables]
#pca <- prcomp(na.omit(stack_rstrs_df), scale = TRUE)   
pca_final <- prcomp(na.omit(stack_rstrs_avg_noC_df_final),
                    retx = TRUE, 
                    center = TRUE, # variables are zero centered 
                    scale. = TRUE  # scaling variables to have unit variance
                    )   

#pca_final
#pca_final$rotation
#head(pca_final$x)
#head(stack_rstrs_avg_noC_df_final)
#stack_rstrs_avg_noC_df_final[8431, ]
#summary(pca_final)

save(pca_final, file = paste0(path2tempResults, "/results_Step8_pca_final.RData"))

#load(paste0(path2tempResults, "/results_Step8_pca_final.RData"))


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
if(exists("pca")) rm(pca)
if(exists("stack_rstrs_avg_noC_df_final")) rm(stack_rstrs_avg_noC_df_final)
if(exists("stack_rstrs_avg_noC_df")) rm(stack_rstrs_avg_noC_df)
gc()

pca_final_rottd_varbles <- as.data.frame(pca_final$x)
if(exists("pca_final")) rm(pca_final)
gc()


pca_final_rottd_varbles$rn <- as.integer(rownames(pca_final_rottd_varbles))  # as.integer should be more memory-efficient
num_pix <- stack_rstrs_avg_noC@ncols * stack_rstrs_avg_noC@nrows

num_pix1 <- c(1:num_pix)
num_pix1 <- num_pix1[!num_pix1 %in% as.integer(rownames(pca_final_rottd_varbles))]

#df2fill <- as.data.frame(matrix(NA, nrow = length(num_pix1), ncol = 4))
#names(df2fill) <- c("PC1", "PC2", "PC3", "rn")
df2fill <- as.data.frame(matrix(NA, nrow = length(num_pix1), ncol = ncol(pca_final_rottd_varbles)))
names(df2fill) <- c(paste0("PC", seq(1:(ncol(pca_final_rottd_varbles) - 1))), "rn")
df2fill$rn <- num_pix1
rownames(df2fill) <- num_pix1
rm(num_pix1)

pca_final_raster1 <- rbindlist(list(pca_final_rottd_varbles, df2fill))
rm(df2fill)

setorderv(pca_final_raster1, "rn")
pca_final_raster1 <- pca_final_raster1[, rn := NULL]
#save(pca_final_raster1, file = paste0(path2tempResults, "/results_Step8_pca_final_raster1.RData"))
#load(paste0(path2tempResults, "/results_Step8_pca_final_raster1.RData"))

xtnt <- extent(stack_rstrs_avg_noC)
pca_final_brick <- brick(nrows = stack_rstrs_avg_noC@nrows, ncols = stack_rstrs_avg_noC@ncols, 
                         xmn = xtnt[1], xmx = xtnt[2], ymn = xtnt[3], ymx = xtnt[4], 
                         crs = crs(stack_rstrs_avg_noC),
                         nl = (ncol(pca_final_rottd_varbles) - 1)
                         )
rm(pca_final_rottd_varbles)

for(i in 1:ncol(pca_final_raster1)){
  rastr_tmp <- raster(nrows = stack_rstrs_avg_noC@nrows, ncols = stack_rstrs_avg_noC@ncols,
                      xmn = xtnt[1], xmx = xtnt[2], ymn = xtnt[3], ymx = xtnt[4],
                      crs = crs(stack_rstrs_avg_noC), 
                      #ext, 
                      #resolution, 
                      vals = pca_final_raster1[[i]])
  pca_final_brick[[i]] <- rastr_tmp
  names(pca_final_brick[[i]]) <- names(pca_final_raster1)[i]
}



rning_plts <- "n"
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\SpatialPatternsPCs.jpg"))
  plot(pca_final_brick[[1]])
  dev.off()
}
  

## Saving results ####

writeRaster(pca_final_brick, paste0(path2tempResults, "/pca_final_raster.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
stuff2save <- c("pca_final_raster1", "pca_final_brick")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step8.RData"))
#load(file = paste0(path2tempResults, "/results_Step8.RData"))


