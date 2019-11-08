
#### Final PCA  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


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
                    center = TRUE, 
                    scale. = TRUE)   

pca_final
pca_final$rotation
head(pca_final$x)
head(stack_rstrs_avg_noC_df_final)
stack_rstrs_avg_noC_df_final[8431, ]
summary(pca_final)

## Rotating PCs to unequivocally associate each variable to a PC
pca_final.rotated <- varimax(pca_final$rotation, normalize = TRUE) # normalize = TRUE; Should Kaiser normalization be performed? If so the rows of x are re-scaled to unit length before rotation, and scaled back afterwards.
pca_final.rotated
pca_final.rotated$loadings

finalPCA_variables_df <- pca_final.rotated$loadings
finalPCA_variables_df <- as.data.frame(unclass(finalPCA_variables_df))
finalPCA_variables_df
finalPCA_variables_df <- round(finalPCA_variables_df, 0)[, c(1:nPCs)]
#finalPCA_variables <- names(which(apply(finalPCA_variables_df, 1, sum) != 0))

variable_PC <- t(apply(finalPCA_variables_df, 1, function(x) names(x)[which(x != 0)]))
variable_PC

pca_final$rotation

## Vector with loadings to multiplicate with variables
value2EOF <- c()
for (i in 1:length(as.vector(variable_PC))) {
  value2EOF <- c(value2EOF, round(pca_final$rotation[i, as.vector(variable_PC)[i]], 4))
}

value2EOF <- rbind(variable_PC, value2EOF)
rownames(value2EOF)[1] <- "PC"
value2EOF_vals <- as.numeric(as.character(as.vector(value2EOF[2, ])))


## Calculating EOFs ####
EFTs_rstrs <- stack_rstrs_avg_noC
EFTs_rstrs <- stack(EFTs_rstrs@layers[names(EFTs_rstrs) %in% screeningPCA_variables])
EFTs_rstrs <- EFTs_rstrs * value2EOF_vals
EFTs_rstrs




## Saving results ####

stuff2save <- c("pca_final", "pca_final.rotated", "value2EOF", "EFTs_rstrs")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step8.RData"))









