
#### First PCA: 'Screening'  ####

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

cat("Calculating First PCA - screening (Step 07)... ", "\n")


## Reading in data (not correlated Phenolo variables) ####

load(file = paste0(path2tempResults, "/results_Step6.RData"), verbose = FALSE)
stack_rstrs_avg_noC <- brick(paste0(path2tempResults, "/stack_rstrs_avg_noC.tif"))
#stack_rstrs_avg_noC


## Performing the 'screening' PCA ####

stack_rstrs_avg_noC_df <- as.data.frame(stack_rstrs_avg_noC)

#pca <- prcomp(na.omit(stack_rstrs_df), scale = TRUE)   
pca <- prcomp(na.omit(stack_rstrs_avg_noC_df), 
              retx = TRUE, 
              center = TRUE, # variables are zero centered 
              scale. = TRUE  # scaling variables to have unit variance
              )      

#save(pca, file = paste0(path2tempResults, "/results_Step7.RData")) 
#load(paste0(path2tempResults, "/results_Step7.RData"), verbose = TRUE)
#pca
#summary(pca)     
#str(pca)   
#pca$rotation #The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components.
#             #the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors)
#head(pca$x) #The values of each sample in terms of the principal components
#            #the value of the rotated data (the centred (and scaled if requested) data multiplied by the rotation matrix). 
#            #Hence, cov(x) is the diagonal matrix diag(sdev^2)
#unclass(pca)
#predict(pca) #rotated variables


### Making plots
rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
  pdf(paste0(path2tempResults, "/pca_results.pdf"))
  #(as.vector(unclass(pca)[[1]]))^2         # variances (SD^2)
  layout(matrix(1:2, ncol=2))
  screeplot(pca)
  screeplot(pca, type = "lines")
  dev.off()
}



rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
  #pdf(paste0(path2tempResults, "/pca_biplot.pdf"))
  jpeg(paste0(path2tempResults, "/pca_biplot.jpg"))
  #biplot(pca, choices = 1:2, pc.biplot = FALSE, xlim = c(-0.0003, 0.0003), ylim = c(-0.0003, 0.0003))
  biplot(pca)
  dev.off()
}


### Rotating 
pca.rotated <- varimax(pca$rotation, normalize = TRUE) # normalize = TRUE; Should Kaiser normalization be performed? If so the rows of x are re-scaled to unit length before rotation, and scaled back afterwards.
#pca.rotated
#head(pca.rotated$loadings, 25) #??
#pca.rotated$rotmat
#pca.rotated$loadings


#The whole same thing, with another package (results already rotated)
#library(psych)
#pca_psych <- principal(na.omit(stack_rstrs_avg_noC_df), nfactors = 5, 
#                       residuals = FALSE, 
#                       rotate="varimax", n.obs=NA, 
#                       covar=FALSE,
#                       scores=TRUE, missing=FALSE, impute="median", oblique.scores=TRUE, method="regression")
#pca_psych
#summary(pca_psych)
#pca_psych$values
#pca_psych$rotation
#pca_psych$n.obs
#pca_psych$communality
#pca_psych$loadings   
#head(pca_psych$scores)
#head(sort(as.numeric(as.character(rownames(pca_psych$scores)))))
#



### Saving results 

#pca
#summary(pca)  
#pca.rotated

pca_importance <- summary(pca)
pca_importance <- as.data.frame(pca_importance)
cumul_varnce_threshold <- 0.9  # Threshold of cumulative variance used to select the number of useful PCs
usefulPCs <- which(pca_importance[3 ,] >= cumul_varnce_threshold)[1]
pca_importance <- pca_importance[, 1:usefulPCs]

if(nPCs > ncol(pca_importance)) nPCs <- ncol(pca_importance)
#screeningPCA_cumul_variance <- round(pca_importance[3, nPCs], 4)

screeningPCA_variables_df <- pca.rotated$loadings
screeningPCA_variables_df <- as.data.frame(unclass(screeningPCA_variables_df))
screeningPCA_variables_df <- round(screeningPCA_variables_df, 0)[, c(1:nPCs)]
#abs(round(screeningPCA_variables_df, 0)) == 1
screeningPCA_variables <- names(which(apply(screeningPCA_variables_df, 1, sum) != 0))

if(length(screeningPCA_variables) != nPCs) stop("inconsistency among number of variables selected for the next step ('final PCA') and number of PCs")


stuff2save <- c("stack_rstrs_avg_noC", "stack_rstrs_avg_noC_df", "pca", "pca_importance", "pca.rotated", 
                #"screeningPCA_cumul_variance", 
                "screeningPCA_variables")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step7.RData"))

