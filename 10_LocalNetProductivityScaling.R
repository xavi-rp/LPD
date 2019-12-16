
#### Local Net Productivity Scaling (LNS)  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in EFTs data (Step 09) ####

load(file = paste0(path2tempResults, "/results_Step9.RData"), verbose = TRUE)

pca_final_clstrs_raster[["new_clst"]]
head(pca_data_ini_clusters)
nrow(pca_data_ini_clusters)

## Reading in Phenolo data (Cyclic Fraction) and averaging ####

load(paste0(path2tempResults, "/CycleFraction_EndStep01.RData"), verbose = TRUE)

## To raster bricks
CycleFraction_rstr <- brick(CycleFraction)
CycleFraction_rstr <- t(CycleFraction_rstr)
extent(CycleFraction_rstr) <- c(range(lon),  range(lat))
CycleFraction_rstr

## Averaging
beginCluster()   # it uses n - 1 clusters
yrs <- (nlayers(CycleFraction_rstr) - 4):nlayers(CycleFraction_rstr)
CycleFraction_rstr_average <- clusterR(CycleFraction_rstr, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()

CycleFraction_rstr_average



## Merging Cyclic Fraction data with new clusters ####
CycleFraction_average_df <- as.data.frame(CycleFraction_rstr_average)
head(CycleFraction_average_df)
nrow(CycleFraction_average_df)

head(pca_data_ini_clusters[, "new_clst", drop=FALSE])
nrow(pca_data_ini_clusters[, "new_clst", drop=FALSE])


CycleFraction_average_df <- as.data.frame(cbind(CycleFraction_average_df, pca_data_ini_clusters[, "new_clst", drop=FALSE]))
names(CycleFraction_average_df) <- c("CyclicFraction", "EFT")
head(CycleFraction_average_df)
nrow(CycleFraction_average_df)


## Calculating 90-percentile by EFT ####
## EFT = 0 is NoData in the raster
CycleFraction_90perc <- as.data.frame(CycleFraction_average_df %>% group_by(EFT) %>% summarise_at(.vars = "CyclicFraction", .funs = c("CyclicFraction_90perc" = quantile), prob = 0.9, na.rm = TRUE))
CycleFraction_90perc
str(CycleFraction_90perc)


## Assigning maximum (potential) productivity to outliers ####
## outliers:  value > percentile 90 intra-cluster

CycleFraction_average_df <- merge(CycleFraction_average_df, CycleFraction_90perc, by = "EFT", all.x = TRUE) 
head(CycleFraction_average_df)

cond <- CycleFraction_average_df$CyclicFraction > CycleFraction_average_df$CyclicFraction_90perc & !is.na(CycleFraction_average_df$CyclicFraction)
CycleFraction_average_df$CyclicFraction[cond] <- CycleFraction_average_df$CyclicFraction_90perc[cond]

CycleFraction_average_df[rownames(CycleFraction_average_df) == 906635, ]



## Calculating Local Scaled Productivity (LSP) ####
## Current status of efficiency of productivity: % over "potential", per pixel

head(CycleFraction_average_df)

CycleFraction_average_df$LSP <- round(((CycleFraction_average_df$CyclicFraction / CycleFraction_average_df$CyclicFraction_90perc) * 100), 1)
head(CycleFraction_average_df)
head(CycleFraction_average_df[CycleFraction_average_df$CyclicFraction < CycleFraction_average_df$CyclicFraction_90perc
                                 & !is.na(CycleFraction_average_df$CyclicFraction), ])

names(CycleFraction_average_df)[3] <- "PotentialProduction"
head(CycleFraction_average_df)
nrow(CycleFraction_average_df)




## Saving results ####

LocalNetProductivity_rstr <- setValues(CycleFraction_rstr[[1:4]], as.matrix(CycleFraction_average_df))

stuff2save <- c("CycleFraction_average_df", "LocalNetProductivity_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step10.RData"))
writeRaster(LocalNetProductivity_rstr, paste0(path2saveTests, "/LocalNetProductivity.tif"), overwrite = TRUE)










