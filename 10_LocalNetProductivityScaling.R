
#### Local Net Productivity Scaling (LNS)  ####

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


## Reading in EFTs data (Step 09) ####

load(file = paste0(path2tempResults, "/results_Step9.RData"), verbose = FALSE)
rm(pca_final_clstrs_raster)
#gc()


## Reading in Phenolo data (Cyclic Fraction) and averaging ####

if(grepl("OldData", var2process_name)){
  #si_clean <- stack(paste0(path2tempResults, "/si_clean.tif"))
  #CycleFraction_rstr <- si_clean
  CycleFraction_rstr <- stack(paste0(path2tempResults, "/si_clean.tif"))

}else{
  load(paste0(path2tempResults, "/CycleFraction_EndStep01.RData"), verbose = TRUE)
  
  ## To raster bricks
  CycleFraction_rstr <- brick(CycleFraction)
  CycleFraction_rstr <- t(CycleFraction_rstr)
  extent(CycleFraction_rstr) <- c(range(lon),  range(lat))
  CycleFraction_rstr
  
}


## Averaging
beginCluster(cors2use)   # it uses n - 1 clusters
yrs <- (nlayers(CycleFraction_rstr) - 4):nlayers(CycleFraction_rstr)
CycleFraction_rstr_average <- clusterR(CycleFraction_rstr, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()

CycleFraction_rstr_average
stuff2save <- c("CycleFraction_rstr_average")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step10.RData"))
#load(file = paste0(path2tempResults, "/results_Step10.RData"), verbose = TRUE)
rm(CycleFraction_rstr)   # Removing here with the aim of freeing memory, although it needs to be loaded again later

## Merging Cyclic Fraction data with new clusters ####
CycleFraction_average_df <- as.data.frame(CycleFraction_rstr_average)

#CycleFraction_average_df <- as.data.frame(cbind(CycleFraction_average_df, pca_final_clstrs[, "clstr", drop = FALSE]))
#names(CycleFraction_average_df) <- c("CyclicFraction", "EFT")
#CycleFraction_average_df$rwnms <- as.numeric(rownames(CycleFraction_average_df))

CycleFraction_average_df$EFT <- pca_final_clstrs$clstr
CycleFraction_average_df$rwnms <- pca_final_clstrs$rn
names(CycleFraction_average_df)[1] <- "CyclicFraction"
if(exists("pca_final_clstrs")) rm(pca_final_clstrs)
#gc()


## Calculating 90-percentile by EFT ####
## EFT = 0 is NoData in the raster
CycleFraction_90perc <- as.data.table(CycleFraction_average_df %>% group_by(EFT) %>% summarise_at(.vars = "CyclicFraction", .funs = c("CyclicFraction_90perc" = quantile), prob = 0.9, na.rm = TRUE))


## Assigning maximum (potential) productivity to outliers ####
## outliers:  value > percentile 90 intra-cluster

CycleFraction_average_df <- merge(as.data.table(CycleFraction_average_df), CycleFraction_90perc, by = "EFT", all.x = TRUE) 
#CycleFraction_average_df <- CycleFraction_average_df[order(CycleFraction_average_df$rwnms), ]
#CycleFraction_average_df <- CycleFraction_average_df[order(rwnms)]
setkeyv(CycleFraction_average_df, "rwnms")

cond <- CycleFraction_average_df$CyclicFraction > CycleFraction_average_df$CyclicFraction_90perc & !is.na(CycleFraction_average_df$CyclicFraction)
CycleFraction_average_df$CyclicFraction[cond] <- CycleFraction_average_df$CyclicFraction_90perc[cond]
rm(cond)
CycleFraction_average_df$CyclicFraction_90perc[is.na(CycleFraction_average_df$CyclicFraction)] <- NA

CycleFraction_average_df <- as.data.frame(CycleFraction_average_df)
CycleFraction_average_df <- CycleFraction_average_df[, !names(CycleFraction_average_df) %in% "rwnms"]



## Calculating Local Scaled Productivity (LSP) ####
## Current status of efficiency of productivity: % over "potential", per pixel

CycleFraction_average_df$LSP <- round(((CycleFraction_average_df$CyclicFraction / CycleFraction_average_df$CyclicFraction_90perc) * 100), 1)
names(CycleFraction_average_df)[3] <- "PotentialProduction"




## Saving results ####
gc()

if(grepl("OldData", var2process_name)){
  #si_clean <- stack(paste0(path2tempResults, "/si_clean.tif"))
  #CycleFraction_rstr <- si_clean
  CycleFraction_rstr <- stack(paste0(path2tempResults, "/si_clean.tif"))
  
}else{
  load(paste0(path2tempResults, "/CycleFraction_EndStep01.RData"), verbose = TRUE)
  ## To raster bricks
  CycleFraction_rstr <- brick(CycleFraction)
  CycleFraction_rstr <- t(CycleFraction_rstr)
  extent(CycleFraction_rstr) <- c(range(lon),  range(lat))
  CycleFraction_rstr
}

CycleFraction_rstr <- CycleFraction_rstr[[1]]

#LocalNetProductivity_rstr <- setValues(CycleFraction_rstr, as.matrix(CycleFraction_average_df))
LocalNetProductivity_rstr <- setValues(CycleFraction_rstr, CycleFraction_average_df$LSP)

stuff2save <- c(stuff2save, "CycleFraction_average_df", "LocalNetProductivity_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step10.RData"))
#writeRaster(LocalNetProductivity_rstr, paste0(path2saveTests, "/LocalNetProductivity.tif"), bylayer = TRUE, suffix = c(names(LocalNetProductivity_rstr)), overwrite = TRUE)
writeRaster(LocalNetProductivity_rstr, paste0(path2tempResults, "/LocalNetProductivity_LSP.tif"), overwrite = TRUE)

#cellStats(LocalNetProductivity_rstr, max)


## Plotting results  ####
LocalNetProductivity_rstr <- raster(paste0(path2tempResults, "/LocalNetProductivity_LSP.tif"))


if(rning_plts == "y"){

  jpeg(paste0(path2saveTests, "/LocalNetProductivity_LSP.jpg"),
       units = "cm", width = 20, height = 12,
       pointsize = 12, quality = 75, res = 300)
  
  par(mar = c(3, 4, 4, 0))
  pal <- colorRampPalette(c("coral1", "yellow", "palegreen3"))
  par(xpd = FALSE)
  plot(LocalNetProductivity_rstr, col = pal(100), legend = FALSE) 
  par(xpd = TRUE)
  #legend("bottom",
  #       legend = c(LocalNetProductivity_rstr@data@min,  LocalNetProductivity_rstr@data@max),
  #       fill = pal(100), inset = 0.01)
  library(plotrix)
  color.legend(-140, -70, 140, -63, 
               paste0(c(LocalNetProductivity_rstr@data@min, 20, 40, 60, 80, LocalNetProductivity_rstr@data@max), "%"),
               pal(100),
               gradient = "x", align = "rb",
               cex = .8)
  title(main = "Local Scaled Productivity of Cyclic Fraction", cex.main = 1.3)
  
  dev.off()
}

