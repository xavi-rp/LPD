
#### Preparing Old Data Sets For Testing  ####

# These are the data sets used (probably) to perform the published study
# They are derived from SPOT

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}



## Reading in data ####

vrbls_lst <- list.files(path2old_data, pattern = ".bil$", full.names = FALSE)
vrbls_lst

# Variables used for the test -> *

# sbd9913.bil -> Season Beginning Day *
# sbd9913mn.bil -> Season Beginning Day Mean (1999-2013)

# sed9913.bil -> Season End Day *
# sed9913mn.bil -> Season End Day Mean (1999-2013)

# si9913.bil -> Season Integral *
# si9913mskd.bil -> Season Integral Mask (???)

# sl9913.bil -> Season Length *

# mi9913.bil -> Minimum Integral (also called Standing Biomass - SB) *

# apl9913.bil -> Autocorrelation_peak_lag (???)

# apv9913.bil -> Autocorrelation_peak_value (???)

# status9913.bil -> Status.9813spotvgt30 (???); This has one year more (1998)

vrbls_lst <- c("sbd", "sed", "si", "sl", "mi")

for (vbl in vrbls_lst){
  varbl <- stack(paste0(path2old_data, vbl, "9913.bil"))
  names(varbl) <- 1999:2013
  assign(vbl, varbl)
}

stuff2save <- vrbls_lst


#for (vbl in vrbls_lst){
#  print(vbl)
#  kk <- get(vbl)
#  print(summary(getValues(kk[[1]])))
#  rm(kk)
#}

#dataType(si)
#si
#
#
#kk <- getValues(si$X1999)
#summary(kk)
#length(kk)
#sum(kk == 0)
#sum(kk < -32767)
#sum(kk < -10000) / length(kk)
#sum(kk < 0) / length(kk)
#sum(kk < 494) / length(kk)
#sum(kk > 32766)
#sum(kk > 10000) / length(kk)
#
#summary(getValues(sed$X1999))
#sum(getValues(sed$X1999) < -365)
#
#jpeg(paste0(path2saveTests, "\\si_raw.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
#plot(si$X1999)
#dev.off()
#writeRaster(si$X1999, paste0(path2saveTests, "/si_raw.tif"), overwrite = TRUE)
#
#si_clean <- si$X1999
#si_clean[si_clean == 0 ] <- NA
#si_clean[si_clean < -32767] <- NA
#si_clean[si_clean > 32766] <- NA
#
#summary(getValues(si_clean))
#quantile(getValues(si_clean), 0.25, na.rm = TRUE)  # 495
#quantile(getValues(si_clean), 0.01, na.rm = TRUE)  # 33
#quantile(getValues(si_clean), 0.001, na.rm = TRUE) # -343
#quantile(getValues(si_clean), 0.005, na.rm = TRUE) # 15
#
#
#si_clean <- si$X1999
#si_clean[si_clean <= 0 ] <- NA
#
#quantile(getValues(si_clean), 0.90, na.rm = TRUE)  # 1658
#quantile(getValues(si_clean), 0.95, na.rm = TRUE)  # 1820
#quantile(getValues(si_clean), 0.99, na.rm = TRUE)  # 2189
#quantile(getValues(si_clean), 0.999, na.rm = TRUE)  # 2775
#quantile(getValues(si_clean), 0.9999, na.rm = TRUE)  # 27003 
#quantile(getValues(si_clean), 0.9995, na.rm = TRUE)  # 6474 
#
#
#si_clean <- si$X1999
#si_clean[si_clean <= 0] <- NA
#si_clean[si_clean >= 7000] <- NA   # this is a bit random
#
#
#jpeg(paste0(path2saveTests, "\\si_clean.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
#plot(si_clean)
#dev.off()
#writeRaster(si_clean, paste0(path2saveTests, "/si_clean.tif"), overwrite = TRUE)




# extent: -11.16071, 27.05357, 33.66964, 71.13393  (xmin, xmax, ymin, ymax)
#extent(var2process)
#si_crop <- crop(si, extent(var2process))
#
#si_crop
#summary(getValues(si_crop$X2010))
#summary(getValues(var2process$layer.12), na.rm = T)
#quantile(getValues(sbd_crop$X2010), 0.001)
#sum(getValues(si_crop$X2010) > 1516)
#sum(getValues(si_crop$X2010) < 1)
#sum(getValues(si_crop$X2010) < 1) / length(getValues(si_crop$X2010))
#sum(getValues(var2process$layer.12) < 1, na.rm = T) / length(getValues(var2process$layer.12))



## Cleaning SI variable ####
cleanSI <- "y"

if(cleanSI == "y"){
  #si_clean <- si
  #si_clean[si_clean <= 0] <- NA
  #si_clean[si_clean >= 7000] <- NA   # this represents about 99.95% of pixels
  
  #si_clean <- crop(si_clean, extent(-11.16071, 27.05357, 33.66964, 71.13393))
  
  valsM <- max(maxValue(si))
  valsm <- min(minValue(si))
  
  rclsf <- t(data.frame(c((valsm - 1), 0, NA), c(7000, (valsM + 1), NA)))
  #si_clean <- reclassify(si_clean, rcl = rclsf, include.lowest = TRUE, right = TRUE)
  
  t0 <- Sys.time()
  beginCluster(cors2use)   # beginCluster() uses n - 1 clusters
  si_clean <- clusterR(si, reclassify, args = list(rcl = rclsf, include.lowest = TRUE, right = TRUE))
  endCluster()
  print(paste0("Reclassification made in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  
  stuff2save <- c(stuff2save, "si_clean")
  
}else{
  stuff2save <- stuff2save
}


#jpeg(paste0(path2saveTests, "\\si_clean.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
#plot(si_clean_01[[1]])
#dev.off()

save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)



cleanMI <- "y"

if(cleanMI == "y"){
  #mi_clean <- mi
  #mi_clean[mi_clean <= 0] <- NA
  #mi_clean[mi_clean >= 7000] <- NA   # # this represents about 99.95% of pixels
  
  valsM <- max(maxValue(mi))
  valsm <- min(minValue(mi))
  
  rclsf <- t(data.frame(c((valsm - 1), 0, NA), c(7000, (valsM + 1), NA)))
  #mi_clean <- reclassify(mi_clean, rcl = rclsf, include.lowest = TRUE, right = TRUE)
  
  t0 <- Sys.time()
  beginCluster(cors2use)   # beginCluster() uses n - 1 clusters
  mi_clean <- clusterR(mi, reclassify, args = list(rcl = rclsf, include.lowest = TRUE, right = TRUE))
  endCluster()
  print(paste0("Reclassification made in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  
  stuff2save <- c(stuff2save, "mi_clean")
  
}else{
  stuff2save <- stuff2save
}


save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)

#summary(getValues(mi_clean[[1]]))








#jpeg(paste0(path2saveTests, "\\mi_clean.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
#plot(mi_clean[[1]])
#dev.off()
#writeRaster(mi_clean$layer.1, paste0(path2saveTests, "/mi_clean.tif"), overwrite = TRUE)





#varbl_simask <- stack(paste0(path2old_data, "si9913mskd.bil"))
#names(varbl_simask) <- 1999:2013
#varbl_simask
#
#summary(getValues(varbl_simask$X1999))




