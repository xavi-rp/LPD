
#### Preparing Old Data Sets For Testing  ####

# These are the data sets used (probably) to perform the published study
# They are derived from SPOT

rm(list = ls())

if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  source("/Users/xavi_rp/Documents/D6_LPD/LPD/00_settings.R")
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
  varbl <- brick(paste0(path2old_data, vbl, "9913.bil"))
  names(varbl) <- 1999:2013
  varbl <- varbl[[-dim(varbl)[3]]]   # removing last year as it is incomplete
  assign(vbl, varbl)
}

stuff2save <- vrbls_lst

## Cleaning variables??
## This means giving NA to oceans (as 0 in th raw data set), removing outliers, etc
clean_vrbls <- "y"


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




## Cleaning MI variable ####

if(clean_vrbls == "y"){
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
#if(file.exists(paste0(path2tempResults, "/mi_clean.tif"))) file.remove(paste0(path2tempResults, "/mi_clean.tif"))
writeRaster(mi_clean, paste0(path2tempResults, "/mi_clean.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)

#summary(getValues(mi_clean[[1]]))




## Cleaning SI variable ####

if(clean_vrbls == "y"){
  #si_clean <- si
  #si_clean[si_clean <= 0] <- NA
  #si_clean[si_clean >= 7000] <- NA   # this represents about 99.95% of pixels
  
  #si_clean <- crop(si_clean, extent(-11.16071, 27.05357, 33.66964, 71.13393))
  
  if(any(dim(si) != dim(mi))){
    crop(si, mi, filename = paste0(path2tempResults, "/si_crop.tif"), overwrite = TRUE)
    si <- stack(paste0(path2tempResults, "/si_crop.tif"))
  } 

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

writeRaster(si_clean, paste0(path2tempResults, "/si_clean.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)




## Cleaning SL variable ####

if(clean_vrbls == "y"){
  sl_clean <- sl
  
  #summary(getValues(sl_clean$X1999))
  #quantile(getValues(sl_clean$X1999), seq(0, 1, 0.1))
  #length(getValues(sl_clean$X1999))      # 591600687
  #sum(getValues(sl_clean$X1999) == 0)    # 188
  #sum(getValues(sl_clean$X1999) < 0)     # 416623938
  #sum(getValues(sl_clean$X1999) < -1)    # 637
  #sum(getValues(sl_clean$X1999) == -1)    # 416623301   #This is the value for NA (oceans)
  #quantile(getValues(sl_clean$X1999), 0.99)    # 295
  #quantile(getValues(sl_clean$X1999), 0.9995)  # 363
  #quantile(getValues(sl_clean$X1999), 0.9999)  # 420
  #sum(getValues(sl_clean$X1999) > 420)  # 58638
  #quantile(getValues(sl_clean$X1999), 0.99999) # 483
  #sum(getValues(sl_clean$X1999) > 483)  # 5844
  #sum(getValues(sl_clean$X1999) > 730)  # 0         # this is two full years season
  #sum(getValues(sl_clean$X1999) > 365)  # 207245    # this is one full year season
  
  #sl_clean[sl_clean <= 0] <- NA
  #sl_clean[sl_clean > 730] <- NA   # # this represents about ???% of pixels
  
  valsM <- max(maxValue(sl))
  valsm <- min(minValue(sl))
  
  rclsf <- t(data.frame(c((valsm - 1), 0, NA), c(731, (valsM + 1), NA)))
  #sl_clean <- reclassify(sl_clean, rcl = rclsf, include.lowest = TRUE, right = TRUE)
  
  t0 <- Sys.time()
  beginCluster(cors2use)   # beginCluster() uses n - 1 clusters
  sl_clean <- clusterR(sl, reclassify, args = list(rcl = rclsf, include.lowest = TRUE, right = TRUE))
  endCluster()
  print(paste0("Reclassification made in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  
  stuff2save <- c(stuff2save, "sl_clean")
  
}else{
  stuff2save <- stuff2save
}

writeRaster(sl_clean, paste0(path2tempResults, "/sl_clean.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)




## Cleaning SBD variable ####

if(clean_vrbls == "y"){
  #sbd_clean <- sbd
  #summary(getValues(sbd_clean$X1999))
  #quantile(getValues(sbd_clean$X1999), seq(0, 1, 0.1))
  #quantile(getValues(sbd_clean), seq(0, 1, 0.1), na.rm = TRUE)
  #sum(getValues(sbd_clean$sbd_crop.1) < -356, na.rm = TRUE)
  
  t0 <- Sys.time()
  sbd_clean <- mask(sbd, mask = mi_clean)
  #writeRaster(sbd_clean, paste0(path2tempResults, "/sbd_crop.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
  
  #valsM <- max(maxValue(sbd_clean))
  #valsm <- min(minValue(sbd_clean))
  #
  #rclsf <- t(data.frame(c((valsm - 1), 0, NA), c(7000, (valsM + 1), NA)))
  ##sbd_clean <- reclassify(sbd_clean, rcl = rclsf, include.lowest = TRUE, right = TRUE)
  #
  #t0 <- Sys.time()
  #beginCluster(cors2use)   # beginCluster() uses n - 1 clusters
  #sbd_clean <- clusterR(sbd_clean, reclassify, args = list(rcl = rclsf, include.lowest = TRUE, right = TRUE))
  #endCluster()
  #print(paste0("Reclassification made in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  print(paste0("Masked in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  stuff2save <- c(stuff2save, "sbd_clean")
  
}else{
  stuff2save <- stuff2save
}

writeRaster(sbd_clean, paste0(path2tempResults, "/sbd_clean.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)



## Cleaning SED variable ####

if(clean_vrbls == "y"){
  #sed_clean <- sed
  #quantile(getValues(sed_clean$X1999), seq(0, 1, 0.1))
  #sum(getValues(sed_clean$X1999) < -356, na.rm = TRUE)
  
  t0 <- Sys.time()
  sed_clean <- mask(sed, mask = mi_clean)
  #writeRaster(sed_clean, paste0(path2tempResults, "/sed_crop.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
  
  #valsM <- max(maxValue(sed))
  #valsm <- min(minValue(sed))
  #
  #rclsf <- t(data.frame(c((valsm - 1), 0, NA), c(7000, (valsM + 1), NA)))
  ##sed_clean <- reclassify(sed_clean, rcl = rclsf, include.lowest = TRUE, right = TRUE)
  #
  #t0 <- Sys.time()
  #beginCluster(cors2use)   # beginCluster() uses n - 1 clusters
  #sed_clean <- clusterR(sed, reclassify, args = list(rcl = rclsf, include.lowest = TRUE, right = TRUE))
  #endCluster()
  #print(paste0("Reclassification made in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  #
  print(paste0("Masked in: ", (Sys.time() - t0), " ", attr((Sys.time() - t0), "units")))
  stuff2save <- c(stuff2save, "sed_clean")
  
}else{
  stuff2save <- stuff2save
}

writeRaster(sed_clean, paste0(path2tempResults, "/sed_clean.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
save(list = stuff2save, file = paste0(path2tempResults, "/OldDataSets_EndStep011.RData"))
#load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)






#jpeg(paste0(path2saveTests, "\\mi_clean.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
#plot(mi_clean[[1]])
#dev.off()
#writeRaster(mi_clean$layer.1, paste0(path2saveTests, "/mi_clean.tif"), overwrite = TRUE)





#varbl_simask <- stack(paste0(path2old_data, "si9913mskd.bil"))
#names(varbl_simask) <- 1999:2013
#varbl_simask
#
#summary(getValues(varbl_simask$X1999))



## Cropping for package ####

vrbls_lst <- c("sbd", "sed", "si", "sl", "mi")

for (vbl in vrbls_lst){
  print(vbl)
  
  if(vbl == "sed"){
    rstr_name <- paste0(path2tempResults, "/", vbl, "_cleanGood.tif")
  }else{
    rstr_name <- paste0(path2tempResults, "/", vbl, "_clean.tif")
  }
  
  varbl <- brick(rstr_name)
  names(varbl) <- paste0(vbl, "_", 1999:2012)
  
  cat_extnt <- extent(c(0.3, 3.4, 40.4, 43))
  varbl <- crop(varbl, cat_extnt, filename = paste0(path2tempResults, "/", vbl, "_clean_Cat.tif"), overwrite = TRUE)
  
  
  if(vbl == "sbd"){
    extent_good <- extent(get(paste0(vbl, "_clean_Cat")))
  }else if(vbl %in% c("sed", "si")){
    extent(varbl) <- extent_good
  }
    
  assign(paste0(vbl, "_clean_Cat"), varbl)
  
  if(vbl == "sed"){
    writeRaster(get(paste0(vbl, "_clean_Cat")), paste0(path2tempResults, "/sed_clean_Cat_1.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
    sed_clean_Cat <- brick(paste0(path2tempResults, "/sed_clean_Cat_1.tif"))
    writeRaster(sed_clean_Cat, paste0(path2tempResults, "/sed_clean_Cat.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
    if (file.exists(paste0(path2tempResults, "/sed_clean_Cat_1.tif"))) file.remove(paste0(path2tempResults, "/sed_clean_Cat_1.tif"))
    sed_clean_Cat <- brick(paste0(path2tempResults, "/sed_clean_Cat.tif"))
  }
  if(vbl == "si"){
    writeRaster(get(paste0(vbl, "_clean_Cat")), paste0(path2tempResults, "/si_clean_Cat_1.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
    si_clean_Cat <- brick(paste0(path2tempResults, "/si_clean_Cat_1.tif"))
    writeRaster(si_clean_Cat, paste0(path2tempResults, "/si_clean_Cat.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
    if (file.exists(paste0(path2tempResults, "/si_clean_Cat_1.tif"))) file.remove(paste0(path2tempResults, "/si_clean_Cat_1.tif"))
    si_clean_Cat <- brick(paste0(path2tempResults, "/si_clean_Cat.tif"))
  }
}


# Checks
for (vbl in vrbls_lst){
  #print(paste0("resolution ", vbl, ": "))
  #print(res(get(paste0(vbl, "_clean_Cat"))))
  #
  print(paste0("dim ", vbl, ": "))
  print(dim(get(paste0(vbl, "_clean_Cat"))))

  #print(paste0("extent ", vbl, ": "))
  #print(as.vector(extent(get(paste0(vbl, "_clean_Cat")))))
}









