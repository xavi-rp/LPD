
#### Combined Assessment of Land Productivity  ####
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data sets (Step 05 and Step10) ####

LandProd_change <- raster(paste0(path2saveTests, "/LandProd_change.tif"))
LandProd_change

LocalNetProductivity <- raster(paste0(path2saveTests, "/LocalNetProductivity_LSP.tif"))
LocalNetProductivity




## Combined Assessment ####

LPD_CombAssess <- LocalNetProductivity

LPD_CombAssess[LandProd_change %in% c(1:6, 8:9)      & LocalNetProductivity  < 50] <- 1      # 1(d): Declining land productivity
LPD_CombAssess[LandProd_change %in% c(3, 6)          & LocalNetProductivity >= 50] <- 1      # 1(d): Declining land productivity
LPD_CombAssess[LandProd_change %in% c(7)             & LocalNetProductivity  < 50] <- 2      # 2(ew): Early signs of decline of land productivity
LPD_CombAssess[LandProd_change %in% c(1:2, 4:5, 8:9) & LocalNetProductivity >= 50] <- 2      # 2(ew): Early signs of decline of land productivity
LPD_CombAssess[LandProd_change %in% c(7)             & LocalNetProductivity >= 50] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
LPD_CombAssess[LandProd_change %in% c(10:12)                                     ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
LPD_CombAssess[LandProd_change %in% c(13:15)                                     ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
LPD_CombAssess[LandProd_change %in% c(16:17, 19)     & LocalNetProductivity  < 50] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
LPD_CombAssess[LandProd_change %in% c(18, 20:22)     & LocalNetProductivity  < 50] <- 5      # 5(i): Increasing land productivity
LPD_CombAssess[LandProd_change %in% c(16:17, 19:20)  & LocalNetProductivity >= 50] <- 5      # 5(i): Increasing land productivity
LPD_CombAssess[LandProd_change %in% c(18, 21:22)     & LocalNetProductivity >= 50] <- 6      # 6(si): Strongly increasing land productivity
LPD_CombAssess[is.na(LandProd_change)] <- NA
LPD_CombAssess



## Saving results ####

stuff2save <- c("LPD_CombAssess")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step11.RData"))
writeRaster(LPD_CombAssess, paste0(path2saveTests, "/LPD_CombinedAssessment.tif"), overwrite = TRUE)









