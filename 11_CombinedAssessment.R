
#### Combined Assessment of Land Productivity  ####
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
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


## Reading in data sets (Step 05 and Step10) ####

LandProd_change <- raster(paste0(path2tempResults, "/LandProd_change.tif"))
LandProd_change

LocalNetProductivity <- raster(paste0(path2tempResults, "/LocalNetProductivity_LSP.tif"))
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
Sys.time()


## Saving results ####

stuff2save <- c("LPD_CombAssess")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step11.RData"))
writeRaster(LPD_CombAssess, paste0(path2tempResults, "/LPD_CombinedAssessment.tif"), overwrite = TRUE)
#LPD_CombAssess <- raster(paste0(path2tempResults, "/LPD_CombinedAssessment.tif"))


## Plotting a map ####
rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
   jpeg(paste0(path2saveTests, "\\LPD_CombinedAssessment.jpg"), width = 30, height = 21, units = "cm", res = 900)#, pointsize = 8, quality = 75)
   par(mar = c(11, 4, 6, 0), mfrow = c(1, 1))
   pal <- colorRampPalette(c("red", "orange", "yellow", "greenyellow", "lightskyblue", "royalblue3"))
   categs <- c("1(d): Declining land productivity",
               "2(ew): Early signs of decline of land productivity",
               "3(nf): Negative fluctuation (stable, but stressed land prod.)",
               "4(pf): Positive fluctuation (stable, not stressed land prod.)",
               "5(i): Increasing land productivity",
               "6(si): Strongly increasing land productivity"
   )
   
   par(xpd = FALSE)
   plot(LPD_CombAssess, col = pal(6), legend = FALSE) 
   par(xpd = TRUE)
   title(main = "Land Productivity Dynamics:\nCombined Assessment", 
         outer = TRUE,
         #adj = 0,
         line = - 4.5,
         cex.main = 2)
   
   #plot(0,type='n', axes=FALSE, ann=FALSE)
   
   legend("bottom",
          #x = - 30, y = 30,
          ncol = 1,
          legend = categs,
          fill = pal(6), inset = - 0.43
   )
   
   dev.off()
   
}






