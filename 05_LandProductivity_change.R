
#### Land-Productivity Long Term Change Map  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data (Season_Integral) ####

load(paste0(path2tempResults, "/results_Step3.RData"), verbose = TRUE)
load(paste0(path2tempResults, "/results_Step4.RData"), verbose = TRUE)

rm(SeasonIntegral_01_avg13, SeasonIntegral_class10_stats, pix_categs2, SeasonIntegral_3class)
rm(SeasonIntegral_01, SeasonIntegral_10class_begin, SeasonIntegral_01_avgLast3, SeasonIntegral_10class_end, SeasonIntegral_10class_dif)

SteadInd_SeasInt
SeasonIntegral_3classChange




## Reclassifying into change categories (22) ####


LandProd_change <- SteadInd_SeasInt

LandProd_change[SteadInd_SeasInt == 1 & SeasonIntegral_3classChange == 1] <- 1      #St1-low-No Change
LandProd_change[SteadInd_SeasInt == 1 & SeasonIntegral_3classChange == 2] <- 2      #St1-low-Change 1 categ
LandProd_change[SteadInd_SeasInt == 1 & SeasonIntegral_3classChange == 3] <- 3      #St1-low-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 2 & SeasonIntegral_3classChange == 1] <- 4      #St1-medium-No Change
LandProd_change[SteadInd_SeasInt == 2 & SeasonIntegral_3classChange == 2] <- 5      #St1-medium-Change 1 categ
LandProd_change[SteadInd_SeasInt == 2 & SeasonIntegral_3classChange == 3] <- 6      #St1-medium-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 3 & SeasonIntegral_3classChange == 1] <- 7      #St1-high-No Change
LandProd_change[SteadInd_SeasInt == 3 & SeasonIntegral_3classChange == 2] <- 8      #St1-high-Change 1 categ
LandProd_change[SteadInd_SeasInt == 3 & SeasonIntegral_3classChange == 3] <- 9      #St1-high-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 4 & SeasonIntegral_3classChange == 1] <- 10     #St2-low-No Change
LandProd_change[SteadInd_SeasInt == 4 & SeasonIntegral_3classChange == 2] <- 10     #St2-low-Change 1 categ
LandProd_change[SteadInd_SeasInt == 4 & SeasonIntegral_3classChange == 3] <- 10     #St2-low-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 5 & SeasonIntegral_3classChange == 1] <- 11     #St2-medium-No Change
LandProd_change[SteadInd_SeasInt == 5 & SeasonIntegral_3classChange == 2] <- 11     #St2-medium-Change 1 categ
LandProd_change[SteadInd_SeasInt == 5 & SeasonIntegral_3classChange == 3] <- 11     #St2-medium-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 6 & SeasonIntegral_3classChange == 1] <- 12     #St2-high-No Change
LandProd_change[SteadInd_SeasInt == 6 & SeasonIntegral_3classChange == 2] <- 12     #St2-high-Change 1 categ
LandProd_change[SteadInd_SeasInt == 6 & SeasonIntegral_3classChange == 3] <- 12     #St2-high-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 7 & SeasonIntegral_3classChange == 1] <- 13     #St3-low-No Change
LandProd_change[SteadInd_SeasInt == 7 & SeasonIntegral_3classChange == 2] <- 13     #St3-low-Change 1 categ
LandProd_change[SteadInd_SeasInt == 7 & SeasonIntegral_3classChange == 3] <- 13     #St3-low-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 8 & SeasonIntegral_3classChange == 1] <- 14     #St3-medium-No Change
LandProd_change[SteadInd_SeasInt == 8 & SeasonIntegral_3classChange == 2] <- 14     #St3-medium-Change 1 categ
LandProd_change[SteadInd_SeasInt == 8 & SeasonIntegral_3classChange == 3] <- 14     #St3-medium-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 9 & SeasonIntegral_3classChange == 1] <- 15     #St3-high-No Change
LandProd_change[SteadInd_SeasInt == 9 & SeasonIntegral_3classChange == 2] <- 15     #St3-high-Change 1 categ
LandProd_change[SteadInd_SeasInt == 9 & SeasonIntegral_3classChange == 3] <- 15     #St3-high-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 10 & SeasonIntegral_3classChange == 1] <- 16    #St4-low-No Change
LandProd_change[SteadInd_SeasInt == 10 & SeasonIntegral_3classChange == 2] <- 17    #St4-low-Change 1 categ
LandProd_change[SteadInd_SeasInt == 10 & SeasonIntegral_3classChange == 3] <- 18    #St4-low-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 11 & SeasonIntegral_3classChange == 1] <- 19    #St4-medium-No Change
LandProd_change[SteadInd_SeasInt == 11 & SeasonIntegral_3classChange == 2] <- 20    #St4-medium-Change 1 categ
LandProd_change[SteadInd_SeasInt == 11 & SeasonIntegral_3classChange == 3] <- 21    #St4-medium-Change 2 or more categs
LandProd_change[SteadInd_SeasInt == 12 & SeasonIntegral_3classChange == 1] <- 22    #St4-high-No Change
LandProd_change[SteadInd_SeasInt == 12 & SeasonIntegral_3classChange == 2] <- 22    #St4-high-Change 1 categ
LandProd_change[SteadInd_SeasInt == 12 & SeasonIntegral_3classChange == 3] <- 22    #St4-high-Change 2 or more categs
LandProd_change[is.na(SeasonIntegral_3classChange)] <- NA 
LandProd_change


stuff2save <- c("LandProd_change")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step5.RData"))
writeRaster(LandProd_change, paste0(path2saveTests, "/LandProd_change.tif"), overwrite = TRUE)




## Some plots and statistics for report
# plotting
jpeg(paste0(path2saveTests, "\\LandProd_change.jpg"), width = 29, height = 21, units = "cm", res = 300)
par(mar = c(5, 4, 7, 4), mfrow = c(1, 3))
pal <- colorRampPalette(c("brown", "darkkhaki", "darkgreen"))
categs <- c("St1-low-No Change", "St1-low-Change 1 categ", "St1-low-Change 2 or more categs", 
            "St1-medium-No Change", "St1-medium-Change 1 categ", "St1-medium-Change 2 or more categ", 
            "St1-high-No Change", "St1-high-Change 1 categ", "St1-high-Change 2 or more categs", 
            "St2-low-No Change/Change 1 categ/Change 2 or more categs", 
            "St2-medium-No Change/Change 1 categ/Change 2 or more categs", 
            "St2-high-No Change/Change 1 categ/Change 2 or more categs", 
            "St3-low-No Change/Change 1 categ/Change 2 or more categs", 
            "St3-medium-No Change/Change 1 categ/Change 2 or more categs", 
            "St3-high-No Change/Change 1 categ/Change 2 or more categs", 
            "St4-low-No Change", "St4-low-Change 1 categ", "St4-low-Change 2 or more categs", 
            "St4-medium-No Change", "St4-medium-Change 1 categ", "St4-medium-Change 2 or more categ", 
            "St4-high-No Change/Change 1 categ/Change 2 or more categs")

par(xpd = FALSE)
plot(LandProd_change, col = pal(22), legend = FALSE) 
par(xpd = TRUE)
title(main = "Land-Productivity Long Term Change Map", 
      outer = TRUE,
      #adj = 0,
      line = - 3,
      cex.main = 2)



cont_table <- as.data.frame(table(getValues(LandProd_change)))
#cont_table$Var1 <- categs
names(cont_table)[1] <- "Land-Productivity_change"
#cont_table
brplt <- barplot((cont_table$Freq / 1000), names.arg = as.character(c(1:22)), #names.arg = cont_table$Productivity_change,
                 las = 1, axis.lty = 1,
                 ylab = "Number of pixels per category (x1000)", 
                 #main = "")#,
                 col = pal(22))
text(x = (brplt + 0.25), y = (((cont_table$Freq) / 1000) + 20), 
     label = paste0(round(((cont_table$Freq) * 100)/sum(cont_table$Freq), 1), " %"), 
     cex = 0.9, pos = 3, col = "black", xpd = TRUE, srt = 90)



plot(0,type='n',axes=FALSE,ann=FALSE)

legend("center",
  #x = - 30, y = 30,
  ncol = 1,
  legend = categs,
  fill = pal(22), inset = - 0.25)
#mtext("", 
#      side = 1, line = 7, 
#      #at = 5,
#      adj = 1,
#      cex = 0.8)
#dev.off()


#abline(0, 0)
dev.off()


