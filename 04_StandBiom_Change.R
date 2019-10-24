
#### Standing Biomass State Change  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data (Season_Integral) ####
load(paste0(path2tempResults, "/Season_Integral_EndStep01.RData"), verbose = TRUE)
rm(SeasonIntegral, lon, lat)

load(paste0(path2tempResults, "/results_Step3.RData"), verbose = TRUE)
rm(SeasonIntegral_01_avg13, SeasonIntegral_class10_stats, pix_categs2, SeasonIntegral_3class, SteadInd_SeasInt)

SeasonIntegral_01
SeasonIntegral_10class_begin <- SeasonIntegral_10class


## Averaging last 3 years ####

#with parallelization           
t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
num_yrs <- dim(SeasonIntegral_01)[3]
yrs <- ((num_yrs) - 2):(num_yrs)
SeasonIntegral_01_avgLast3 <- clusterR(SeasonIntegral_01, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()
Sys.time() - t0

stuff2save <- c("SeasonIntegral_01", "SeasonIntegral_10class_begin", "SeasonIntegral_01_avgLast3")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step4.RData"))



## Reclassifying average-last-3 into 10 categories ####
# using same thresholds of average-first-3 to be consistent when calculating category shifts 

pix_categs1[1, 1] <- 0
pix_categs1[nrow(pix_categs1), 2] <- max(getValues(SeasonIntegral_01_avgLast3), na.rm = TRUE) + 1
SeasonIntegral_10class_end <- reclassify(SeasonIntegral_01_avgLast3, rcl = pix_categs1, filename='', include.lowest = TRUE, right = TRUE)
SeasonIntegral_10class_end
writeRaster(SeasonIntegral_10class_end, paste0(path2saveTests, "/SeasonIntegral_10class_end.tif"), overwrite = TRUE)


## Calculating Standing Biomass change (difference begin - end; reclassifying into 3 categories)
SeasonIntegral_10class_dif <- SeasonIntegral_10class_begin - SeasonIntegral_10class_end
SeasonIntegral_10class_dif
unique(getValues(SeasonIntegral_10class_dif))
table(getValues(SeasonIntegral_10class_dif))
writeRaster(SeasonIntegral_10class_dif, paste0(path2saveTests, "/SeasonIntegral_10class_dif.tif"), overwrite = TRUE)

pix_categs3 <- as.data.frame(matrix(nrow = 5, ncol = 0))
pix_categs3$from    <- c(-10, -1.5, -0.5, 0.5, 1.5)
pix_categs3$to      <- c( -2,   -1,    0,   1,   9)
pix_categs3$becomes <- c(  3,    2,    1,   2,   3)

SeasonIntegral_3classChange <- reclassify(SeasonIntegral_10class_dif, rcl = pix_categs3, filename='', include.lowest = TRUE, right = TRUE)
SeasonIntegral_3classChange
writeRaster(SeasonIntegral_3classChange, paste0(path2saveTests, "/SeasonIntegral_3classChange.tif"), overwrite = TRUE)


stuff2save <- c(stuff2save, "SeasonIntegral_3classChange", "SeasonIntegral_10class_end", "SeasonIntegral_10class_dif")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step4.RData"))



## Some plots and statistics
# plotting
jpeg(paste0(path2saveTests, "\\SeasInt_Change.jpg"), width = 28, height = 20, units = "cm", res = 300)
par(mar = c(9.2, 4, 4, 4), mfrow = c(1, 2))
pal <- colorRampPalette(c("wheat2", "skyblue2", "blue"))
categs <- c("No Change", "Changed 1 categ", expression("Changed" >= "2 categs"))
par(xpd = FALSE)
plot(SeasonIntegral_3classChange, col = pal(3), legend = FALSE) 
par(xpd = TRUE)
title(main = "Class Change for the Standing Biomass", 
      outer = TRUE,
      #adj = 0,
      line = - 1.5,
      cex.main = 1.3)
legend("bottom",
       ncol = 1,
       legend = categs,
       fill = pal(3), inset = - 0.25)

if((length(time) - 2) == dim(SeasonIntegral_01)[3]){
   y2plot <- time[-c(1,length(time))]
   y2plot_beg <- y2plot[1:3][c(1,length(yrs))]
   y2plot_end <- y2plot[yrs][c(1,length(yrs))]
}else if((length(time)) == dim(SeasonIntegral)[3]){
   y2plot_beg <- time[c(1, 3)]
   y2plot_end <- time[c((length(time) - 2), length(time))]
}else{
   y2plot <- ""
}

y2plot_beg <- paste0("Beginning years: average of ", paste(y2plot_beg, collapse = "-"))
mtext(y2plot_beg, 
      side = 1, line = 7, 
      #at = 5,
      adj = 0,
      cex = 0.8)
y2plot_end <- paste0("End years: average of ", paste(y2plot_end, collapse = "-"))
mtext(y2plot_end, 
      side = 1, line = 8, 
      #at = 5,
      adj = 0,
      cex = 0.8)
#dev.off()

cont_table <- as.data.frame(table(getValues(SeasonIntegral_3classChange)))
cont_table$Var1 <- categs
names(cont_table)[1] <- "StandBiomass_change"
#cont_table
brplt <- barplot((cont_table$Freq / 1000), names.arg = cont_table$StandBiomass_change, las = 3, axis.lty = 1,
                 ylab = "Number of pixels per category (x1000)", 
                 #main = "Class Change for the Standing Biomass")#,
                 col = pal(3))
text(x = brplt, y = ((cont_table$Freq) /1000), 
     label = paste0(round(((cont_table$Freq) * 100)/sum(cont_table$Freq), 1), " %"), 
     cex = 0.9, pos = 3, col = "black", xpd = TRUE)

#abline(0, 0)
dev.off()


