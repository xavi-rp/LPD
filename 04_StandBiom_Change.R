
#### Standing Biomass State Change  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
   source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
   source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
   stop("Define your machine before to run LPD")
}

cat("Calculating Standing Biomass State Change (Step 04)... ", "\n")


## Reading in data (Standing Biomass) ####

if(grepl("OldData", var2process_name)){
   #load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)
   var2process <- stack(paste0(path2tempResults, "/mi_clean.tif"))
   #assign(var2process_name, mi_clean)
   #var2process <- mi_clean  
   cat("processing 'mi_clean'... ", "\n")
   
}else{
   #load(paste0(path2tempResults, "/season_length_EndStep01.RData"), verbose = TRUE)
   load(paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"), verbose = TRUE)
   
   ## Normalising data set (0-1) 
   do_normalize <- "no"
   
   if(grepl("^[Yy]", do_normalize)){
      #range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
      
      SeasonIntegral_01 <- range01(SeasonIntegral, na.rm = TRUE)
      dim(SeasonIntegral_01)
      summary(as.vector(SeasonIntegral_01[, , 1]))
      
      hist(SeasonIntegral, breaks=10)
      hist(SeasonIntegral_01[, , 1], breaks=10)
      hist(SeasonIntegral_01, breaks=10)
      
      sum(is.na(SeasonIntegral_01[ , , 3]))
   }else{
      SeasonIntegral_01 <- SeasonIntegral
   }
   
   
   ## to a raster brick
   var2process <- brick(SeasonIntegral_01)
   var2process <- t(var2process)
   extent(var2process) <- c(range(lon),  range(lat))
   var2process
   
} 



load(paste0(path2tempResults, "/results_Step3.RData"), verbose = TRUE)
#rm(SeasonIntegral_01_avg13, SeasonIntegral_class10_stats, pix_categs2, SeasonIntegral_3class, SteadInd_Baseline)

#var2process
#assign(paste0(var2process_name, "_10class_begin"), get(paste0(var2process_name, "_10class")))
assign(paste0(var2process_name, "_10class_begin"), raster(paste0(path2tempResults, "/StandingBiomass_10class_begin.tif")))  # Check this!!!!




## Averaging last 3 years ####

#with parallelization           
t0 <- Sys.time()
beginCluster(cors2use)   
num_yrs <- dim(var2process)[3]
yrs <- ((num_yrs) - 2):(num_yrs)
#SeasonIntegral_01_avgLast3 <- clusterR(var2process, calc, args = list(fun = mean_years_function), export = "yrs")
var2process_avgLast3 <- clusterR(var2process, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()
Sys.time() - t0

assign(paste0(var2process_name, "_avgLast3"), var2process_avgLast3)
stuff2save <- c(var2process_name , paste0(var2process_name, "_10class_begin"), paste0(var2process_name, "_avgLast3"))
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step4.RData"))
writeRaster(var2process_avgLast3, paste0(path2tempResults, "/StandingBiomass_avgLast3.tif"), overwrite = TRUE)



## Reclassifying average-last-3 into 10 categories ####
# using same thresholds of average-first-3 to be consistent when calculating category shifts 

pix_categs1[1, 1] <- 0
pix_categs1[nrow(pix_categs1), 2] <- max(getValues(var2process_avgLast3), na.rm = TRUE) + 1
var2process_10class_end <- reclassify(var2process_avgLast3, rcl = pix_categs1, filename='', include.lowest = TRUE, right = TRUE)
var2process_10class_end
writeRaster(var2process_10class_end, paste0(path2tempResults, "/StandingBiomass_10class_end.tif"), overwrite = TRUE)


## Calculating Standing Biomass change (difference begin - end; reclassifying into 3 categories)
var2process_10class_dif <- get(paste0(var2process_name, "_10class_begin")) - var2process_10class_end
var2process_10class_dif
#unique(getValues(var2process_10class_dif))
#table(getValues(var2process_10class_dif))
writeRaster(var2process_10class_dif, paste0(path2tempResults, "/StandingBiomass_10class_dif.tif"), overwrite = TRUE)

pix_categs3 <- as.data.frame(matrix(nrow = 5, ncol = 0))
pix_categs3$from    <- c(-10, -1.5, -0.5, 0.5, 1.5)
pix_categs3$to      <- c( -2,   -1,    0,   1,   9)
pix_categs3$becomes <- c(  3,    2,    1,   2,   3)

var2process_3classChange <- reclassify(var2process_10class_dif, rcl = pix_categs3, filename='', include.lowest = TRUE, right = TRUE)
var2process_3classChange
writeRaster(var2process_3classChange, paste0(path2tempResults, "/StandingBiomass_3classChange.tif"), overwrite = TRUE)


assign(paste0(var2process_name, "_3classChange"), var2process_3classChange)
assign(paste0(var2process_name, "_10class_end"), var2process_10class_end)
assign(paste0(var2process_name, "_10class_dif"), var2process_10class_dif)
stuff2save <- c(stuff2save, paste0(var2process_name, "_3classChange"), paste0(var2process_name, "_10class_end"), paste0(var2process_name, "_10class_dif"))
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step4.RData"))



## Some plots and statistics
# plotting
rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
   jpeg(paste0(path2saveTests, "\\SeasInt_Change.jpg"), width = 28, height = 20, units = "cm", res = 300)
   par(mar = c(9.2, 4, 4, 4), mfrow = c(1, 2))
   pal <- colorRampPalette(c("wheat2", "skyblue2", "blue"))
   categs <- c("No Change", "Changed 1 categ", expression("Changed" >= "2 categs"))
   par(xpd = FALSE)
   plot(var2process_3classChange, col = pal(3), legend = FALSE) 
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
   
   if((length(time) - 2) == dim(var2process)[3]){
      y2plot <- time[-c(1,length(time))]
      y2plot_beg <- y2plot[1:3][c(1,length(yrs))]
      y2plot_end <- y2plot[yrs][c(1,length(yrs))]
   }else if((length(time)) == dim(var2process)[3]){
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
   
   cont_table <- as.data.frame(table(getValues(var2process_3classChange)))
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

}
