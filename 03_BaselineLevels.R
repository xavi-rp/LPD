
#### Base Line Calculation (Standing Biomass)  ####

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

cat("Calculating base line levels (Step 03)... ", "\n")

## Reading in data (Standing Biomass) ####


if(grepl("OldData", var2process_name)){
  #load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)
  var2process <- stack(paste0(path2tempResults, "/mi_clean.tif"))
  assign(var2process_name, var2process)
  #var2process <- mi_clean  
  cat("processing 'mi_clean'... ", "\n")
  
}else{
  #load(paste0(path2tempResults, "/season_length_EndStep01.RData"), verbose = TRUE)
  load(paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"), verbose = TRUE)
  cat("processing 'SeasonIntegral'... ", "\n")
  

  ## Normalising data set (0-1) 
  do_normalize <- "no"
  
  if(grepl("^[Yy]", do_normalize)){
    #range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
    
    SeasonIntegral_01 <- range01(SeasonIntegral, na.rm = TRUE)   # This has to be Standing Biomass!!!
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




#jpeg(paste0(path2saveTests, "\\SeasonIntegral_01.jpg"))
#plot(SeasonIntegral_01)
#dev.off()
#jpeg(paste0(path2saveTests, "\\SeasonIntegral_01_y1.jpg"))
#plot(SeasonIntegral_01[[1]])
#dev.off()

#summary(getValues(mi_clean$layer.1))


## Averaging ####

#with parallelization           
t0 <- Sys.time()
beginCluster(cors2use)   
yrs <- 1:3
var2process_avg13 <- clusterR(var2process, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()
cat("Average calculated in: ",(Sys.time() - t0), " ", attr((Sys.time() - t0), "units"), "\n")


assign(paste0(var2process_name, "_avg13"), var2process_avg13)
stuff2save <- c(var2process_name, paste0(var2process_name, "_avg13"))
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))


#some checks...
rning_tsts <- "n"
rning_tsts <- "y"
if(rning_tsts == "y"){
  chk_avg <- round(mean(as.vector(var2process[7336, 20159][1:3])), 0) == round(as.vector(var2process_avg13[7336, 20159]), 0) # has to be TRUE
  if(chk_avg != TRUE) stop("Something wrong in the averaging process")
}


#rning_plts <- "y"
#rning_plts <- "n"
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\StandingBiomass_avg13.jpg"))
  plot(var2process_avg13)
  dev.off()
}
  



## Classification of averaged pixels ####

# Method found in 'zonal10class.pro'; However, it's not reclassifying into groups with similar number of pixels, 
#                                     but groups with regular range of values. It's not an ISODATA methodology either
#rg_SI <- range(getValues(SeasonIntegral_01_avg13), na.rm = TRUE)
#thr <- (rg_SI[2] - rg_SI[1]) / 10
#thr
#
#rg_SI[1] 
#rg_SI[1] + 1 * thr
#rg_SI[1] + 2 * thr
#rg_SI[1] + 3 * thr
#


# Alternative method: it calculates percentiles in order to make groups with similar number of pixels
#                     It's not an ISODATA methodology but more similar to what is explained in the report

pix_categs <- raster::quantile(var2process_avg13, probs =  seq(0, 1, 0.1), names = TRUE)
pix_categs
pix_categs01 <- c(pix_categs[- length(pix_categs)])
pix_categs01

pix_categs1 <- as.data.frame(pix_categs[-1])
pix_categs1$from <- pix_categs01 
pix_categs1$becomes <- 1:(length(pix_categs) - 1)
#pix_categs1$becomes <- c(rep(1, 4), rep(2, 5), 3)

pix_categs1 <- pix_categs1[, c(2, 1, 3)]
names(pix_categs1)[2] <- "to"
pix_categs1

var2process_10class <- reclassify(var2process_avg13, rcl = pix_categs1, filename='', include.lowest = TRUE, right = TRUE)
var2process_10class
writeRaster(var2process_10class, paste0(path2tempResults, "/StandingBiomass_10class_begin.tif"), overwrite = TRUE)

#jpeg(paste0(path2saveTests, "\\SeasonIntegral_10class.jpg"))
#plot(SeasonIntegral_10class)
#dev.off()


var2process_avg13_df <- as.data.table(getValues(var2process_avg13))
var2process_avg13_df_class10 <- as.data.frame(matrix(nrow = 0, ncol = 0))

for(i in 1:nrow(pix_categs1)){
  cond <- var2process_avg13_df$V1 >= pix_categs1[i, 1] & var2process_avg13_df$V1 <  pix_categs1[i, 2]
  
  var2process_avg13_df_class10_1 <- var2process_avg13_df[cond]
  var2process_avg13_df_class10_1$class10 <- pix_categs1[i, 3]
  
  var2process_avg13_df_class10 <- rbind(var2process_avg13_df_class10, var2process_avg13_df_class10_1)
  
}

#head(SeasonIntegral_01_avg13_df_class10)
#unique(SeasonIntegral_01_avg13_df_class10[SeasonIntegral_01_avg13_df_class10$V1 <= 0.1430105, ]$class10)
#sum(SeasonIntegral_01_avg13_df_class10[SeasonIntegral_01_avg13_df_class10$V1 <= 0.1430105, ]$class10 == 2)
#table(SeasonIntegral_01_avg13_df_class10$class10)
var2process_class10_stats <- as.data.frame(var2process_avg13_df_class10 %>% group_by(class10) %>% summarise_all(.funs = c(n = "length", min = "min", max = "max", mean = "mean", sd = "sd")))


#pix_categs2 <- as.data.frame(matrix(nrow = 3, ncol = 0))
#pix_categs2$from <- c(1, 5, 10)
#pix_categs2$to <- c(4, 9, 10)
#pix_categs2$becomes <- c(1, 2, 3)

pix_categs2 <- pix_categs1
pix_categs2$becomes <- c(rep(1, 4), rep(2, 5), 3)

var2process_3class <- reclassify(var2process_avg13, rcl = pix_categs2, filename='', include.lowest = TRUE, right = TRUE)
var2process_3class
writeRaster(var2process_3class, paste0(path2tempResults, "/SeasonIntegral_3class_begin.tif"), overwrite = TRUE)

assign(paste0(var2process_name, "_10class"), var2process_10class)
assign(paste0(var2process_name, "_class10_stats"), var2process_class10_stats)
assign(paste0(var2process_name, "_3class"), var2process_3class)
stuff2save <- c(stuff2save, "pix_categs1", paste0(var2process_name, "_10class"), 
                paste0(var2process_name, "_class10_stats"), "pix_categs2", 
                paste0(var2process_name, "_3class"))
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))


#rning_plts <- "y"
#rning_plts <- "n"
if(rning_plts == "y"){
  #var2process_3class <- raster(paste0(path2tempResults, "/SeasonIntegral_3class_begin.tif"))
  jpeg(paste0(path2saveTests, "/StandingBiomass_3class.jpg"),
       units = "cm", width = 20, height = 13.9,
       pointsize = 12, quality = 75, res = 300)
  par(mar = c(3, 3, 2, 0), bty = "n")
  pal <- colorRampPalette(c("red4", "yellow", "darkgreen"))
  par(xpd = FALSE)
  plot(var2process_3class, col = pal(3), legend = FALSE) 
  par(xpd = TRUE)
  legend("bottom",
         #title = "First 3 years averaged",
         legend = c("Low", "Medium", "High"),
         ncol = 3,
         fill = pal(3), inset = 0.07)
  title(main = "Baseline Levels of Standing Biomass",
        outer = TRUE,
        line = - 3.5,
        cex.main = 2)
  mtext("Note: Used the average of the first 3 years of the time series", 
        side = 1, line = 2, 
        #at = 5,
        adj = 0,
        cex = 0.7)
  dev.off()
} 




## Combining Steadiness Index with baseline levels for Standing Biomass ####

#load(file = paste0(path2tempResults, "/results_Step2.RData"), verbose = TRUE)
#rm(slope_rstr, mtid_rstr)
SteadInd_rstr <- raster(paste0(path2tempResults, "/SteadInd_raster.tif"))

SteadInd_Baseline <- raster(var2process_3class)

SteadInd_Baseline[SteadInd_rstr == 1 & var2process_3class == 1] <- 1   # Steadiness Index 1 (Strong Negative) - Standing Biomass 1 (low)      -> St1-low
SteadInd_Baseline[SteadInd_rstr == 1 & var2process_3class == 2] <- 2   # Steadiness Index 1 (Strong Negative) - Standing Biomass 2 (medium)   -> St1-medium
SteadInd_Baseline[SteadInd_rstr == 1 & var2process_3class == 3] <- 3   # Steadiness Index 1 (Strong Negative) - Standing Biomass 3 (high)     -> St1-high
SteadInd_Baseline[SteadInd_rstr == 2 & var2process_3class == 1] <- 4   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 1 (low)    -> St2-low
SteadInd_Baseline[SteadInd_rstr == 2 & var2process_3class == 2] <- 5   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 2 (medium) -> St2-medium
SteadInd_Baseline[SteadInd_rstr == 2 & var2process_3class == 3] <- 6   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 3 (high)   -> St2-high
SteadInd_Baseline[SteadInd_rstr == 3 & var2process_3class == 1] <- 7   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 1 (low)    -> St3-low
SteadInd_Baseline[SteadInd_rstr == 3 & var2process_3class == 2] <- 8   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 2 (medium) -> St3-medium
SteadInd_Baseline[SteadInd_rstr == 3 & var2process_3class == 3] <- 9   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 3 (high)   -> St3-high
SteadInd_Baseline[SteadInd_rstr == 4 & var2process_3class == 1] <- 10   # Steadiness Index 4 (Strong Positive) - Standing Biomass 1 (low)     -> St4-low
SteadInd_Baseline[SteadInd_rstr == 4 & var2process_3class == 2] <- 11   # Steadiness Index 4 (Strong Positive) - Standing Biomass 2 (medium)  -> St4-medium
SteadInd_Baseline[SteadInd_rstr == 4 & var2process_3class == 3] <- 12   # Steadiness Index 4 (Strong Positive) - Standing Biomass 3 (high)    -> St4-high
SteadInd_Baseline

stuff2save <- c(stuff2save, "SteadInd_Baseline")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))
writeRaster(SteadInd_Baseline, paste0(path2tempResults, "/SteadInd_Baseline.tif"), overwrite = TRUE)
#SteadInd_Baseline <- raster(paste0(path2tempResults, "/SteadInd_Baseline.tif"))
#load(paste0(path2tempResults, "/results_Step3.RData"))

#jpeg(paste0(path2saveTests, "\\SteadInd_Baseline.jpg"))
#plot(SteadInd_Baseline)
#dev.off()


# plotting for report
#rning_plts <- "y"
#rning_plts <- "n"
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "/SteadInd_Baseline.jpg"), width = 28, height = 20, units = "cm", res = 300)
  layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE),
         heights = c(2, 1))# ,widths = c(2, 1))
  par(bty = 'n')
  par(mar = c(2, 2, 2, 0), bty = 'n')#, mfrow = c(1, 2))
  pal <- colorRampPalette(c("brown2", "brown3", "brown4", "wheat2", "wheat3", "wheat4", "palegreen2", "palegreen3", "palegreen4", "skyblue2", "skyblue3", "skyblue4"))
  categs <- c("St1-low", "St1-medium", "St1-high", "St2-low", "St2-medium", "St2-high", "St3-low", "St3-medium", "St3-high", "St4-low", "St4-medium", "St4-high")
  par(xpd = FALSE)
  plot(SteadInd_Baseline, col = pal(12), legend = FALSE) 
  par(xpd = TRUE)
  title(main = "Steadiness Index combined with baseline levels of Standing Biomass", 
        outer = TRUE,
        #adj = 0,
        line = - 2.5,
        cex.main = 1.7)
  legend(#"bottom",
         -175, 40,
         ncol = 1,
         legend = categs,
         fill = pal(12)#, 
         #inset = 0.1
         )
  #dev.off()
  
  ## Some statistics to include in the plot
  cont_table <- as.data.frame(table(getValues(SteadInd_Baseline)))
  cont_table$Var1 <- categs
  names(cont_table)[1] <- "SteadInd_StandBiomass_categs"
  par(mar = c(5, 6, 8, 2), bty = 'n')
  barplot(rev(cont_table$Freq), names.arg = rev(cont_table$SteadInd_StandBiomass_categs), las = 3, #axis.lty = 1,
          xlab = "Number of pixels per category", 
          #main = "Steadiness Index combined with baseline levels \nof Standing Biomass",
          col = rev(pal(12)), horiz = TRUE, las = 1, cex.names = 0.5, cex.axis = 0.8)
  #abline(0, 0)
  
  mtext("St1: Strong Neg; St2: Medium Neg; St3: Medium Pos; St4: Strong Pos", 
        side = 3, line = 2, 
        #at = 5,
        adj = 0,
        cex = 0.8)
  if((length(time) - 2) == dim(var2process)[3]){
    y2plot <- time[-c(1,length(time))]
    y2plot <- y2plot[yrs][c(1,length(yrs))]
  }else if((length(time)) == dim(var2process)[3]){
    y2plot <- time[yrs][c(1,length(yrs))]
  }else{
    y2plot <- ""
  }
  
  y2plot <- paste0("Baseline years: average of ", paste(y2plot, collapse = "-"))
  mtext(y2plot, 
        side = 3, line = 1, 
        #at = 5,
        adj = 0,
        cex = 0.8)
  
  dev.off()


}

