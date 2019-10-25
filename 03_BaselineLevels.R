
#### Base Line Calculation (Standing Biomass)  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data (Season_Integral) ####
load(paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"), verbose = TRUE)
summary(SeasonIntegral, na.rm = T)


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
SeasonIntegral_01 <- brick(SeasonIntegral_01)
SeasonIntegral_01 <- t(SeasonIntegral_01)
extent(SeasonIntegral_01) <- c(range(lon),  range(lat))
SeasonIntegral_01

#jpeg(paste0(path2saveTests, "\\SeasonIntegral_01.jpg"))
#plot(SeasonIntegral_01)
#dev.off()
#jpeg(paste0(path2saveTests, "\\SeasonIntegral_01_y1.jpg"))
#plot(SeasonIntegral_01[[1]])
#dev.off()


## Averaging ####

#with parallelization           
t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
yrs <- 1:3
SeasonIntegral_01_avg13 <- clusterR(SeasonIntegral_01, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()
Sys.time() - t0

stuff2save <- c("SeasonIntegral_01", "SeasonIntegral_01_avg13")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))


#some checks...
jpeg(paste0(path2saveTests, "\\SeasonIntegral_avg13.jpg"))
plot(SeasonIntegral_01_avg13)
dev.off()

chk_avg <- round(mean(SeasonIntegral_01[2900, 1900][1:3]), 4) == round(SeasonIntegral_01_avg13[2900, 1900], 4) # has to be TRUE
if(chk_avg != TRUE) stop("Something wrong in the averaging process")


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

pix_categs <- raster::quantile(SeasonIntegral_01_avg13, probs =  seq(0, 1, 0.1), names = TRUE)
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

SeasonIntegral_10class <- reclassify(SeasonIntegral_01_avg13, rcl = pix_categs1, filename='', include.lowest = TRUE, right = TRUE)
SeasonIntegral_10class
writeRaster(SeasonIntegral_10class, paste0(path2saveTests, "/SeasonIntegral_10class_begin.tif"), overwrite = TRUE)

#jpeg(paste0(path2saveTests, "\\SeasonIntegral_10class.jpg"))
#plot(SeasonIntegral_10class)
#dev.off()


SeasonIntegral_01_avg13_df <- as.data.table(getValues(SeasonIntegral_01_avg13))
SeasonIntegral_01_avg13_df_class10 <- as.data.frame(matrix(nrow = 0, ncol = 0))

for(i in 1:nrow(pix_categs1)){
  cond <- SeasonIntegral_01_avg13_df$V1 >= pix_categs1[i, 1] &
          SeasonIntegral_01_avg13_df$V1 <  pix_categs1[i, 2]
  
  SeasonIntegral_01_avg13_df_class10_1 <- SeasonIntegral_01_avg13_df[cond]
  SeasonIntegral_01_avg13_df_class10_1$class10 <- pix_categs1[i, 3]
  
  SeasonIntegral_01_avg13_df_class10 <- rbind(SeasonIntegral_01_avg13_df_class10, SeasonIntegral_01_avg13_df_class10_1)
  
}

#head(SeasonIntegral_01_avg13_df_class10)
#unique(SeasonIntegral_01_avg13_df_class10[SeasonIntegral_01_avg13_df_class10$V1 <= 0.1430105, ]$class10)
#sum(SeasonIntegral_01_avg13_df_class10[SeasonIntegral_01_avg13_df_class10$V1 <= 0.1430105, ]$class10 == 2)
#table(SeasonIntegral_01_avg13_df_class10$class10)
SeasonIntegral_class10_stats <- as.data.frame(SeasonIntegral_01_avg13_df_class10 %>% group_by(class10) %>% summarise_all(.funs = c(n = "length", min = "min", max = "max", mean = "mean", sd = "sd")))


#pix_categs2 <- as.data.frame(matrix(nrow = 3, ncol = 0))
#pix_categs2$from <- c(1, 5, 10)
#pix_categs2$to <- c(4, 9, 10)
#pix_categs2$becomes <- c(1, 2, 3)

pix_categs2 <- pix_categs1
pix_categs2$becomes <- c(rep(1, 4), rep(2, 5), 3)

SeasonIntegral_3class <- reclassify(SeasonIntegral_01_avg13, rcl = pix_categs2, filename='', include.lowest = TRUE, right = TRUE)
SeasonIntegral_3class
writeRaster(SeasonIntegral_3class, paste0(path2saveTests, "/SeasonIntegral_3class_begin.tif"), overwrite = TRUE)

stuff2save <- c(stuff2save, "pix_categs1", "SeasonIntegral_10class", "SeasonIntegral_class10_stats", "pix_categs2", "SeasonIntegral_3class")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))


jpeg(paste0(path2saveTests, "\\SeasonIntegral_3class.jpg"))
#plot(SeasonIntegral_3class)
par(mar = c(3, 4, 4, 6))
pal <- colorRampPalette(c("red4", "coral1", "darkseagreen1", "darkgreen"))
par(xpd = FALSE)
plot(SeasonIntegral_3class, col = pal(4), legend = FALSE) 
par(xpd = TRUE)
legend("right",
       title = "Ecosystem Dynamics",
       legend = c("Strong Negative", "Moderate Negative",
                  "Moderate Positive", "Strong Positive"),
       fill = pal(4), inset = - 0.4)
title(main = paste0("Steadiness Index: ", var2process_name), cex.main = 1.3)
dev.off()





## Combining Steadiness Index with baseline levels for Standing Biomass ####

load(file = paste0(path2tempResults, "/results_Step2.RData"), verbose = TRUE)
rm(SeasonLenght, slope_rstr, mtid_rstr)

SteadInd_SeasInt <- raster(SeasonIntegral_3class)

SteadInd_SeasInt[SteadInd_rstr == 1 & SeasonIntegral_3class == 1] <- 1   # Steadiness Index 1 (Strong Negative) - Standing Biomass 1 (low)      -> St1-low
SteadInd_SeasInt[SteadInd_rstr == 1 & SeasonIntegral_3class == 2] <- 2   # Steadiness Index 1 (Strong Negative) - Standing Biomass 2 (medium)   -> St1-medium
SteadInd_SeasInt[SteadInd_rstr == 1 & SeasonIntegral_3class == 3] <- 3   # Steadiness Index 1 (Strong Negative) - Standing Biomass 3 (high)     -> St1-high
SteadInd_SeasInt[SteadInd_rstr == 2 & SeasonIntegral_3class == 1] <- 4   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 1 (low)    -> St2-low
SteadInd_SeasInt[SteadInd_rstr == 2 & SeasonIntegral_3class == 2] <- 5   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 2 (medium) -> St2-medium
SteadInd_SeasInt[SteadInd_rstr == 2 & SeasonIntegral_3class == 3] <- 6   # Steadiness Index 2 (Moderate Negative) - Standing Biomass 3 (high)   -> St2-high
SteadInd_SeasInt[SteadInd_rstr == 3 & SeasonIntegral_3class == 1] <- 7   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 1 (low)    -> St3-low
SteadInd_SeasInt[SteadInd_rstr == 3 & SeasonIntegral_3class == 2] <- 8   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 2 (medium) -> St3-medium
SteadInd_SeasInt[SteadInd_rstr == 3 & SeasonIntegral_3class == 3] <- 9   # Steadiness Index 3 (Moderate Positive) - Standing Biomass 3 (high)   -> St3-high
SteadInd_SeasInt[SteadInd_rstr == 4 & SeasonIntegral_3class == 1] <- 10   # Steadiness Index 4 (Strong Positive) - Standing Biomass 1 (low)     -> St4-low
SteadInd_SeasInt[SteadInd_rstr == 4 & SeasonIntegral_3class == 2] <- 11   # Steadiness Index 4 (Strong Positive) - Standing Biomass 2 (medium)  -> St4-medium
SteadInd_SeasInt[SteadInd_rstr == 4 & SeasonIntegral_3class == 3] <- 12   # Steadiness Index 4 (Strong Positive) - Standing Biomass 3 (high)    -> St4-high
SteadInd_SeasInt

stuff2save <- c(stuff2save, "SteadInd_SeasInt")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step3.RData"))
writeRaster(SteadInd_SeasInt, paste0(path2saveTests, "/SteadInd_SeasInt.tif"), overwrite = TRUE)



#jpeg(paste0(path2saveTests, "\\SteadInd_SeasInt.jpg"))
#plot(SteadInd_SeasInt)
#dev.off()


# plotting for report
jpeg(paste0(path2saveTests, "\\SteadInd_SeasInt.jpg"), width = 28, height = 20, units = "cm", res = 300)
par(mar = c(9.2, 4, 4, 4), mfrow = c(1, 2))
pal <- colorRampPalette(c("brown2", "brown3", "brown4", "wheat2", "wheat3", "wheat4", "palegreen2", "palegreen3", "palegreen4", "skyblue2", "skyblue3", "skyblue4"))
categs <- c("St1-low", "St1-medium", "St1-high", "St2-low", "St2-medium", "St2-high", "St3-low", "St3-medium", "St3-high", "St4-low", "St4-medium", "St4-high")
par(xpd = FALSE)
plot(SteadInd_SeasInt, col = pal(12), legend = FALSE) 
par(xpd = TRUE)
title(main = "Steadiness Index combined with baseline levels of Standing Biomass", 
      outer = TRUE,
      #adj = 0,
      line = - 1.5,
      cex.main = 1.3)
legend("bottom",
       ncol = 4,
       legend = categs,
       fill = pal(12), inset = - 0.25)
mtext("St1: Strong Neg; St2: Medium Neg; St3: Medium Pos; St4: Strong Pos", 
      side = 1, line = 7, 
      #at = 5,
      adj = 0,
      cex = 0.8)
if((length(time) - 2) == dim(SeasonIntegral)[3]){
  y2plot <- time[-c(1,length(time))]
  y2plot <- y2plot[yrs][c(1,length(yrs))]
}else if((length(time)) == dim(SeasonIntegral)[3]){
  y2plot <- time[yrs][c(1,length(yrs))]
}else{
  y2plot <- ""
}

y2plot <- paste0("Baseline years: average of ", paste(y2plot, collapse = "-"))
mtext(y2plot, 
      side = 1, line = 8, 
      #at = 5,
      adj = 0,
      cex = 0.8)

#dev.off()


## Some statistics
cont_table <- as.data.frame(table(getValues(SteadInd_SeasInt)))
cont_table$Var1 <- categs
names(cont_table)[1] <- "SteadInd_StandBiomass_categs"
barplot(cont_table$Freq, names.arg = cont_table$SteadInd_StandBiomass_categs, las = 3, axis.lty = 1,
        ylab = "Number of pixels per category", 
        #main = "Steadiness Index combined with baseline levels \nof Standing Biomass",
        col = pal(12))
#abline(0, 0)
dev.off()



