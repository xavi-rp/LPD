
#### Steadiness Index I: Tendency of the change ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}


## Setting the data set to use ####
#var2process <- SeasonLenght
#var2process_name <- "SeasonLenght"
var2process_name <- "SeasonIntegral"
var2process_name <- "SeasonIntegral_OldData"



## Reading in data (season integral) ####

if(grepl("OldData", var2process_name)){
  load(paste0(path2tempResults, "/OldDataSets_EndStep011.RData"), verbose = TRUE)
  assign(var2process_name, si)
  var2process <- SeasonIntegral_OldData     
}else{
  #load(paste0(path2tempResults, "/season_length_EndStep01.RData"), verbose = TRUE)
  load(paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"), verbose = TRUE)
  var2process <- SeasonIntegral
        
  ## Array to raster brick 
  var2process <- brick(var2process)
  var2process <- t(var2process)
  extent(var2process) <- c(range(lon),  range(lat))
  var2process
} 

summary(getValues(var2process$X1999))
kk <- getValues(var2process$X1999)
length(kk) #591655680
sum(kk < -32760) #28
sum(kk < -365) #159240
sum(kk < -100) #324500
sum(kk < 0) #685488
sum(kk == 0) #416685362
sum(kk > 0) #174284830





#jpeg(paste0(path2saveTests, "/", var2process_name, ".jpg"))
#plot(var2process)
#dev.off()

years <- nlayers(var2process)   #1999 - 2013



## Fitting a linear regression and getting the slope ####

yrs <- 1:years

# without parallelization                     # It takes 3.5 hours
#slope_rstr <- calc(var2process, slp_lm)
#Sys.time()
#slope_rstr


#with parallelization                         # It takes 1 hour (0.5h after 0 to NA)
Sys.getenv("NUMBER_OF_PROCESSORS")

t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
#slope_rstr_SL <- clusterR(var2process, calc, args = list(fun = slp_lm), export = "yrs")
slope_rstr <- clusterR(var2process, calc, args = list(fun = slp_lm), export = "yrs")
endCluster()
print("Slope calculated in: ")
print(Sys.time() - t0)


stuff2save <- c(var2process_name, "slope_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step2.RData"))
writeRaster(slope_rstr, paste0(path2saveTests, "/slope_raster.tif"), overwrite = TRUE)


# some tests...
rning_tsts <- "y"
rning_tsts <- "n"
if(rning_tsts == "y"){
  summary(getValues(slope_rstr))
  quantile(getValues(slope_rstr), c(0.05, 0.95), na.rm = TRUE)
  quantile(getValues(slope_rstr), c(0.01, 0.99), na.rm = TRUE)
  quantile(getValues(slope_rstr), c(0.001, 0.999), na.rm = TRUE)
  quantile(getValues(slope_rstr), c(0.1835), na.rm = TRUE)
  quantile(getValues(slope_rstr), seq(0, 1, 0.1), na.rm = TRUE)
  length(getValues(slope_rstr))
  sum(getValues(slope_rstr) < -31.25, na.rm = T)
  sum(getValues(slope_rstr) > 11.3, na.rm = T)
  sum(getValues(slope_rstr) < -11.9, na.rm = T)
  sum(getValues(slope_rstr) == 0, na.rm = T)
  kk1 <- getValues(slope_rstr)
  sum(kk1 > 0)  #132861034        
  sum(kk1 == 0) #416678934    
  sum(kk1 < 0)  # 42115712    
  
}

#library(devtools)
#install_github("danlwarren/ENMTools", force = TRUE)
#library(ENMTools)
#ENMTools::raster.overlap(slope_rstr, slope_rstr, verbose = FALSE)
#ENMTools::raster.overlap(slope_rstr, slope_rstr1, verbose = FALSE)

#https://gis.stackexchange.com/questions/265717/statistical-comparison-between-different-rasters-using-r
#RMSE <- function(x, y) { sqrt(mean((x - y)^2, na.rm = TRUE)) } 
#RMSE(getValues(slope_rstr), getValues(slope_rstr1))
#sum(is.na(getValues(slope_rstr)))
#sum(is.na(getValues(slope_rstr1)))
#r_diff <- slope_rstr1 - slope_rstr
#writeRaster(r_diff, paste0(path2saveTests, "/r_diff.tif"), overwrite = TRUE)




# plotting for report
rning_plts <- "y"
rning_plts <- "n"
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\slope_rstr.jpg"))
  #plot(slope_rstr)
  slope_rstr_vls <- getValues(slope_rstr)
  slope_rstr2plot <- slope_rstr
  slope_rstr2plot[slope_rstr2plot < 0 ] <- -1
  slope_rstr2plot[slope_rstr2plot > 0 ] <- 1
  slope_rstr2plot[slope_rstr2plot == 0 ] <- 0
  
  par(mar = c(3, 4, 4, 4.5))
  pal <- colorRampPalette(c("coral1", "yellow", "palegreen3"))
  par(xpd = FALSE)
  plot(slope_rstr2plot, col = pal(3), legend = FALSE) 
  par(xpd = TRUE)
  legend("right",
         legend = c("Negative", "Zero",
                    "Positive"),
         fill = pal(3), inset = - 0.3)
  title(main = "Tendency of Change (Slope)", cex.main = 1.3)
  
  dev.off()
}


## Computing net change: MTID (Multi Temporal Image Differencing) ####

#years1 <- years
#years <- years1
#years <- 14


#with parallelization           
t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
mtid_rstr <- clusterR(var2process, calc, args = list(fun = mtid_function), export = "years")
endCluster()
print("MTID calculated in: ")
print(Sys.time() - t0)

summary(getValues(mtid_rstr))

stuff2save <- c(stuff2save, "mtid_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step2.RData"))
writeRaster(mtid_rstr, paste0(path2saveTests, "/mtid_raster.tif"), overwrite = TRUE)


# plotting for report
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\mtid_rstr.jpg"))
  #plot(slope_rstr)
  mtid_rstr_rstr2plot <- mtid_rstr
  mtid_rstr_rstr2plot[mtid_rstr_rstr2plot < 0 ] <- -1
  mtid_rstr_rstr2plot[mtid_rstr_rstr2plot > 0 ] <- 1
  mtid_rstr_rstr2plot[mtid_rstr_rstr2plot == 0 ] <- 0
  
  par(mar = c(3, 4, 4, 4.5))
  pal <- colorRampPalette(c("tomato3", "yellow", "seagreen4"))
  par(xpd = FALSE)
  plot(mtid_rstr_rstr2plot, col = pal(3), legend = FALSE) 
  par(xpd = TRUE)
  legend("right",
         legend = c("Negative", "Zero",
                    "Positive"),
         fill = pal(3), inset = - 0.3)
  title(main = "Net Change (MTID)", cex.main = 1.3)
  
  dev.off()
}


## Calculating steadiness classes ####

SteadInd_rstr <- raster(mtid_rstr)

t0 <- Sys.time()
SteadInd_rstr[slope_rstr < 0 & mtid_rstr > 0] <- 1   # strong negative ecosystem dynamics
SteadInd_rstr[slope_rstr < 0 & mtid_rstr < 0] <- 2   # moderate negative ecoystem dynamics
SteadInd_rstr[slope_rstr > 0 & mtid_rstr < 0] <- 3   # moderate positive ecosystem dynamics
SteadInd_rstr[slope_rstr > 0 & mtid_rstr > 0] <- 4   # strong positive ecosystem dynamics
Sys.time() - t0

# saving
#crs(SteadInd_rstr) <- CRS("+init=EPSG:4326")
stuff2save <- c(stuff2save, "SteadInd_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step2.RData"))
writeRaster(SteadInd_rstr, paste0(path2saveTests, "/SteadInd_raster.tif"), overwrite = TRUE)


# plotting for report
if(rning_plts == "y"){
  jpeg(paste0(path2saveTests, "\\SteadInd_rstr.jpg"))#, width = 15, height = 15, units = "cm", res = 300)
  #plot(SteadInd_rstr)
  par(mar = c(3, 4, 4, 6))
  pal <- colorRampPalette(c("red4", "coral1", "darkseagreen1", "darkgreen"))
  par(xpd = FALSE)
  plot(SteadInd_rstr, col = pal(4), legend = FALSE) 
  par(xpd = TRUE)
  legend("right",
         title = "Ecosystem Dynamics",
         legend = c("Strong Negative", "Moderate Negative",
                    "Moderate Positive", "Strong Positive"),
         fill = pal(4), inset = - 0.4)
  title(main = paste0("Steadiness Index: ", var2process_name), cex.main = 1.3)
  
  dev.off()
}
##






