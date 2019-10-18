
#### Steadiness Index I: Tendency of the change ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data (season length) ####
load(paste0(path2tempResults, "/season_length_EndStep01.RData"), verbose = TRUE)

dim(SeasonLenght)
is.array(SeasonLenght)



## Array to raster brick ####

SeasonLenght <- brick(SeasonLenght)
SeasonLenght <- t(SeasonLenght)
extent(SeasonLenght) <- c(range(lon),  range(lat))
SeasonLenght

#jpeg(paste0(path2saveTests, "\\SeasonLenght_y1.jpg"))
#plot(SeasonLenght)
#dev.off()

years <- nlayers(SeasonLenght)   #1999 - 2013



## Fitting a linear regression and getting the slope ####

yrs <- 1:years

# without parallelization                     # It takes 3.5 hours
#slope_rstr <- calc(SeasonLenght, slp_lm)
#Sys.time()
#slope_rstr


#with parallelization                         # It takes 1 hour (0.5h after 0 to NA)
Sys.getenv("NUMBER_OF_PROCESSORS")

t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
slope_rstr <- clusterR(SeasonLenght, calc, args = list(fun = slp_lm), export = "yrs")
endCluster()
Sys.time() - t0


stuff2save <- c("SeasonLenght", "slope_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step2.RData"))
writeRaster(slope_rstr, paste0(path2saveTests, "/slope_raster.tif"), overwrite = TRUE)


# some tests...
summary(getValues(slope_rstr))
quantile(getValues(slope_rstr), c(0.05, 0.95), na.rm = TRUE)
quantile(getValues(slope_rstr), c(0.01, 0.99), na.rm = TRUE)
length(getValues(slope_rstr))
sum(getValues(slope_rstr) < -31.25, na.rm = T)
sum(getValues(slope_rstr) > 11.3, na.rm = T)
sum(getValues(slope_rstr) < -11.9, na.rm = T)
sum(getValues(slope_rstr) == 0, na.rm = T)

#library(devtools)
#install_github("danlwarren/ENMTools", force = TRUE)
#library(ENMTools)
#ENMTools::raster.overlap(slope_rstr, slope_rstr, verbose = FALSE)
#ENMTools::raster.overlap(slope_rstr, slope_rstr1, verbose = FALSE)

#https://gis.stackexchange.com/questions/265717/statistical-comparison-between-different-rasters-using-r
#RMSE <- function(x, y) { sqrt(mean((x - y)^2, na.rm = TRUE)) } 
RMSE(getValues(slope_rstr), getValues(slope_rstr1))
sum(is.na(getValues(slope_rstr)))
sum(is.na(getValues(slope_rstr1)))
r_diff <- slope_rstr1 - slope_rstr
writeRaster(r_diff, paste0(path2saveTests, "/r_diff.tif"), overwrite = TRUE)




# plotting for checking
jpeg(paste0(path2saveTests, "\\slope_rstr.jpg"))
plot(slope_rstr)
#cuts <- seq(-430, 337, length.out = 100)
#pal <- colorRampPalette(c("red", "blue"))
#plot(slope_rstr, breaks = cuts, col = pal(100)) #plot with defined breaks
dev.off()




## Computing net change: MTID (Multi Temporal Image Differencing) ####

#years1 <- years
#years <- years1
#years <- 14


#with parallelization           
t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
mtid_rstr <- clusterR(SeasonLenght, calc, args = list(fun = mtid_function), export = "years")
endCluster()
Sys.time() - t0

summary(getValues(mtid_rstr))

stuff2save <- c(stuff2save, "mtid_rstr")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step2.RData"))
writeRaster(mtid_rstr, paste0(path2saveTests, "/mtid_raster.tif"), overwrite = TRUE)




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

# plotting
jpeg(paste0(path2saveTests, "\\SteadInd_rstr.jpg"), width = 15, height = 15, units = "cm", res = 300)
#plot(SteadInd_rstr)
par(mar = c(9, 4, 4, 0))
pal <- colorRampPalette(c("red4", "coral1", "darkseagreen1", "darkgreen"))
par(xpd = FALSE)
plot(SteadInd_rstr, col = pal(4), legend = FALSE) 
par(xpd = TRUE)
legend("bottom",
       legend = c("Strong negative ecosystem dynamics", "Moderate negative ecoystem dynamics",
                  "Moderate positive ecosystem dynamics", "Strong positive ecosystem dynamics"),
       fill = pal(4), inset = -0.5)
title(main = "Steadiness Index (classes) : SeasonLength", cex.main = 1.3)


dev.off()



##






