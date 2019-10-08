#source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


#### Steadiness Index I: Tendency of the change ####


rm(list = ls()[!ls() %in% c("path2data", "path2saveTests", "path2tempResults")])

## Reading in data (season length)
load(paste0(path2tempResults, "/season_length_days.RData"), verbose = TRUE)

dim(SeasonLenght)
is.array(SeasonLenght)





## Array to raster brick

SeasonLenght <- brick(SeasonLenght)
SeasonLenght <- t(SeasonLenght)
extent(SeasonLenght) <- c(range(lon),  range(lat))
SeasonLenght

years <- nlayers(SeasonLenght)   #1999 - 2013


## Fitting a linear regression and getting the slope

yrs <- 1:years

slp_lm <- function(x){ if (is.na(x[1])){ NA } else { lm(x ~ yrs)$coefficients[2] }}

# without parallelization                     # It takes 3.5 hours
#slope_rstr <- calc(SeasonLenght, slp_lm)
#Sys.time()
#slope_rstr


#with parallelization                         # It takes 1 hour
Sys.getenv("NUMBER_OF_PROCESSORS")

t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
slope_rstr <- clusterR(SeasonLenght, calc, args = list(fun = slp_lm), export = "yrs")
endCluster()
Sys.time() - t0


save(slope_rstr, file = paste0(path2tempResults, "/slope_raster.RData"))
writeRaster(slope_rstr, paste0(path2saveTests, "/slope_raster.tif"), overwrite = TRUE)


# some tests...
summary(getValues(slope_rstr))
quantile(getValues(slope_rstr), c(0.05, 0.95), na.rm = TRUE)
quantile(getValues(slope_rstr), c(0.01, 0.99), na.rm = TRUE)
length(getValues(slope_rstr))
sum(getValues(slope_rstr) < -31.25, na.rm = T)
sum(getValues(slope_rstr) > 11.3, na.rm = T)
sum(getValues(slope_rstr) < -11.9, na.rm = T)



# plotting for checking
jpeg(paste0(path2saveTests, "\\slope_rstr.jpg"))
plot(slope_rstr)
dev.off()



## 









