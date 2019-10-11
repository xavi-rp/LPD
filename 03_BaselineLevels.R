#source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


#### Base Line Calculation (Standing Biomass)  ####


rm(list = ls()[!ls() %in% c("path2data", "path2saveTests", "path2tempResults")])


## Reading in data (season length) ####
load(paste0(path2tempResults, "/Season_Integral_EndStep01.RData"), verbose = TRUE)
summary(SeasonIntegral, na.rm = T)


## normalising data set (0-1)
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

SeasonIntegral_01 <- range01(SeasonIntegral, na.rm = TRUE)
dim(SeasonIntegral_01)
summary(as.vector(SeasonIntegral_01[, , 1]))

hist(SeasonIntegral_01[, , 1], breaks=10)
hist(SeasonIntegral_01, breaks=10)
hist(SeasonIntegral, breaks=10)

sum(is.na(SeasonIntegral_01[ , , 3]))


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


## averaging 
mean_years_function <- function(x, na.rm = TRUE){ mean(x[yrs]) }  #where x is the data set and yrs is a vector with the years to be averaged (e.g. yrs = c(1:3))


#with parallelization           
t0 <- Sys.time()
beginCluster()   # it uses n - 1 clusters
yrs <- 1:3
SeasonIntegral_01_avg13 <- clusterR(SeasonIntegral_01, calc, args = list(fun = mean_years_function), export = "yrs")
endCluster()
Sys.time() - t0


#some checks...
jpeg(paste0(path2saveTests, "\\SeasonIntegral_01_avg13.jpg"))
plot(SeasonIntegral_01_avg13)
dev.off()

round(mean(SeasonIntegral_01[2900, 1900][1:3]), 4) == round(SeasonIntegral_01_avg13[2900, 1900], 4)



## 

