#source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


#### Base Line Calculation (Standing Biomass)  ####


rm(list = ls()[!ls() %in% c("path2data", "path2saveTests", "path2tempResults")])


## Reading in data (Season_Integral) ####
load(paste0(path2tempResults, "/Season_Integral_EndStep01.RData"), verbose = TRUE)
summary(SeasonIntegral, na.rm = T)


## normalising data set (0-1)
do_normalize <- "no"

if(grepl("^[Yy]", do_normalize)){
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  
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
jpeg(paste0(path2saveTests, "\\SeasonIntegral_avg13.jpg"))
plot(SeasonIntegral_01_avg13)
dev.off()

chk_avg <- round(mean(SeasonIntegral_01[2900, 1900][1:3]), 4) == round(SeasonIntegral_01_avg13[2900, 1900], 4) # has to be TRUE
if(chk_avg != TRUE) stop("Something wrong in the averaging process")


## classification of averaged pixels

# Method found in 'zonal10class.pro'; However, it's not reclassifying into groups with similar number of pixels, 
#                                     but groups with regular range of values. It's not an ISODATA methodology
rg_SI <- range(getValues(SeasonIntegral_01_avg13), na.rm = TRUE)
thr <- (rg_SI[2] - rg_SI[1]) / 10
thr

rg_SI[1] 
rg_SI[1] + 1 * thr
rg_SI[1] + 2 * thr
rg_SI[1] + 3 * thr



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
table(SeasonIntegral_01_avg13_df_class10$class10)
as.data.frame(SeasonIntegral_01_avg13_df_class10 %>% group_by(class10) %>% summarise_all(.funs = c("min", "max", "mean", "sd")))


#

#pix_categs2 <- as.data.frame(matrix(nrow = 3, ncol = 0))
#pix_categs2$from <- c(1, 5, 10)
#pix_categs2$to <- c(4, 9, 10)
#pix_categs2$becomes <- c(1, 2, 3)

pix_categs2 <- pix_categs1
pix_categs2$becomes <- c(rep(1, 4), rep(2, 5), 3)

SeasonIntegral_3class <- reclassify(SeasonIntegral_01_avg13, rcl = pix_categs2, filename='', include.lowest = TRUE, right = TRUE)
SeasonIntegral_3class
writeRaster(SeasonIntegral_3class, paste0(path2saveTests, "/SeasonIntegral_3class_begin.tif"), overwrite = TRUE)

jpeg(paste0(path2saveTests, "\\SeasonIntegral_3class.jpg"))
plot(SeasonIntegral_3class)
dev.off()



