#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in 'Phenolo' data
nc <- nc_open(paste0(path2data, "/", nc_file))
nc
#str(nc)

lon <- ncvar_get(nc, "lon")
head(lon)
summary(lon)
lat <- ncvar_get(nc, "lat")
head(lat)
summary(lat)

time <- ncvar_get(nc, "time")
range(time)  # However, 1998 and 2013 (first and last) are not complete (removing...)


SeasonLenght <- ncvar_get(nc, "SeasonLenght")
SeasonLenght <- SeasonLenght[, , - c(1, length(time))]
dim(SeasonLenght)

StartWeek <- ncvar_get(nc, "StartWeek")
Endweek <- ncvar_get(nc, "Endweek")
SeasonIntegral <- ncvar_get(nc, "SeasonIntegral")
SeasonPermanentIntegral <- ncvar_get(nc, "SeasonPermanentIntegral")
CycleFraction <- ncvar_get(nc, "CycleFraction")
#ActiveFractionIntegral <- ncvar_get(nc, "ActiveFractionIntegral")
#lapply(list(StartWeek, Endweek, SeasonIntegral, SeasonPermanentIntegral, CycleFraction, ActiveFractionIntegral), dim)
lapply(list(StartWeek, Endweek, SeasonIntegral, SeasonPermanentIntegral, CycleFraction), dim)

StartWeek <- StartWeek[, , - c(1, length(time))]
Endweek <- Endweek[, , - c(1, length(time))]
SeasonIntegral <- SeasonIntegral[, , - c(1, length(time))]
SeasonPermanentIntegral <- SeasonPermanentIntegral[, , - c(1, length(time))]
CycleFraction <- CycleFraction[, , - c(1, length(time))]
#ActiveFractionIntegral <- ActiveFractionIntegral[, , - c(1, length(time))]
#lapply(list(StartWeek, Endweek, SeasonIntegral, SeasonPermanentIntegral, CycleFraction, ActiveFractionIntegral), dim)
lapply(list(StartWeek, Endweek, SeasonIntegral, SeasonPermanentIntegral, CycleFraction), dim)


head(SeasonLenght)
summary(as.vector(SeasonLenght[, , 1]))
summary(as.vector(StartWeek[, , 1]))
summary(as.vector(Endweek[, , 1]))
summary(as.vector(SeasonPermanentIntegral[, , 1]))
summary(as.vector(CycleFraction[, , 1]))
#summary(as.vector(ActiveFractionIntegral[, , 1]))
summary(as.vector(SeasonIntegral[, , 1]))
summary(as.vector(SeasonIntegral[, , 14]))
(sum(as.vector(SeasonIntegral[, , 14]) < 0, na.rm = TRUE)/length(as.vector(SeasonIntegral[, , 14]))) * 100  # 0.0037% negatives (-1, 0)


## plotting year 1 to check
SeasonLenght_y1 <- SeasonLenght[, , 1] # year 1999
dim(SeasonLenght_y1)
range(as.vector(SeasonLenght_y1), na.rm = T)

graphics.off()
jpeg(paste0(path2saveTests, "\\SeasonLenght_y1.jpg"))
image.plot(lon, rev(lat), (SeasonLenght_y1[, seq(4197,1,-1)] / 8.64e+13))
#image.plot(lon, rev(lat), StartWeek[,,2][, seq(4197,1,-1)])
#map("world", add=T)
dev.off()

jpeg(paste0(path2saveTests, "\\StartWeek_y1.jpg"))
StartWeek_y1 <- StartWeek[, , 1]
image.plot(lon, rev(lat), (StartWeek_y1[, seq(4197,1,-1)]))
dev.off()




## Dealing with NoData
dim(SeasonLenght_y1)
is.matrix(SeasonLenght_y1)

# for the frist year
sum(SeasonLenght_y1 == 0)
sum(is.na(SeasonLenght_y1))
SeasonLenght_y1_NA <- SeasonLenght_y1
SeasonLenght_y1_NA[SeasonLenght_y1_NA <= -9.00e+18] <- NA
sum(is.na(SeasonLenght_y1_NA))
summary(as.vector(SeasonLenght_y1_NA))
summary((as.vector(SeasonLenght_y1_NA) / 8.64e+13))
quantile((as.vector(SeasonLenght_y1_NA) / 8.64e+13), 0.95, na.rm = TRUE)
sum((as.vector(SeasonLenght_y1_NA[!is.na(SeasonLenght_y1_NA)]) / 8.64e+13) > 365)

# saving raster
#r1 <- raster((t(SeasonLenght_y1[, ]) / 8.64e+13))
#extent(r1) <- c(range(lon),  range(lat))
#r1
#writeRaster(r1, paste0(path2saveTests, "/SeasonLenght_y1.tif"), overwrite = TRUE)

r <- raster((t(SeasonLenght_y1_NA[, ]) / 8.64e+13))
extent(r) <- c(range(lon),  range(lat))
r
writeRaster(r, paste0(path2saveTests, "/SeasonLenght_y1_NA.tif"), overwrite = TRUE)


# for the time series
sum(SeasonLenght == 0)
sum(is.na(SeasonLenght))

SeasonLenght_NA <- SeasonLenght
SeasonLenght_NA[SeasonLenght_NA <= -9.00e+18] <- NA

sum(is.na(SeasonLenght_NA))
summary(as.vector(SeasonLenght_NA))
summary((as.vector(SeasonLenght_NA) / 8.64e+13))
quantile((as.vector(SeasonLenght_NA) / 8.64e+13), 0.95, na.rm = TRUE)
dim(SeasonLenght_NA)


# from nanoseconds(?) to days
SeasonLenght_NA_days <- SeasonLenght_NA / 8.64e+13
summary(as.vector(SeasonLenght_NA_days))

# For SeasonLength: change 0 (Ocean or some kind of unfeasibility) to NA
SeasonLenght_NA_days[SeasonLenght_NA_days == 0] <- NA


# saving results
SeasonLenght <- SeasonLenght_NA_days

r2 <- raster(t(SeasonLenght[, , 1]))
extent(r2) <- c(range(lon),  range(lat))
writeRaster(r2, paste0(path2saveTests, "/SeasonLenght_y1_EndStep01.tif"), overwrite = TRUE)

jpeg(paste0(path2saveTests, "\\SeasonLenght_y1_EndStep01.jpg"), width = 480, height = 480)
plot(r2)
dev.off()


save(SeasonLenght, lon, lat, time, file = paste0(path2tempResults, "/season_length_EndStep01.RData"))

save(StartWeek, lon, lat, time, file = paste0(path2tempResults, "/StartWeek_EndStep01.RData"))
save(Endweek, lon, lat, time, file = paste0(path2tempResults, "/Endweek_EndStep01.RData"))
save(SeasonIntegral, lon, lat, time, file = paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"))
save(SeasonPermanentIntegral, lon, lat, time, file = paste0(path2tempResults, "/SeasonPermanentIntegral_EndStep01.RData"))
save(CycleFraction, lon, lat, time, file = paste0(path2tempResults, "/CycleFraction_EndStep01.RData"))
#save(ActiveFractionIntegral, lon, lat, time, file = paste0(path2tempResults, "/ActiveFractionIntegral_EndStep01.RData"))





nc_close(nc)





