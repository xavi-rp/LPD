library(ncdf4)
library(fields)
nc <- nc_open("E:\\rotllxa\\Documents\\phenolo_res\\europe.nc")
nc
#str(nc)

lon <- ncvar_get(nc, "lon")
head(lon)
summary(lon)
lat <- ncvar_get(nc, "lat")
head(lat)
summary(lat)

time <- ncvar_get(nc, "time")
range(time)

SeasonLenght <- ncvar_get(nc, "SeasonLenght")
head(SeasonLenght)
summary(SeasonLenght)
dim(SeasonLenght)

SeasonLenght_y1 <- SeasonLenght[, , 2] # year 1
dim(SeasonLenght_y1)

graphics.off()
jpeg("E:\\rotllxa\\LPD\\kk\\SeasonLenght_y1.jpg")
#image.plot(SeasonLenght_y1[, seq(4197,1,-1)])
image.plot(lon, rev(lat), SeasonLenght_y1[, seq(4197,1,-1)])
#map("world", add=T)
dev.off()


# Dealing with NoData
dim(SeasonLenght_y1)
is.matrix(SeasonLenght_y1)

sum(SeasonLenght_y1 == 0)
sum(is.na(SeasonLenght_y1))
SeasonLenght_y1_NA <- SeasonLenght_y1
SeasonLenght_y1_NA[SeasonLenght_y1_NA <= -9.00e+18] <- NA
sum(is.na(SeasonLenght_y1_NA))
summary(as.vector((SeasonLenght_y1_NA / 1000000000) / 60 * 60 * 24))

jpeg("E:\\rotllxa\\LPD\\kk\\SeasonLenght_y1_NA.jpg")
image.plot(lon, rev(lat), SeasonLenght_y1_NA[, seq(4197,1,-1)])
dev.off()







nc_close(nc)