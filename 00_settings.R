
#### Packages and Settings #### 
#rm(list = ls())

## Packages ####
library(ncdf4)
library(fields)
library(raster)
library(rgdal)
library(dplyr)
library(data.table)
library(virtualspecies)


## Settings ####
path2project <- "E:\\rotllxa\\LPD"
path2data <- "E:\\rotllxa\\Documents\\phenolo_res"
path2saveTests <- paste0(path2project, "/kk")
path2tempResults <- paste0(path2project, "/temp_results")


#nc file
nc_file <- "europe.nc"


## Sourcing functions ####
source(paste0(path2project, "/LPD/001_functions.R"))



