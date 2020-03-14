
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
library(parallel)
library(rrcov)
library(lattice)
library(latticeExtra)
library(devtools)
library(tidyr)
library(tidyverse)


## Defining machine ####
if(Sys.info()[4] == "D01RI1700371"){
  Iam <- "PCxavi"
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  Iam <- "Server05wad"
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  Iam <- "MacXavi"
}else{
  stop("Define your machine before to run LPD")
}


## Settings ####
if(Iam == "PCxavi"){
  path2project <- "E:\\rotllxa\\LPD"
  path2data <- "E:\\rotllxa\\Documents\\phenolo_res"
  path2old_data <- "X:\\RS\\DER_TS\\PHENOL\\SPOT\\INDICIES/"
  path2saveTests <- paste0(path2project, "/kk")
  path2tempResults <- paste0(path2project, "/temp_results")
}else if(Iam == "Server05wad"){
  path2project <- "/home/rotllxa/LPD"
  path2data <- "/home/rotllxa/phenolo_res"
  path2old_data <- "/wad-1/RS/DER_TS/PHENOL/SPOT/INDICIES/"
  path2saveTests <- paste0(path2project, "/kk")
  path2tempResults <- paste0(path2project, "/temp_results")
}else if(Iam == "MacXavi"){
  path2project <- "/Users/xavi_rp/Documents/D6_LPD/"
  path2data <- ""
  path2old_data <- paste0(path2project, "old_data_sets")
  path2saveTests <- paste0(path2project, "kk")
  path2tempResults <- paste0(path2project, "temp_results")
}


# nc file
nc_file <- "europe.nc"

## Setting the data set to use ####
#var2process <- SeasonLenght
#var2process_name <- "SeasonLenght"
var2process_name <- "SeasonIntegral"
var2process_name <- "StandingBiomass_OldData" # This is an old data set which might be the one used by Ivits in 2013 (SB = MI)

if(grepl("OldData", var2process_name)) vrbls_lst <- c("sbd", "sed", "si", "sl", "mi")



# Number of available cores for parallel processing
avlble_cors <- detectCores()
if(avlble_cors == 4){
  cors2use <- 3
}else{
  cors2use <- ceiling(avlble_cors / 1.5)
  cors2use <- 61
}

## Sourcing functions ####
source(paste0(path2project, "/LPD/001_functions.R"))


## PCA settings ####
nPCs <- 4
nPCs <- 3
nPCs <- 4
#A threshhold on a certain minimum variance achieved coud be set here instead of the number of PCs to check


# plotting for report
rning_plts <- "n"
rning_plts <- "y"


