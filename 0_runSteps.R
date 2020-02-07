## This is a script to sequentially run the steps ####


## Settings (step 00)

if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}

cat("Starting time: ", as.character(Sys.time()), "\n")



## Reading nc files (step 01)
#source(paste0(path2project, "/LPD/01_read_nc.R"))
#cat("Step 01 finished at: ", as.character(Sys.time()), "\n")



## Preparing old data sets (step 011)
source(paste0(path2project, "/LPD/011_PrepOldData2test.R"))
cat("Step 011 finished at: ", as.character(Sys.time()), "\n")



## Calculating Steadiness Index (step 02)
source(paste0(path2project, "/LPD/02_steadiness_index.R"))
cat("Step 02 finished at: ", as.character(Sys.time()), "\n")



## Calculating Base Line (step 03)
source(paste0(path2project, "/LPD/03_BaselineLevels.R"))
cat("Step 03 finished at: ", as.character(Sys.time()), "\n")



## Standing Biomass State Change (step 04)
source(paste0(path2project, "/LPD/04_StandBiom_Change.R"))
cat("Step 04 finished at: ", as.character(Sys.time()), "\n")



## Land-Productivity Long Term Change Map (step 05)
source(paste0(path2project, "/LPD/05_LandProductivity_change.R"))
cat("Step 05 finished at: ", as.character(Sys.time()), "\n")



