## This is a script to sequentially run the steps ####
#source("/home/rotllxa/LPD/LPD/0_runSteps.R")
#source("E:/rotllxa/LPD/LPD/0_runSteps.R")

## Settings (step 00)

if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}

cat("Starting time: ", as.character(Sys.time()), "\n")

steps2run <- c("step 06", "step 07")
steps2run <- c("step 07", "step 08")
steps2run <- c("step 08")
steps2run <- c("step 011", "step 02", "step 03", "step 04")
steps2run <- c(#"step 01", 
  "step 011", 
  "step 02", "step 03", "step 04", "step 05", 
  "step 06", "step 07", "step 08", "step 09", "step 10", "step 11")



## Reading nc files (step 01)
if("step 01" %in% steps2run){
  source(paste0(path2project, "/LPD/01_read_nc.R"))
  cat("Step 01 finished at: ", as.character(Sys.time()), "\n")
}



## Preparing old data sets (step 011)
if("step 011" %in% steps2run){
  source(paste0(path2project, "/LPD/011_PrepOldData2test.R"))
 cat("Step 011 finished at: ", as.character(Sys.time()), "\n")
}



## Calculating Steadiness Index (step 02)
if("step 02" %in% steps2run){
  source(paste0(path2project, "/LPD/02_steadiness_index.R"))
  cat("Step 02 finished at: ", as.character(Sys.time()), "\n")
}


## Calculating Baseline (step 03)
if("step 03" %in% steps2run){
  source(paste0(path2project, "/LPD/03_BaselineLevels.R"))
  cat("Step 03 finished at: ", as.character(Sys.time()), "\n")
}


## Standing Biomass State Change (step 04)
if("step 04" %in% steps2run){
  source(paste0(path2project, "/LPD/04_StandBiom_Change.R"))
  cat("Step 04 finished at: ", as.character(Sys.time()), "\n")
}


## Land-Productivity Long Term Change Map (step 05)
if("step 05" %in% steps2run){
  source(paste0(path2project, "/LPD/05_LandProductivity_change.R"))
  cat("Step 05 finished at: ", as.character(Sys.time()), "\n")
}


## Averaging Phenolo Variables and Multicollinearity (step 06)
if("step 06" %in% steps2run){
  source(paste0(path2project, "/LPD/06_PhenoloVars_multicollin.R"))
  cat("Step 06 finished at: ", as.character(Sys.time()), "\n")
}


## First PCA - Screening (step 07)
if("step 07" %in% steps2run){
  source(paste0(path2project, "/LPD/07_First_PCA_Screening.R"))
  cat("Step 07 finished at: ", as.character(Sys.time()), "\n")
}


## Final PCA (step 08)
if("step 08" %in% steps2run){
  source(paste0(path2project, "/LPD/08_Final_PCA.R"))
  cat("Step 08 finished at: ", as.character(Sys.time()), "\n")
}


## Clustering EFTs (step 09)
if("step 09" %in% steps2run){
  source(paste0(path2project, "/LPD/09_clustering_EFTs.R"))
  cat("Step 09 finished at: ", as.character(Sys.time()), "\n")
}


## Local Net Productivity Scaling (step 10)
if("step 10" %in% steps2run){
  source(paste0(path2project, "/LPD/10_LocalNetProductivityScaling.R"))
  cat("Step 10 finished at: ", as.character(Sys.time()), "\n")
}

## Combined Assessment (step 11)
if("step 11" %in% steps2run){
  source(paste0(path2project, "/LPD/11_CombinedAssessment.R"))
  cat("Step 11 finished at: ", as.character(Sys.time()), "\n")
}


