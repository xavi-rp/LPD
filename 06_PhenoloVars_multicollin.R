

#### Phenolo Variables: Multicollinearity  ####

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
if(Sys.info()[4] == "D01RI1700371"){
  source("E:\\rotllxa\\LPD\\LPD/00_settings.R")
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  source("/home/rotllxa/LPD/LPD/00_settings.R")
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  source("/Users/xavi_rp/Documents/D6_LPD/LPD/00_settings.R")
}else{
  stop("Define your machine before to run LPD")
}


## Reading in data (Phenolo variables) ####

if(grepl("OldData", var2process_name)){
  mi_clean <- stack(paste0(path2tempResults, "/mi_clean.tif"))
  si_clean <- stack(paste0(path2tempResults, "/si_clean.tif"))
  sl_clean <- stack(paste0(path2tempResults, "/sl_clean.tif"))
  sbd_clean <- stack(paste0(path2tempResults, "/sbd_clean.tif"))
  #sbd_clean <- stack(paste0(path2tempResults, "/sbd_crop.tif"))
  sed_clean <- stack(paste0(path2tempResults, "/sed_clean.tif"))
  ls_vr <- length(vrbls_lst)

}else{
  
  ls_ini <- list()
  
  ls_ini[[1]] <- lapply(paste0(path2tempResults, "/test01/season_length_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  ls_ini[[2]] <- lapply(paste0(path2tempResults, "/test01/StartWeek_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  ls_ini[[3]] <- lapply(paste0(path2tempResults, "/test01/Endweek_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  ls_ini[[4]] <- lapply(paste0(path2tempResults, "/test01/SeasonIntegral_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  ls_ini[[5]] <- lapply(paste0(path2tempResults, "/test01/SeasonPermanentIntegral_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  ls_ini[[6]] <- lapply(paste0(path2tempResults, "/test01/CycleFraction_EndStep01.RData"), function(x) mget(load(x)))[[1]]
  
  lon <- ls_ini[[1]][[2]]
  lat <- ls_ini[[1]][[3]]
  
  ls_vr <- length(ls_ini)
  #length(ls_ini)
  #str(ls_ini)
  #names(ls_ini[[1]])[1]
  
}



## Calculating averages ####
## For now, average is calculated over ALL the available years, but this might be included as an argument

vrbles <- c()
stack_rstrs_avg <- stack() 
stuff2save <- c()

t0 <- Sys.time()
for (i in 1:ls_vr){
  
  if(grepl("OldData", var2process_name)){
    nms <- vrbls_lst[i]
    var2process <- stack(paste0(path2tempResults, "/", nms, "_clean.tif"))
    #print(nms)
    #print(var2process)

  }else{
    ## To raster bricks
    var2process <- brick(ls_ini[[i]][[1]])
    var2process <- t(var2process)
    extent(var2process) <- c(range(lon),  range(lat))
    #assign(paste0(names(ls_ini[[i]])[1], "_rstr"), var2process)
    #print(paste0(paste0(names(ls_ini[[i]])[1], "_rstr"), " ... generated"))
    
    nms <- names(ls_ini[[i]])[1]
           
  }
  
  ## Averaging
  beginCluster()   # it uses n - 1 clusters
  yrs <- 1:nlayers(var2process)
  rstr_average <- clusterR(var2process, calc, args = list(fun = mean_years_function), export = "yrs")
  endCluster()
  
  names(rstr_average) <- nms
  vrbles <- c(vrbles, nms)
  assign(paste0(nms, "_avrge"), rstr_average)
  writeRaster(get(paste0(nms, "_avrge")), filename = paste0(path2tempResults, "/", nms, "_avrge.tif"), overwrite = TRUE)
  #assign(paste0(nms, "_avrge"), raster(paste0(path2tempResults, "/", nms, "_avrge.tif")))

  stack_rstrs_avg <- stack(stack_rstrs_avg, get(paste0(nms, "_avrge")))
  print(paste0(nms, " ... average calculated"))
  stuff2save <- c(stuff2save, paste0(nms, "_avrge"))
}

writeRaster(stack_rstrs_avg, paste0(path2tempResults, "/stack_rstrs_avg.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
Sys.time() - t0



#some checks...
#vrbles
#stack_rstrs_avg
#
#load(paste0(path2tempResults, "/SeasonIntegral_EndStep01.RData"), verbose = TRUE)
#SeasonIntegral_01 <- brick(SeasonIntegral)
#SeasonIntegral_01 <- t(SeasonIntegral_01)
#extent(SeasonIntegral_01) <- c(range(lon),  range(lat))
#SeasonIntegral_01
#
#
#beginCluster()   # it uses n - 1 clusters
#yrs <- 1:length(names(SeasonIntegral_01))
#SeasonIntegral_01_avg <- clusterR(SeasonIntegral_01, calc, args = list(fun = mean_years_function), export = "yrs")
#endCluster()
#SeasonIntegral_01_avg
#
#stack_rstrs_avg <- stack(stack_rstrs_avg@layers[-c(7:8)])




## Multicollinearity ####
#graphics.off()
t0 <- Sys.time()
#multicol_df <- cor(getValues(stack_rstrs_avg), use = "complete.obs", method = "pearson")  #method = c("pearson", "kendall", "spearman")
vrbles_NoC <- virtualspecies::removeCollinearity(stack_rstrs_avg,
                                                 multicollinearity.cutoff = 0.70,  # it uses Pearson's R
                                                 select.variables = TRUE,  # if TRUE, randomly select one variable of the group. If FALSE, returns a list with the groups
                                                 #sample.points = FALSE,  # using all pixels
                                                 sample.points = TRUE,  # using nb.points to calculate multicollinearity
                                                 #nb.points = 100000,
                                                 nb.points = 1000000,
                                                 plot = TRUE)
vrbles_NoC

# saving plot
dev.copy(pdf, paste0(path2tempResults, "/vars_collinearity.pdf"))    
dev.off() 

Sys.time() - t0


#stack_rstrs_avg
#stack_rstrs_avg@layers[[1]]@file@name

# removing correlated variables
stack_rstrs_avg_noC <- brick(stack_rstrs_avg@layers[names(stack_rstrs_avg) %in% vrbles_NoC])


# saving results
writeRaster(stack_rstrs_avg_noC, paste0(path2tempResults, "/stack_rstrs_avg_noC.tif"), options = "INTERLEAVE=BAND", overwrite = TRUE)
stuff2save <- c(stuff2save, "vrbles", "stack_rstrs_avg", 
                #"multicol_df", 
                "vrbles_NoC", "stack_rstrs_avg_noC")
save(list = stuff2save, file = paste0(path2tempResults, "/results_Step6.RData"))

