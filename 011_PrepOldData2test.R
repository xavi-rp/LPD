
#### Preparing Old Data Sets For Testing  ####

# These are the data sets used (probably) to perform the published study
# They are derived from SPOT

#rm(list = ls()[!ls() %in% c("path2project", "path2data", "path2saveTests", "path2tempResults")])
source("E:\\rotllxa\\LPD\\LPD/00_settings.R")


## Reading in data ####

path2old_data <- "X:\\RS\\DER_TS\\PHENOL\\SPOT\\INDICIES/"

vrbls_lst <- list.files(path2old_data, pattern = ".bil$", full.names = FALSE)
vrbls_lst

# Variables used for the test -> *

# sbd9913.bil -> Season Beginning Day *
# sbd9913mn.bil -> Season Beginning Day Mean (1999-2013)

# sed9913.bil -> Season End Day *
# sed9913mn.bil -> Season End Day Mean (1999-2013)

# si9913.bil -> Season Integral *
# si9913mskd.bil -> Season Integral Mask (???)

# sl9913.bil -> Season Length *

# mi9913.bil -> Minimum Integral *

# apl9913.bil -> Autocorrelation_peak_lag (???)

# apv9913.bil -> Autocorrelation_peak_value (???)

# status9913.bil -> Status.9813spotvgt30 (???); This has one year more (1998)

vrbls_lst <- c("sbd", "sed", "si", "sl", "mi")

for (vbl in vrbls_lst){
  varbl <- stack(paste0(path2old_data, vbl, "9913.bil"))
  names(varbl) <- 1999:2013
  assign(vbl, varbl)
}

for (vbl in vrbls_lst){
  print(vbl)
  kk <- get(vbl)
  print(summary(getValues(kk[[1]])))
  rm(kk)
}

dataType(si)



# extent: -11.16071, 27.05357, 33.66964, 71.13393  (xmin, xmax, ymin, ymax)
#extent(var2process)
#si_crop <- crop(si, extent(var2process))
#
#si_crop
#summary(getValues(si_crop$X2010))
#summary(getValues(var2process$layer.12), na.rm = T)
#quantile(getValues(sbd_crop$X2010), 0.001)
#sum(getValues(si_crop$X2010) > 1516)
#sum(getValues(si_crop$X2010) < 1)
#sum(getValues(si_crop$X2010) < 1) / length(getValues(si_crop$X2010))
#sum(getValues(var2process$layer.12) < 1, na.rm = T) / length(getValues(var2process$layer.12))
















