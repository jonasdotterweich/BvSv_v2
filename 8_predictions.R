# 8_predictions

library(flexsdm)
library(sp)
library(raster)
library(sf)
library(terra)


#predict area needs spatial polygon:

study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

studar_sp <- as_Spatial(study_area)


# loading biovariables
#old

bivars_old <- brick("Rasters/biovars_VIFed_jSA.tif")

oldnames <- brick("Rasters/biovars_VIFed_jSA_forenames.grd")

names(bivars_old) <- names(oldnames)

bivars_old <- rast(bivars_old)

#new

bivars_new <- brick("Rasters/fut/biovars_fut.tif")

newnames <- brick("Rasters/fut/biovars_fut_fornames.grd")

names(bivars_new) <- names(newnames)


bivars_new <- rast(bivars_new)


#loading the model for BV

load("Models/BV_ensembled_v2.Rda")

#loading the model for SV

load("Models/SV_ensembled_v2.Rda")


# predicting current BV

currBV <- sdm_predict(
  models = BV_ensembled_v2,
  pred = bivars_old,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


# predicting current SV

currSV <- sdm_predict(
  models = SV_ensembled_v2,
  pred = bivars_old,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


# predicting future BV

futBV <- sdm_predict(
  models = BV_ensembled_v2,
  pred = bivars_new,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


# predicting future SV

futSV <- sdm_predict(
  models = SV_ensembled_v2,
  pred = bivars_new,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


##


currBVmean <- currBV$mean
currSVmean <- currSV$mean

futBVmean <- futBV$mean
futSVmean <- futSV$mean

currBVmean_m <- currBVmean$mean
currSVmean_m <- currSVmean$mean
currBVmax <- currBVmean$max_sens_spec
currSVmax <- currSVmean$max_sens_spec

futBVmean_m <- futBVmean$mean
futSVmean_m <- futSVmean$mean
futBVmax <- futBVmean$max_sens_spec
futSVmax <- futSVmean$max_sens_spec

plot(currBVmean)
plot(currSVmean)
plot(futBVmean)
plot(futSVmean)


plot(currBVmean)




#differences
#not possible because different extents of current and future.... can not resample because the values cange slightly

diffBV <-  futBVmean - currBVmean

plot(diffBV)

diffSV <- futSVmean - currSVmean

plot(diffSV)

interBV<- interp(currBVmean$mean,
       futBVmean$mean,
       2020,
       2030)

plot(interBV)





#### exploring means off ggplot to plot predictions

names(currBVmean_m) <- "Probability"
currBVmean_df <- as.data.frame(currBVmean_m, xy= TRUE)

prob_limits <- c(0, 1)




### bank vole current

BV_plotpres <- ggplot()+
                    geom_raster(data = currBVmean_df, mapping = aes(x, y, fill= Probability))+
                    geom_sf(fill= "transparent", data= study_area) +
                    scale_fill_viridis_c(name="Probability", limits= prob_limits, direction = 1)+
                    xlab("Longitude") + ylab("Latitude") +
                    ggtitle("Prediction Bank Vole Present") 


### bank vole future

names(futBVmean_m) <- "Probability"
futBVmean_df <- as.data.frame(futBVmean_m, xy= TRUE)


BV_plotfut <- ggplot()+
                geom_raster(data = futBVmean_df, mapping = aes(x, y, fill= Probability))+
                geom_sf(fill= "transparent", data= study_area) +
                scale_fill_viridis_c(name="Probability", limits= prob_limits, direction = 1)+
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("Prediction Bank Vole Future") 



### snow vole present
names(currSVmean_m) <- "Probability"
currSVmean_df <- as.data.frame(currSVmean_m, xy= TRUE)



SV_plotpres <- ggplot()+
  geom_raster(data = currSVmean_df, mapping = aes(x, y, fill= Probability))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_c(name="Probability", limits= prob_limits, direction = 1)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Prediction Snow Vole Present") 


### snow vole future
names(futSVmean_m) <- "Probability"
futSVmean_df <- as.data.frame(futSVmean_m, xy= TRUE)



SV_plotfut <- ggplot()+
  geom_raster(data = futSVmean_df, mapping = aes(x, y, fill= Probability))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_c(name="Probability", limits= prob_limits, direction = 1)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Prediction Snow Vole Future") 



BV_pres_fut <- gridExtra::grid.arrange(BV_plotpres, BV_plotfut)
SV_pres_fut <- gridExtra::grid.arrange(SV_plotpres, SV_plotfut)


############################################
#### Trying to do the same for Max_sens_spec
############################################


#still need to figure it out better

### bank vole current

names(currBVmax) <- "Presence/Absence"
currBVmax_df <- as.data.frame(currBVmax, xy= TRUE)


BV_plotpres_max <- ggplot()+
  geom_raster(data = currBVmax_df, mapping = aes(x, y, fill= "True"))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_d(name="Presence/Absence")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Binary Prediction Bank Vole Present") 


### bank vole future

names(futBVmax) <- "Presence/Absence"
futBVmax_df <- as.data.frame(futBVmax, xy= TRUE)


BV_plotfut_max <- ggplot()+
  geom_raster(data = futBVmax_df, mapping = aes(x, y, fill= "Presence/Absence"))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_c(name="Presence/Absence", limits= prob_limits, direction = 1)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Binary Prediction Bank Vole Future") 


### snow vole present
names(currSVmax) <- "Presence/Absence"

currSVmax_df <- as.data.frame(currSVmax, xy= TRUE)



SV_plotpres_max <- ggplot()+
  geom_raster(data = currSVmax_df, mapping = aes(x, y, fill= "Presence/Absence"))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_c(name="Presence/Absence", limits= prob_limits, direction = 1)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Binary Prediction Snow Vole Present") 


### snow vole future
names(futSVmax) <- "Presence/Absence"
futSVmax_df <- as.data.frame(futSVmax, xy= TRUE)



SV_plotfut_max <- ggplot()+
  geom_raster(data = futSVmax_df, mapping = aes(x, y, fill= "Presence/Absence"))+
  geom_sf(fill= "transparent", data= study_area) +
  scale_fill_viridis_c(name="Presence/Absence", limits= prob_limits, direction = 1)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Prediction Snow Vole Future") 



BV_pres_fut_max <- gridExtra::grid.arrange(BV_plotpres_max, BV_plotfut_max)
SV_pres_fut_max <- gridExtra::grid.arrange(SV_plotpres_max, SV_plotfut_max)







