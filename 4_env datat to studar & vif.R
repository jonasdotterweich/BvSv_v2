### environmental variables in joined studar

library(raster)
library(sf)
library(dismo)
library(rgdal)
library(gdalUtils)


# import Bio & envirem variables

setwd("~/Documents/R Project Bank Vole")

BIO1 <- raster("Raster/raw/BIO/BIO1.tif")
BIO2<-raster("Raster/raw/BIO/BIO2.tif")
BIO3<-raster("Raster/raw/BIO/BIO3.tif")
BIO4<-raster("Raster/raw/BIO/BIO4.tif")
BIO5<-raster("Raster/raw/BIO/BIO5.tif")
BIO6<-raster("Raster/raw/BIO/BIO6.tif")
BIO7<-raster("Raster/raw/BIO/BIO7.tif")
BIO8<-raster("Raster/raw/BIO/BIO8.tif")
BIO9<-raster("Raster/raw/BIO/BIO9.tif")
BIO10<-raster("Raster/raw/BIO/BIO10.tif")
BIO11<-raster("Raster/raw/BIO/BIO11.tif")
BIO12<-raster("Raster/raw/BIO/BIO12.tif")
BIO13<-raster("Raster/raw/BIO/BIO13.tif")
BIO14<-raster("Raster/raw/BIO/BIO14.tif")
BIO15<-raster("Raster/raw/BIO/BIO15.tif")
BIO16<-raster("Raster/raw/BIO/BIO16.tif")
BIO17<-raster("Raster/raw/BIO/BIO17.tif")
BIO18<-raster("Raster/raw/BIO/BIO18.tif")
BIO19<-raster("Raster/raw/BIO/BIO19.tif")

bioc <- brick(BIO1, BIO2, BIO3, BIO4, BIO5, BIO6, BIO7, BIO8, BIO9, BIO10, BIO11, 
              BIO12, BIO13, BIO14, BIO15, BIO16, BIO17, BIO18, BIO19)

### -- Envirem -- ###

ER1 <- raster("Raster/raw/ENVIREM/ER_annualPET.tif")
ER2 <- raster("Raster/raw/ENVIREM/ER_aridityIndexThornthwaite.tif")
ER3 <- raster("Raster/raw/ENVIREM/ER_climaticMoistureIndex.tif")
ER4 <- raster("Raster/raw/ENVIREM/ER_continentality.tif")
ER5 <- raster("Raster/raw/ENVIREM/ER_embergerQ.tif")
ER6 <- raster("Raster/raw/ENVIREM/ER_growingDegDays0.tif")
ER7 <- raster("Raster/raw/ENVIREM/ER_growingDegDays5.tif")
ER8 <- raster("Raster/raw/ENVIREM/ER_maxTempColdest.tif")
ER9 <- raster("Raster/raw/ENVIREM/ER_minTempWarmest.tif")
ER10 <- raster("Raster/raw/ENVIREM/ER_monthCountByTemp10.tif")
ER11 <- raster("Raster/raw/ENVIREM/ER_PETColdestQuarter.tif")
ER12 <- raster("Raster/raw/ENVIREM/ER_PETDriestQuarter.tif")
ER13 <- raster("Raster/raw/ENVIREM/ER_PETseasonality.tif")
ER14 <- raster("Raster/raw/ENVIREM/ER_PETWarmestQuarter.tif")
ER15 <- raster("Raster/raw/ENVIREM/ER_PETWettestQuarter.tif")
ER16 <- raster("Raster/raw/ENVIREM/ER_thermicityIndex.tif")
ER17 <- raster("Raster/raw/ENVIREM/ER_tri.tif")

envirem <- brick(ER1, ER2, ER3, ER4, ER5, ER6, ER7, ER8, ER9, ER10, ER11, ER12, ER13,
                 ER14, ER15, ER16, ER17)



#### Altitude #### 


altitude <-  brick("Raster/raw/altitude/WC_alt_lonlat.tif")

#### Land cover data #### 

#this time also using copernicus landcover as it is 

setwd("~/Documents/Thesis project/uni_v2")

LC <- brick("Landcover/merged_LC19.tif")



### cropping and masking of the stacks

#importing the study area



#loading the "joined" study area 
study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")


##cropping masking bio


bio_cr <- crop(bioc, study_area)

bio_ma <- mask(bio_cr, study_area)


##cropping masking envirem

envirem_cr <- crop(envirem, study_area)

envirem_ma <- mask(envirem_cr, study_area)


##cropping masking altitude

altitude_cr <- crop(altitude, study_area)
altitude_ma <- mask(altitude_cr, study_area)

names(altitude_ma) <- "WC_alt_lonlat"

##cropping masking resampling landcover

LC_cr <- crop(LC, study_area)
LC_ma <- mask(LC_cr, study_area)
LC_re <- resample(LC_ma, bio_ma)

names(LC_re) <- "merged_LC"


#making a stack

bio_vars_v2 <- stack(bio_ma, envirem_ma, altitude_ma, LC_re)

names(bio_vars_v2)


writeRaster(bio_vars_v2, "Rasters/biovars_joined_studar.tif", format="GTiff", overwrite=TRUE)

#writing another raster for the names

writeRaster(bio_vars_v2, "Rasters/biovars_joined_studar_fornames.grd", overwrite=TRUE)


###### VIF ######


library(usdm)
 
vif_bivars <- vifstep(bio_vars_v2, th = 10)

### vif informations

#26 variables from the 38 input variables have collinearity problem: 
  
#  BIO5 ER_continentality BIO10 BIO11 ER_thermicityIndex BIO1 BIO6 ER_annualPET ER_maxTempColdest BIO12 BIO2 BIO16 BIO9 BIO4 BIO17 ER_growingDegDays0 ER_climaticMoistureIndex BIO18 ER_PETseasonality ER_embergerQ ER_minTempWarmest BIO14 ER_PETColdestQuarter ER_PETWarmestQuarter BIO8 BIO13 

#After excluding the collinear variables, the linear correlation coefficients ranges between: 
#  min correlation ( ER_growingDegDays5 ~ BIO19 ):  0.01275519 
#max correlation ( ER_monthCountByTemp10 ~ ER_growingDegDays5 ):  0.8854105 

#---------- VIFs of the remained variables -------- 
#  Variables      VIF
#1                         BIO3 1.782569
#2                         BIO7 1.645539
#3                        BIO15 1.710678
#4                        BIO19 2.092933
#5  ER_aridityIndexThornthwaite 5.220590
#6           ER_growingDegDays5 7.314992
#7        ER_monthCountByTemp10 9.612055
#8          ER_PETDriestQuarter 3.578741
#9         ER_PETWettestQuarter 2.373141
#10                      ER_tri 3.921764
#11               WC_alt_lonlat 6.839317
#12                   merged_LC 1.379662

##### Update feb.23: month count by 10 was dropped as study area was made smaller



bivars_to_keep <- exclude(bio_vars_v2, vif_bivars)

names(bivars_to_keep)


#checking the extent as it was ot correct to the study_area in an occasion later on

study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

bivars_to_keep <- crop(bivars_to_keep, study_area)
bivars_to_keep <- mask(bivars_to_keep, study_area)





#saving 

writeRaster(bivars_to_keep , "Rasters/biovars_VIFed_jSA.tif", format="GTiff", overwrite=TRUE)

writeRaster(bivars_to_keep , "Rasters/biovars_VIFed_jSA_forenames.grd", overwrite=TRUE)





