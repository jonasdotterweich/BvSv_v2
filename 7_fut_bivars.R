#### creating the future biovariables 

library(envirem)
library(raster)
library(rgeos)
library(rgdal)
library(maps)
library(sf)




study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

##creating new biovariables for the future period

setwd("~/Documents")

list.files("R Project Bank Vole/Worldclim_GCM_ssp585/MIROC6")


MIROCtmin <- "R Project Bank Vole/Worldclim_GCM_ssp585/MIROC6/wc2.1_30s_tmin_MIROC6_ssp585_2041-2060.tif"
MIROCtmax <- "R Project Bank Vole/Worldclim_GCM_ssp585/MIROC6/wc2.1_30s_tmax_MIROC6_ssp585_2041-2060.tif"
MIROCprec <- "R Project Bank Vole/Worldclim_GCM_ssp585/MIROC6/wc2.1_30s_prec_MIROC6_ssp585_2041-2060.tif"
MIROCbioc <- "R Project Bank Vole/Worldclim_GCM_ssp585/MIROC6/wc2.1_30s_bioc_MIROC6_ssp585_2041-2060.tif"

tmin <-brick(MIROCtmin)
tmax <- brick(MIROCtmax)
prec <- brick(MIROCprec)
bioc <- brick(MIROCbioc)


tmin <- crop(tmin, study_area)
tmax <- crop(tmax, study_area)
prec <- crop(prec, study_area)
bioc <- crop(bioc, study_area)

tmin <- mask(tmin, study_area)
tmax <- mask(tmax, study_area)
prec <- mask(prec, study_area)
bioc <- mask(bioc, study_area)


tmean <- (tmin + tmax) / 2

trange <- abs(tmax - tmin) 

solrad <- ETsolradRasters(tmean, year = 100)

potevap <- monthlyPET(tmean, solrad, trange, tempScale = 1)


names(tmin)
names(tmax)
names(tmean)
names(prec)
names(solrad)

assignNames(tmin = "wc2.1_30s_tmin_##",
            tmax = "wc2.1_30s_tmax_##",
            tmean = "layer.##",
            precip= "wc2.1_30s_prec_##",
            solrad = "et_solrad_##"
) 


worldclimstack <- stack(tmin, tmax, prec)

verifyRasterNames(worldclimstack, solrad)

all <- layerCreation(worldclimstack, solrad, var = "all")



#### first of all Bioclim

#checkinging the old of what I need

setwd("~/Documents/Thesis project/uni_v2")

bivars_old <- brick("Rasters/biovars_VIFed_jSA.tif")

bivars_old_n <- brick("Rasters/biovars_VIFed_jSA_forenames.grd")



names(bivars_old) <- names(bivars_old_n)

BIO3 <- bioc$wc2_3           
BIO7 <- bioc$wc2_7                    
BIO15 <- bioc$wc2_15               
BIO19  <- bioc$wc2_19

biobrick <- brick(BIO3, BIO7, BIO15, BIO19)

ER_aridityIndexThornthwaite <- all$aridityIndexThornthwaite
ER_growingDegDays5  <- all$growingDegDays5

ER_PETDriestQuarter  <- all$PETDriestQuarter
ER_PETWettestQuarter  <- all$PETWettestQuarter

ERbrick <- brick(ER_aridityIndexThornthwaite, ER_growingDegDays5,
                  ER_PETDriestQuarter, ER_PETWettestQuarter)

#importing the rasters that will stay the same as in the old 

setwd("~/Documents/R Project Bank Vole")

ER_tri <- brick("Raster/raw/ENVIREM/ER_tri.tif")

WC_alt_lonlat <- brick("Raster/raw/altitude/WC_alt_lonlat.tif")
        
setwd("~/Documents/Thesis project/uni_v2")      
merged_LC <- brick("Landcover/merged_LC19.tif")





#checking and setting for the right extent
# reminder should be the same as bivar old

extent(bivars_old)

extent(biobrick) <- extent(bivars_old)

#resampling for dimensions#
biobrick <- resample(biobrick, bivars_old)

###### CHECK:YES for bioclim

extent(ERbrick) <- extent(bivars_old)

#resampling for dimensions#
ERbrick <- resample(ERbrick, bivars_old)

###### CHECK:YES for envirem

ER_tri <- crop(ER_tri, study_area)
ER_tri <- mask(ER_tri, study_area)

extent(ER_tri)

###### CHECK:YES for envirem

WC_alt_lonlat <- crop(WC_alt_lonlat, study_area)
WC_alt_lonlat <- mask(WC_alt_lonlat, study_area)

extent(WC_alt_lonlat)

###### CHECK:YES for altitude

merged_LC <- crop(merged_LC, study_area)
merged_LC <- mask(merged_LC, study_area)

extent(merged_LC) <- extent(bivars_old)

###### CHECK:YES for LC

# but also need to resample LC.... wait.. why resample wehn I can just used it directly from the old If no
# landcover future projection is used anyway


merged_LC <- bivars_old$merged_LC


#stacking the future stack

bivars_fut <- stack(biobrick, ERbrick, ER_tri,
                    WC_alt_lonlat, merged_LC)



names(bivars_fut) <- names(bivars_old)


setwd("~/Documents/Thesis project/uni_v2")

writeRaster(bivars_fut, "Rasters/fut/biovars_fut.tif", format= "GTiff", overwrite=T)

writeRaster(bivars_fut, "Rasters/fut/biovars_fut_fornames.grd", overwrite=T)



