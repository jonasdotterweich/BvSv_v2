# 11_nice overlap evaluations

## trying maybe enmSdm & ENMTools

library(enmSdm)
library(ENMTools)
library(sf)


#loading the models

#loading the model for BV

load("Models/BV_ensembled_v2.Rda")

#loading the model for SV

load("Models/SV_ensembled_v2.Rda")


#loading the complete environmental variables (current)

bivars <- brick("Rasters/biovars_VIFed_jSA.tif")

nam <- brick("Rasters/biovars_VIFed_jSA_forenames.grd")


names(bivars) <- names(nam)



####### first trying enm tools a before ####


env.overlap(
 BV_ensembled_v2,
 SV_ensembled_v2,
  bivars,
  tolerance = 0.001,
  max.reps = 10,
  cor.method = "spearman",
  chunk.size = 1e+05,
  recal.model.1 = NA,
  recal.model.2 = NA,
  verbose = FALSE
)

### not working models need to be predictable by predict function

BVmodels <- BV_ensembled_v2$models

#models
bvm1 <- BVmodels$m_1
bvm2 <- BVmodels$m_2
bvm3 <- BVmodels$m_3


bvm1 <- bvm1$model
bvm2 <- bvm2$model
bvm3 <- bvm3$model


SVmodels <- SV_ensembled_v2$models

#models
svm1 <- SVmodels$m_1
svm2 <- SVmodels$m_2
svm3 <- SVmodels$m_3


svm1 <- svm1$model
svm2 <- svm2$model
svm3 <- svm3$model


## splitted it up in the singel models, so I can try like this 

##trying gbm_current

env.ov_gbm <- env.overlap(
                          bvm1,
                          svm1,
                          bivars,
                          tolerance = 0.001,
                          max.reps = 10,
                          cor.method = "spearman",
                          chunk.size = 1e+05,
                          recal.model.1 = NA,
                          recal.model.2 = NA,
                          verbose = FALSE
                        )


env.ov_rf <- env.overlap(
                            bvm2,
                            svm2,
                            bivars,
                            tolerance = 0.001,
                            max.reps = 10,
                            cor.method = "spearman",
                            chunk.size = 1e+05,
                            recal.model.1 = NA,
                            recal.model.2 = NA,
                            verbose = FALSE
                          )


env.ov_svm <- env.overlap(
                          bvm3,
                          svm3,
                          bivars,
                          tolerance = 0.001,
                          max.reps = 10,
                          cor.method = "spearman",
                          chunk.size = 1e+05,
                          recal.model.1 = NA,
                          recal.model.2 = NA,
                          verbose = FALSE
                        )


env.ov_gbm
env.ov_rf
env.ov_svm




## trying it with future bio variables##

bivars_fut <- brick("Rasters/fut/biovars_fut.tif")

names(bivars_fut) <- names(nam)


env.ov_gbmfut <- env.overlap(
  bvm1,
  svm1,
  bivars_fut,
  tolerance = 0.001,
  max.reps = 10,
  cor.method = "spearman",
  chunk.size = 1e+05,
  recal.model.1 = NA,
  recal.model.2 = NA,
  verbose = FALSE
)


env.ov_rffut <- env.overlap(
  bvm2,
  svm2,
  bivars_fut,
  tolerance = 0.001,
  max.reps = 10,
  cor.method = "spearman",
  chunk.size = 1e+05,
  recal.model.1 = NA,
  recal.model.2 = NA,
  verbose = FALSE
)


env.ov_svmfut <- env.overlap(
  bvm3,
  svm3,
  bivars_fut,
  tolerance = 0.001,
  max.reps = 10,
  cor.method = "spearman",
  chunk.size = 1e+05,
  recal.model.1 = NA,
  recal.model.2 = NA,
  verbose = FALSE
)


env.ov_gbm
env.ov_gbmfut


env.ov_rf
env.ov_rffut

env.ov_svm
env.ov_svmfut


#current


envDgbm <- env.ov_gbm$env.D
envDrf <- env.ov_rf$env.D
envDsvm <- env.ov_svm$env.D

curr_env.ov_meanD <- mean(envDgbm, envDrf, envDsvm)

envIgbm <- env.ov_gbm$env.I
envIrf <- env.ov_rf$env.I
envIsvm <- env.ov_svm$env.I

curr_env.ov_meanI <- mean(envIgbm, envIrf, envIsvm)


#future

envDgbm_fut <- env.ov_gbmfut$env.D
envDrf_fut <- env.ov_rffut$env.D
envDsvm_fut <- env.ov_svmfut$env.D

fut_env.ov_meanD <- mean(envDgbm_fut, envDrf_fut, envDsvm_fut)

envIgbm_fut <- env.ov_gbmfut$env.I
envIrf_fut <- env.ov_rffut$env.I
envIsvm_fut <- env.ov_svmfut$env.I

fut_env.ov_meanI <- mean(envIgbm_fut, envIrf_fut, envIsvm_fut)


curr_env.ov_meanD
fut_env.ov_meanD 

curr_env.ov_meanI
fut_env.ov_meanI



### question being, how relevant is that?










           # ------------- #

##### okay those two work, should try the function geog.range.overlap

          # ------------- #







#for this we need a species with a raster, so maybe the presence absence dataframes with biovariables

bivars


## we can try to create an enmtools.species object with the function
## needs:
#range
# presence.points
#background.points 
#species.name 
#models


#range:

study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

studar <- as_Spatial(study_area)

crs(studar) <- crs("+init=epsg:4326")

studar <- geometry(studar)

#### there was a warning indicating that studar micht be an S4 object and in another skript it was the same and there were issues with the range
# so checking now... maybe I should pass it in list or vector form.. whereas the formular description is saying a raster or spatial polygon

isS4(studar)
class(studar)

# well but it is an S4 object maybe it needs to be an S3???

sr_list <- list(extent(studar))
class(sr_list)

## well with the list there is an error.. demands raster or spatial polygon

#when trying the geog.range.overlap function further down it is saying that the rang raster is not defined
# so maybe it would be better to give a raster to the function instead of a spatpol...

studar_sf <- st_as_sf(studar)

stud_rast <- raster(studar_sf)

## interesting, no warning with that


## presence points bv 

bv_var <- read.csv("Data/df_BV+bivars.csv")

bv_pres_bck <- bv_var %>% dplyr::distinct(decimalLon, decimalLat, pr_ab)

bv_preocc <- bv_pres_bck %>% dplyr::filter(pr_ab==1)


##background points

bv_bckpts <- bv_pres_bck %>% dplyr::filter(pr_ab==0)


## species names

sp_name <- "Myodes glareolus" 


## models
#first trying the uniform ensembled model as is, as:

BVmodels

### ready for enmtools.species object for bank vole uniform

BVenmt <- enmtools.species(
  range = stud_rast,
  presence.points =  bv_preocc,
  background.points = bv_bckpts,
  species.name = sp_name,
  models = BVmodels
)


### same for snowvole

#range 
studar

#presence.points 

sv_var <- read.csv("Data/df_SV+bivars.csv")

sv_pres_bck <- sv_var %>% dplyr::distinct(decimalLongitude, decimalLatitude, pr_ab)

sv_preocc <- sv_pres_bck %>% dplyr::filter(pr_ab==1)


#background.points 

sv_bckpts<- sv_pres_bck %>% dplyr::filter(pr_ab==0)

#species.name 

sp_name_sv <- "Chionomys nivalis"

#models 

SVmodels



SVenmt <- enmtools.species(
  range = stud_rast,
  presence.points =  sv_preocc,
  background.points = sv_bckpts,
  species.name = sp_name_sv,
  models = SVmodels
)


geog.range.overlap(BVenmt, SVenmt)

### okay, it is asking for a range raster... well trying OKAAAAAAYYY! god damn it function...

###first raster object has o values... why? and why should have it values, and if yes which...

#I am just trying now to input elevation raster to all of it...cause it should have values then
# but idk if it will distort it then


elev <- bivars$WC_alt_lonlat
mlc <- bivars$merged_LC

BVenmt <- enmtools.species(
  range = elev,
  presence.points =  bv_preocc,
  background.points = bv_bckpts,
  species.name = sp_name,
  models = BVmodels
)

SVenmt <- enmtools.species(
  range = elev,
  presence.points =  sv_preocc,
  background.points = sv_bckpts,
  species.name = sp_name_sv,
  models = SVmodels
)


geo_overlap <- geog.range.overlap(BVenmt, SVenmt)

### this worked... haaayyyaaaa
# trying also with other evnrronmental rasters to see if there is and influence
# nope, not with BI03







########----- MOST correct geog.range approach ------######


### 28.01.23 found out how ENMTools wants the range raster

### inputting the same for the range here as was input for the initial study area creation

bv_sub_df <- read.csv("occ_pts/bv_to_studarSV/bv_in_SVarea.csv")

bv_sub_df <- bv_sub_df %>% dplyr::select(-X)

sv_thin <- sv_thin3_thin1

sv_thin_df <- as.data.frame(sv_thin)

joined_pts <- rbind(sv_thin_df, bv_sub_df)

joined_pts <- joined_pts %>% dplyr::select(-species)



rangebvsv <- background.raster.buffer(joined_pts, 100000, bivars[[1]])

BVenmt$range <- rangebv

SVenmt$range <- rangesv



geo_overlap <- geog.range.overlap(BVenmt, SVenmt)

geo_overlap

### okay the geological overlap is 1 because it is a range overlap and we defined the range as the study area which was defind by both species together
## so the input has to be changed 

bv_sub_df <- bv_sub_df %>% dplyr::select(-species)
sv_thin_df <- sv_thin_df %>% dplyr::select(-species)

rangeBV <- background.raster.buffer(bv_sub_df, 100000, bivars[[1]])
rangeSV <- background.raster.buffer(sv_thin_df, 100000, bivars[[1]])

BVenmt$range <- rangeBV

SVenmt$range <- rangeSV


geo_overlap <- geog.range.overlap(BVenmt, SVenmt)


######------     -----######





######------     -----######




###### 28.01.23 #####

## tryning the visualize.overlap function ## 

BVmodels <- BV_ensembled_v2$models
SVmodels <- SV_ensembled_v2$models

bvm1 <- BVmodels$m_1
svm1 <- SVmodels$m_1

bvm1 <- bvm1$model
svm1 <- svm1$model

bvm2 <- BVmodels$m_2
svm2 <- SVmodels$m_2

bvm2 <- bvm2$model
svm2 <- svm2$model



visualize.overlap(bvm2,
                  svm2,
                  bivars,
                  nbins = 100,
                  layers = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                             "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                             "ER_monthCountByTemp10",       "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                             "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                  plot.points = F
)


## sadly it requires an enmtool model object, as my models are created in flexsdm it is not comparable

## tryout to create a rf model in enmtools and run the visualized niche overlap

## need to rename lon lat in presences and absences

#for BV

pres.ptsBV <- BVenmt$presence.points

pres.ptsBV <- pres.ptsBV %>% dplyr::rename(Longitude = decimalLon,
                                           Latitude = decimalLat)

pres.ptsBV <- pres.ptsBV %>% dplyr::select(-pr_ab)

BVenmt$presence.points <- pres.ptsBV


bck.ptsBV <- BVenmt$background.points


bck.ptsBV <- bck.ptsBV %>% dplyr::rename(Longitude = decimalLon,
                                         Latitude = decimalLat) 

bck.ptsBV <- bck.ptsBV %>% dplyr::select(-pr_ab)


BVenmt$background.points <- bck.ptsBV 
  
  

enmRF_bvcurr <- enmtools.rf(BVenmt,
                            bivars)


#for SV

pres.ptsSV <- SVenmt$presence.points

pres.ptsSV <- pres.ptsSV %>% dplyr::rename(Longitude = decimalLongitude,
                                           Latitude = decimalLatitude)

pres.ptsSV <- pres.ptsSV %>% dplyr::select(-pr_ab)

SVenmt$presence.points <- pres.ptsSV


bck.ptsSV <- SVenmt$background.points


bck.ptsSV <- bck.ptsSV %>% dplyr::rename(Longitude = decimalLongitude,
                                         Latitude = decimalLatitude) 

bck.ptsSV <- bck.ptsSV %>% dplyr::select(-pr_ab)


SVenmt$background.points <- bck.ptsSV 



enmRF_svcurr <- enmtools.rf(SVenmt,
                            bivars)



### vector of layer names

l_names <- c(  "BIO3", "BIO19")


vis.ov <- visualize.overlap(enmRF_bvcurr,
                            enmRF_svcurr,
                            bivars,
                            nbins = 100, 
                            layers = l_names)


##### okay, have to say, that I dont know if this Is the way

### waht else can be done???

#### RANGEBREAK could be tried as well?
#### yet rangebreak would not accept my flexsdm models, or even just weaker models 


## or

#### maybe an ecospat analysis should be incoporated

### trying ecospats niche overlap...... requires some new creations though, lets see if its doable in not too much time




############

#ECOSPAT#

############


library(ecospat)

## for nich overlap we need an ecospat grod.clim object
## therefor we need a dataframe with the climatic varaiables, lets see if we can use bivars and modulate it to work
# well maybe not bivars as wee need a table so maybe the training df for BV (in case of BV)


trainBV <- read.csv("Models/training_BV_v2.csv")
df_BV <- read.csv("Data/df_BV+bivars.csv")

## the function states, that it wants backgroundpoints.. can absences be used or should backgroundpoints be created?
#trying as absences = backgroundpoints


# we are missing background pixels for the whole study area

bivars$

bck_grid <- raster(xmn=-9.333333, xmx=16.08333, ymn=36.16667, ymx=49.25, res=0.08333333)

stack(bck_grid, bivars)
## back grid was ignored as it was empty

#what if I just make the bivars as a dtat frame, is this not a grid??

bivars_df <- as.data.frame(bivars)


#then this would be glob1

df_bck <- df_BV %>% dplyr::filter(pr_ab==0)

df_bck_woprab <- df_bck %>% dplyr::select(-pr_ab)

#this would be sp

df_pres <- df_BV %>% dplyr::filter(pr_ab==1)

df_pres_woprab <- df_pres %>% dplyr::select(-pr_ab)

### resolution of grid, we will take the resolution of bivars

res(bivars)


ecospat.grid.clim.dyn(glob = bivars, glob1 = df_bck_woprab, sp= df_pres_woprab, R= 0.08333333)
