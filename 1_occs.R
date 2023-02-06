## uniform_v2  preparing the occurance points + thinning

library(dplyr)


## importing the dataframes manually with rstudio import from files

## seems to be the same so we can continue with the origial df

#selectiong only the ones with coordinates
M_glareolus <- M_glareolus3 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

#selectiong only the ones with Myodes glareolus species name... as thinning 
M_glareolus <- M_glareolus %>% filter(scientificName == "Myodes glareolus (Schreber, 1780)" | 
                                        scientificName == "Clethrionomys glareolus (Schreber, 1780)")


#####preparing dataset for spThin

M_glareolus_thin <-M_glareolus %>% dplyr::select(decimalLatitude, decimalLongitude, species)

### spThin

library(spThin)

thin(
  loc.data= M_glareolus_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "occ_pts/thinned",
  out.base = "bv_thin4",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_bv.txt",
  verbose = TRUE
)

## obtained a thinning with 402 only, maybe better to use 10 kam but let's see... I will try

#####  Now moving to C. nivalis, filtering it and seeing if thinning is ven needed



C_nivalis <- C_nivalis2 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

C_nivalis <- C_nivalis  %>% filter(scientificName == "Chionomys nivalis (Martins, 1842)")

C_nivalis_thin <- C_nivalis %>% dplyr::select(decimalLatitude, decimalLongitude, species)


thin(
  loc.data= C_nivalis_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "occ_pts/thinned",
  out.base = "sv_thin3",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_sv.txt",
  verbose = TRUE
)

#### not a lot of c. nivalis, but we will see after the study area how the ratio changes








