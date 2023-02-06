## study area

library(sf)
library(mapview)
library(flexsdm)
library(leaflet)
library(raster)
library(terra)


## study area based on the thinne occurences of the snow vole with a 100 km buffer

studar <- calib_area(sv_thin3_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
           groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


studar_sf <- st_as_sf(studar)


crs(studar) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  
plot(studar)

writeVector(studar, "studar/study_area(SVthin).shp", overwrite=T)


