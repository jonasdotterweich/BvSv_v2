# 3_smaller bv df based on studyarea of snow vole

## working with bv_thin2 currently in environment

library(raster)
library(terra)
library(sf)
library(sp)

bv_thin <- bv_thin4_thin1
sv_thin <- sv_thin3_thin1

# geometric points for visualization

bv_pts <- st_as_sf(bv_thin, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

sv_pts <- st_as_sf(sv_thin, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

write_sf(sv_pts, "occ_pts/sv_pts.shp", overwrite=T)
write_sf(bv_pts, "occ_pts/bv_pts.shp", overwrite=T)

mapview(list(bv_pts, sv_pts))



# croping the dataframe of BV to Svstudar

#first we need a spatial point dtata frame
xy <- bv_thin[,c(2,3)]

BV_spat_df <- SpatialPointsDataFrame(coords = xy, data = bv_thin,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

xy2 <- sv_thin[,c(2,3)]
SV_spat_df <-  SpatialPointsDataFrame(coords = xy2, data = sv_thin,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#extent of studar 
ext <- extent(-9.308684, 15.0175, 36.15523, 48.32953)

# studar raster
mask_rast <- raster(ext)

studar <- read_sf("studar/study_area(SVthin).shp")

stud_rast <- rasterize(studar, mask_rast)


BV_sub <- crop(vect(BV_spat_df), ext)


writeVector(BV_sub, "occ_pts/bv_to_studarSV/bv_in_SVarea.shp", overwrite=T)

# as dataframe
bv_sub_df <- as.data.frame(BV_sub)

write.csv(bv_sub_df, "occ_pts/bv_to_studarSV/bv_in_SVarea.csv")


#as ist can be seen while visualizing in a map, a lot of lowland bank vole still in the model. will delimit up to paris manually

bv_reduced <- read_sf("occ_pts/bv_to_studarSV/bv_in Sv_reduced.shp")

bv_red_df  <- as.data.frame(bv_reduced)

write.csv(bv_red_df, "occ_pts/bv_to_studarSV/bv_in_SV_reduced.csv")

bv_red_df  <- bv_red_df[-c(4)]

write.csv(bv_red_df, "occ_pts/bv_to_studarSV/bv_in_SV_reduced.csv")



######## idea for extended study area:

## as the bv points are not masked but cropped to the study area there are some outside of the actual study area. 
# what if the sv & bv cropped to sv area get combined for the sake of creating a more extended study area

### 31.01.23: deleting portugal and spain point in sv thin


sv_thin_df <- as.data.frame(sv_thin)

sv_thin_df <- sv_thin_df[-15,]
sv_thin_df <- sv_thin_df[-5,]

write.csv(sv_thin_df, "occ_pts/thinned/sv_thin_df.csv")

bv_red_df  <- bv_red_df %>% dplyr::rename( decimalLongitude = decimalLon,
                                           decimalLatitude =  decimalLat)

joined_pts <- rbind(sv_thin_df, bv_red_df)


studar_joined <- calib_area(joined_pts, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                     groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(studar_joined)

writeVector(studar_joined, "studar/study_area(svthin+bvcrop).shp", overwrite=T)









