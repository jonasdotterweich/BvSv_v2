### creation of maps 


### presence points in study area


library(ggplot2)
library(ggspatial)
library(rnaturalearthdata)
library(rnaturalearth)


BVocc <- read.csv("occ_pts/bv_to_studarSV/bv_in_SV_reduced.csv")

SVocc <- read.csv("occ_pts/thinned/sv_thin_df.csv")





## --- trying to make nice maps with ggplot and ggspat

## needing the extent of the study area 

study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

st_crs(study_area) <- st_crs(4326)

extent(study_area)

### trying the linepoints created in Qgis

studar_linepts <- read_sf("studar/studar_linepts.shp")


geopts <- as.data.frame(studar_linepts$geometry)


studar_pts <- studar_linepts %>% mutate(x = map_dbl(geometry, ~ .x[1]),
                                        y = map_dbl(geometry, ~ .x[2]))

pts_df <- as.data.frame(studar_pts)

pts_df <- pts_df[,8:9]

### this is the dataframe of the coordinates which compile the study area




world <- ne_countries(scale = "medium", returnclass = "sf")



ggplot(data = world) +
  geom_sf() +
  geom_point(data = BVocc, aes(decimalLon, decimalLat), color="red", show.legend = TRUE)+
  geom_point(data = SVocc, aes(decimalLongitude, decimalLatitude), color="blue", show.legend = TRUE)+
  
  geom_path(data = pts_df, aes(x=x, y=y), show.legend = TRUE ) +
  
  annotation_scale(location = "bl", width_hint = 0.5,
                   height = unit(0.25, "cm") )+
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.4, "cm"), pad_y = unit(11, "cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-8.314924, 16.07201), ylim = c(39.7, 49.5), crs = sf::st_crs(4326)) +
  xlab("Longitude") + ylab("Latitude") 
  
 



  

  

 
  





