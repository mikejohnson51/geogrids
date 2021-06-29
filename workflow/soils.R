template = raster::raster(crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66',
                   ext = raster::extent(-2357000, 2259000, 277000, 3173000),
                   res = 1000)

conus  = AOI::aoi_get(state = "conus")
bb =  st_as_sfc(st_bbox(conus)) 

# GLiM --------------------------------------------------------------------

bb1 = st_transform(bb, 'ESRI:54012')

data = read_sf('/Volumes/Transcend/ngen/soil/LiMW_GIS 2015.gdb', 
               "GLiM_export", 
               wkt_filter = sf::st_as_text(bb1))

d2 = data %>% 
  st_transform(st_crs(template)) %>% 
  mutate(xx = as.factor(xx))

glim = fasterize::fasterize(d2, template, field = 'xx', background = NA) %>% 
  raster::mask(st_transform(conus, st_crs(template)),
               filename = "/Volumes/Transcend/ngen/soil/GLIM_xx.tif")


# GLHYMS ------------------------------------------------------------------

bb2 = st_transform(bb, 'ESRI:54034') 

data = read_sf('/Volumes/Transcend/ngen/soil/GLHYMPS/GLHYMPS/GLHYMPS.gdb', 
               "Final_GLHYMPS_Polygon", 
               wkt_filter = sf::st_as_text(bb2))

d2 = st_transform(data, st_crs(template))

glhymps = fasterize::fasterize(d2, template, background = NA, field = 'Permeability_permafrost') %>% 
  raster::mask(st_transform(conus, st_crs(template)),
               filename = "/Volumes/Transcend/ngen/soil/GLHYMPS.tif")

