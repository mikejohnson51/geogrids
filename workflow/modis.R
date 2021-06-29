# Downloaded and corrected from here:
# http://book.ecosens.org/modis-sinusoidal-grid-download/
modis_grid = file.path('/Volumes/Transcend/land-cover-tests/MODIS/modis_grid/modis_sinusoidal_grid_world.shp') %>%
  rgdal::readOGR(verbose = FALSE) %>%
  st_as_sf() %>%
  st_set_crs(modis_proj)
# 
# write_sf(modis_grid, "characteristics/data/modis_grid.gpkg")

# MODIS -------------------------------------------------------------------
library(sf)
library(rvest)
library(httr)

# LAI:
# NDVI:


base.url = 'https://e4ftl01.cr.usgs.gov/MOLT/'
type = "MOD15A2H.006"
type = 'MOD13A3.006'
type = "MCD12Q1.006"
AOI = AOI::aoi_get(state = "conus")
startDate = "2001-02-01"
endDate = "2020-12-31"
dir = '/Volumes/Transcend/ngen/climate/'
#modis_proj = "+proj=sinu +R=6371007.181 +nadgrids=@null +wktext"
modis_grid = read_sf('characteristics/data/modis_grid.gpkg')
reg_bbox_modis_proj   <-  st_transform(AOI, st_crs(modis_grid))
ints  <-  st_filter(modis_grid, reg_bbox_modis_proj)
# What MODIS tiles intersect with the REGION DOMAIN
tiles <-  paste0("h", sprintf("%02d", as.numeric(ints$h)), "v", sprintf("%02d", as.numeric(ints$v)))

dates = paste0(base.url, type) %>% 
  read_html() %>%
  html_nodes("a") %>%
  html_attr('href')

dates = lubridate::ymd(dates) %>% 
  na.omit() 
dates = dates[between(dates, as.Date(startDate), as.Date(endDate))]

fs::dir_create(file.path(dir,'MODIS/', type,  dates ))

for(i in 1:length(dates)){
  path = paste0(base.url, type, '/', gsub("-", ".", dates[i]))
  
  files = path %>% 
    read_html() %>%
    html_nodes("a") %>%
    html_attr('href')
  
  f1 = grep(".hdf$", files, value = TRUE)
  f2 = grep(paste(tiles, collapse = "|"), f1, value = TRUE)
  
  urls = file.path(path, f2)
  
  tmp = file.path(dir,'MODIS', type,  dates[i], f2 )
  
  message("Downloading: ", path)
  lapply(1:length(urls), function(x){
    if(!file.exists(tmp[x])){
      httr::GET(urls[x],
                write_disk(tmp[x], overwrite = TRUE),
                progress(),
                config(netrc = TRUE, netrc_file = getNetrcPath()),
                set_cookies("LC" = "cookies"))
    }
  })
}

# MODIS LC

base = 'https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/2019.01.01/'

reg_bbox_modis_proj   <-  st_transform(AOI, st_crs(modis_grid))
ints  <-  st_filter(modis_grid, reg_bbox_modis_proj)
# What MODIS tiles intersect with the REGION DOMAIN
tiles <-  paste0("h", sprintf("%02d", as.numeric(ints$h)), "v", sprintf("%02d", as.numeric(ints$v)))

for(i in 1:length(dates)){
  path = paste0(base.url, type, '/', gsub("-", ".", dates[i]))
  
  files = base %>% 
    read_html() %>%
    html_nodes("a") %>%
    html_attr('href')
  
  f1 = grep(".hdf$", files, value = TRUE)
  f2 = grep(paste(tiles, collapse = "|"), f1, value = TRUE)
  
  urls = file.path(base, f2)
  
  lcDir = '/Volumes/Transcend/ngen/climate//MODIS/MCD12Q1.006/'
  fs::dir_create(lcDir)
  tmp = file.path(lcDir, f2 )
  
  lapply(1:length(urls), function(x){
    if(!file.exists(tmp[x])){
      httr::GET(urls[x],
                write_disk(tmp[x], overwrite = TRUE),
                progress(),
                config(netrc = TRUE, netrc_file = getNetrcPath()),
                set_cookies("LC" = "cookies"))
    }
  })
}


out_file = paste0(lcDir, "modis_lc_1.nc")

gdalinfo_raw <- sf::gdal_utils(util = "info", tmp[1], quiet  = TRUE)
rr = strsplit(gdalinfo_raw, "\\n")[[1]]

subdataset_rawnames <-  rr[grepl(glob2rx("*SUBDATASET*NAME*"), rr)]
subdataset_names <- sapply(X = seq(length(subdataset_rawnames)),
                           FUN = function(X) {
                             split1 <- strsplit(subdataset_rawnames[X], "=")
                             return(gsub("\"", "", split1[[1]][2]))
                           })

pattern = gsub(tmp[1],"[file]",subdataset_names)[1]
print(pattern)
files = stringr::str_replace(pattern, "\\[file\\]", tmp)

unlink(out_file)
sf::gdal_utils(util = "warp",
               source = files,
               destination  = out_file,
               options = c("-of", "NetCDF", '-t_srs', 'EPSG:4296'))

system(paste('ncrename -v Band1,landcover', out_file))
system(paste('ncatted -O -a long_name,landcover,o,c,"Land CoverType 1: Annual International Geosphere-Biosphere Programme (IGBP) classification"', out_file))

# Build MOSAICS -----------------------------------------------------------
library(dplyr)
dir = '/Volumes/Transcend/ngen/climate/MODIS/MOD13A3.006/rawMonthly'

df = data.frame(dir =  fs::dir_ls(dir)) %>% 
  dplyr::mutate(years = lubridate::year(basename(dir))) %>% 
  filter(between(years, 2001, 2020))


for(i in 1:nrow(df)){
  tmp = list.files(df$dir[i], full.names = TRUE)
  out_file = paste0('/Volumes/Transcend/ngen/climate/MODIS/MOD13A3.006/conusMonthly/', basename(df$dir[i]), ".nc")
  
  gdalinfo_raw <- sf::gdal_utils(util = "info", tmp[1], quiet  = TRUE)
  rr = strsplit(gdalinfo_raw, "\\n")[[1]]
  
  subdataset_rawnames <-  rr[grepl(glob2rx("*SUBDATASET*NAME*"), rr)]
  subdataset_names <- sapply(X = seq(length(subdataset_rawnames)),
                             FUN = function(X) {
                               split1 <- strsplit(subdataset_rawnames[X], "=")
                               return(gsub("\"", "", split1[[1]][2]))
                             })
  
  pattern = gsub(tmp[1],"[file]",subdataset_names)[1]
  print(pattern)
  files = stringr::str_replace(pattern, "\\[file\\]", tmp)
  
  unlink(out_file)
  sf::gdal_utils(util = "warp",
                 source = files,
                 destination  = out_file,
                 options = c("-of", "NetCDF", '-t_srs', 'EPSG:4296'))
  
  system(paste('ncrename -v Band1,ndvi', out_file))
  system(paste('ncatted -O -a long_name,ndvi,o,c,"1 km monthly NDVI"', out_file))
  
  diff = as.Date(gsub(".nc", "", basename(out_file))) - as.Date('1970-01-01')
  system(paste0("ncap2 -s 'defdim(\"time\",1);time[time]=",diff,";time@long_name=\"Time Since 1970-01-01\" ' -O ", out_file, " ", out_file))
  message(i, " of ", nrow(df))
}

dir = '/Volumes/Transcend/ngen/climate/MODIS/MOD13A3.006/conusMonthly/'

df = data.frame(files =  list.files(dir, full = TRUE)) %>% 
  dplyr::mutate(years = lubridate::year(basename(files))) %>% 
  filter(between(years, 2001, 2020))

for(i in unique(df$years)){
  path = paste0('/Volumes/Transcend/ngen/climate/MODIS/MOD13A3.006/conusAnnual/', as.character(i),'.nc')
  
  f = filter(df, years == i)$files
  system(paste('ncecat -u time', paste(f, collapse = " "),  path))
  system(paste('ncks -4 -L 3 -O', path, path))
  message(i)
}

path = paste0('/Users/mikejohnson/Desktop/tmp/CONUS/', as.character(i),'.nc')

f = list.files(path = '/Users/mikejohnson/Desktop/tmp/', pattern =  as.character(i), full.names = TRUE)
system(paste('ncecat -u time', paste(f, collapse = " "),  path))
system(paste('ncks -4 -L 3 -O', path, path))

  
  
}
for(i in 2001:2020){
  
  year_files = filter(df, years == i) 
  
  for(j in 1:nrow(year_files)){
    tmp = list.files(year_files$dir[j], full.names = TRUE)
    out_file = paste0('/Users/mikejohnson/Desktop/tmp/', basename(year_files$dir[j]), ".nc")
  
    gdalinfo_raw <- sf::gdal_utils(util = "info", tmp[1], quiet  = TRUE)
    rr = strsplit(gdalinfo_raw, "\\n")[[1]]
    
    subdataset_rawnames <-  rr[grepl(glob2rx("*SUBDATASET*NAME*"), rr)]
    subdataset_names <- sapply(X = seq(length(subdataset_rawnames)),
                               FUN = function(X) {
                                 split1 <- strsplit(subdataset_rawnames[X], "=")
                                 return(gsub("\"", "", split1[[1]][2]))
                               })
    
    pattern = gsub(tmp[1],"[file]",subdataset_names)[1]
    print(pattern)
    files = stringr::str_replace(pattern, "\\[file\\]", tmp)

    unlink(out_file)
    sf::gdal_utils(util = "warp",
                   source = files,
                   destination  = out_file,
                   options = c("-of", "NetCDF", '-t_srs', 'EPSG:4296'))
    
    system(paste('ncrename -v Band1,LAI', out_file))
    system(paste('ncatted -O -a long_name,LAI,o,c,leaf_area_index', out_file))
    
    diff = as.Date(gsub(".nc", "", basename(out_file))) - as.Date('1970-01-01')
    system(paste0("ncap2 -s 'defdim(\"time\",1);time[time]=",diff,";time@long_name=\"Time Since 1970-01-01\" ' -O ", out_file, " ", out_file))
    message(j)
  }
  
  path = paste0('/Users/mikejohnson/Desktop/tmp/CONUS/', as.character(i),'.nc')
  
  f = list.files(path = '/Users/mikejohnson/Desktop/tmp/', pattern =  as.character(i), full.names = TRUE)
  system(paste('ncecat -u time', paste(f, collapse = " "),  path))
  system(paste('ncks -4 -L 3 -O', path, path))
 
}
 
 
 
 
# 5,339,171,600
library(RNetCDF)
  
system.time({  
test = open.nc("/Users/mikejohnson/Desktop/tmp/CONUS/2001.nc")
tt = var.get.nc(test, "LAI")
})

system.time({  
  test = open.nc("/Users/mikejohnson/Desktop/tmp/CONUS/20012.nc")
  tt = var.get.nc(test, "LAI", unpack = TRUE)
})

dim(tt)
prod(dim(tt))

8560 *2485*   12
255,259,200
295,945,650
