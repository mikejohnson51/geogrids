# modis_runner
library(geogrids)

#---------------------------------------------------------
### Basic paths, AOI, and spatial grid
AOI = AOI::aoi_get(state = "conus")
main_dir = "/Volumes/Transcend/ngen"
gridmet_grid = make_grid(file = "/Volumes/Transcend/ngen/gridmet/elevation.nc")
modis_lc = make_grid(file = '/Volumes/Transcend/ngen/MODIS/MCD12Q1.006/1km/2019-01-01.tif')

# Leaf Area Index ---------------------------------------------------------
# https://lpdaac.usgs.gov/products/mcd15a2hv006/
# 8 day product
product = 'MOD15A2H.006'

downloadMODIS(AOI,
              product,
              date = c("2001-01-01", "2020-12-31"))

(patterns = getSubsets(product = product))

mosaicMODIS(dir = geo_path(),
            product = product,
            date = c("2010-01-01", "2020-12-31"),
            pattern = patterns[2],
            # --- terra automatically applies scale factor in reading but not to range ---#
            range = c(0,100) * .1,
            r = "bilinear",
            overwrite = TRUE,
            cog = TRUE)

# 8 day to monthly
day8_to_month(dir = '/Volumes/Transcend/ngen/MODIS/MOD15A2H.006/mosaics_cogs',
              measurement = "representative",
              date = c("2010-01-01", "2020-12-31"))


complete_netcdf(dir  = file.path(main_dir, product, 'monthly/'),
                out  = file.path(main_dir, product, 'annual', paste0(product, ".nc")) ,
                keep = FALSE,
                name = "LAI",
                long_name = "Leaf Area Index")

months = sprintf("%02s", 1:12)

dir = file.path(dirname(file$fullname[1]), "monthly_mean")

for(i in 1:12){
  outfile = file.path(dir, paste0(months[i], ".tif"))

  if(!file.exists(outfile)){
    l = filter(file, grepl(paste0(months[i], "-01"), fullname))
    t = terra::rast(l$fullname)
    mt = mean(t, na.rm = TRUE)
    terra::writeRaster(mt, filename = outfile, overwrite = TRUE)
  }
}

tc = terra::rast(file$fullname)

z = zonal::execute_zonal_cat(tc, geom, "ID")

# NDVI ---------------------------------------------------------
# 16 product
product = 'MOD13A3.006'

dir = file.path(main_dir, "MODIS", product, 'raw')
fs::dir_create(dir)

downloadMODIS(AOI, product, startDate = "2001-01-01", endDate = "2020-12-31", dir)

patterns = getSubsets(product = product)

mosaicMODIS(dir = geo_path(),
            product = product,
            date = c("2010-01-01", "2019-12-31"),
            pattern = patterns[1],
            # --- scale factor must be applied twice? terra misapplies it? ---#
            scale_factor = 0.0001^2,
            range = c(-2000, 10000),
            r = "bilinear",
            overwrite = TRUE,
            cog = TRUE)



# LC ---------------------------------------------------------
# Yearly product
product = 'MCD12Q1.006'
dir = file.path(main_dir,  "MODIS", product, 'raw')
fs::dir_create(dir)

downloadMODIS(AOI,
              product,
              date = c("2010-01-01", "2020-12-31"),
              dir)

patterns = getSubsets(list.files(dir, recursive = TRUE, full.names = TRUE)[1])


mosaicMODIS(dir = geo_path(),
            product = product,
            date = c("2010-01-01", "2020-12-31"),
            pattern = patterns[1],
            r = "bilinear",
            overwrite = TRUE,
            cog = TRUE)

######

GVF


modis_lc = filter(files, grepl('MCD12Q1.006/1km/2019-01-01.tif', .data$fullname))

lc_tiff = terra::rast(modis_lc$fullname) %>%
  terra::crop(cats, snap = "out")

lu = zonal::execute_zonal_cat(lc_tiff, w = soils_w)

forest = lu %>%
  filter(value %in% c(1:5)) %>%
  group_by(ID) %>%
  summarise(forest = sum(percentage, na.rm = TRUE)) %>%
  ungroup()

traits = left_join(traits, forest, by = "ID") %>%
  mutate(forest = ifelse(is.na(forest), 0, forest))

## GVF & LAI

modis_mapping = read.csv('/Users/mjohnson/github/geogrids/inst/modis_lc.csv')
modis_mapping$ndvi_inf = c(0.81,0.86,0.88,
                           0.90,0.87,0.86,
                           0.86,0.75,0.76,
                           0.74,0.86,0.84,
                           0.86,0.82,NA,
                           0.86,NA)


library(glue)
library(terra)
library(dplyr)

years = c(2015:2019)
tifs = '/Volumes/Transcend/ngen/MODIS/GVF/tifs'
cogs = '/Volumes/Transcend/ngen/MODIS/GVF/cogs'

for(y in 1:length(years)){

lc_tiff = glue('/Volumes/Transcend/ngen/MODIS/MCD12Q1.006/mosaics_cog/{year}-01-01.tif', year = years[y])

ndvi_denom = terra::classify(rast(lc_tiff),
                             dplyr::select(modis_mapping, is = Class, becomes = ndvi_inf)) - 0.05

files = list.files(
  '/Volumes/Transcend/ngen/MODIS/MOD13A3.006/mosaics_cog',
  pattern = "tif$",
  full.names = TRUE
)

files = grep(years[y], files, value = TRUE)

ndvi =  terra::rast(files) - 0.05

ndvi_denom_norm = geogrid_warp(file = ndvi_denom,
                   grid = make_grid(ndvi[[1]]),
                   disk = FALSE)

gvf  = ndvi / ndvi_denom_norm
gvf2 = terra::clamp(gvf, lower = 0, upper = 1)

names = basename(files)

for(i in 1:12){
  writeRaster(gvf2[[i]], file.path(tifs, names[i]), overwrite = TRUE)
  unlink(file.path(cogs, names[i]))
  make_cog(file.path(tifs, names[i]), file.path(cogs, names[i]))

}
message("Finished year: ", years[y])
}

make_cog = function(in_file, out_file){
  sf::gdal_utils("translate",
                 source = in_file,
                 destination  = out_file,
                 options = c("-co", "TILED=YES",
                             "-co",  "COPY_SRC_OVERVIEWS=YES",
                             "-co",  "COMPRESS=DEFLATE"))

}

### GVF
files  = list.files("/Volumes/Transcend/ngen/MODIS/GVF/cogs", full.names = TRUE, pattern = ".tif$")
ind    = rep(1:12, times = length(files) / 12)

gvf.mean <- terra::tapp(rast(files), ind, fun = mean, na.rm = TRUE)
writeRaster(gvf.mean,  '/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-mean.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-mean.tif',
         '/Volumes/Transcend/ngen/MODIS/GVF/summary/cogs/gvf-mean.tif')
gvf.max  <- max(gvf.mean)
writeRaster(gvf.max,  '/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-max.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-max.tif',
         '/Volumes/Transcend/ngen/MODIS/GVF/summary/cogs/gvf-max.tif')
gvf.min  <- min(gvf.mean)
writeRaster(gvf.min,  '/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-min.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-min.tif',
         '/Volumes/Transcend/ngen/MODIS/GVF/summary/cogs/gvf-min.tif')
gvf.dif  <- gvf.max - gvf.min
writeRaster(gvf.dif,  '/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-diff.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/GVF/summary/tifs/gvf-diff.tif',
         '/Volumes/Transcend/ngen/MODIS/GVF/summary/cogs/gvf-diff.tif')

### LAI


files  = list.files('/Volumes/Transcend/ngen/MODIS/MOD15A2H.006/monthly_mean_cog',
                    full.names = TRUE, pattern = ".tif$")
ind    = rep(1:12, times = length(files) / 12)

lai.mean <- terra::tapp(rast(files), ind, fun = mean, na.rm = TRUE)
writeRaster(lai.mean,  '/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-mean.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-mean.tif',
         '/Volumes/Transcend/ngen/MODIS/lai/summary/cogs/lai-mean.tif')
lai.max  <- max(lai.mean)
writeRaster(lai.max,  '/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-max.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-max.tif',
         '/Volumes/Transcend/ngen/MODIS/lai/summary/cogs/lai-max.tif')
lai.min  <- min(lai.mean)
writeRaster(lai.min,  '/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-min.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-min.tif',
         '/Volumes/Transcend/ngen/MODIS/lai/summary/cogs/lai-min.tif')
lai.dif  <- lai.max - lai.min
writeRaster(lai.dif,  '/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-diff.tif', overwrite = TRUE)
make_cog('/Volumes/Transcend/ngen/MODIS/lai/summary/tifs/lai-diff.tif',
         '/Volumes/Transcend/ngen/MODIS/lai/summary/cogs/lai-diff.tif')





