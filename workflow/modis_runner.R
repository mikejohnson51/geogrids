# modis_runner
library(AOI)
library(geogrids)

#---------------------------------------------------------
### Basic paths, AOI, and spatial grid
AOI   = AOI::aoi_get(state = 'conus')
prefix = "gridmet"
te    = c(-124.7875, 25.04583,-67.0375, 49.42083)
tr    = c(0.04166667, 0.04166667)
t_srs = '+proj=longlat +ellps=WGS84 +no_defs'

# Leaf Area Index ---------------------------------------------------------
# 8 day product
product = 'MOD15A2H.006'

downloadMODIS(AOI,
              product,
              startDate = "2001-01-01",
              endDate = "2020-12-31")

patterns = getSubsets(product = product)

mosaicMODIS(prefix  = prefix,
            product = product,
            date    = c("2001-01-01", "2001-12-31"),
            pattern = patterns[2],
            te      = te,
            tr      = tr,
            t_srs   = t_srs,
            r       = "bilinear")

# 8 day to monthly
day8_to_month(product = product, prefix = prefix)

complete_netcdf(dir  = file.path(main_dir, product, 'monthly/'),
                out  = file.path(main_dir, product, 'annual', paste0(product, ".nc")) ,
                keep = FALSE,
                name = "LAI",
                long_name = "Leaf Area Index")


# NDVI ---------------------------------------------------------
# Monthly product
product = 'MOD13A3.006'
dir = file.path(main_dir, product, 'raw')
fs::dir_create(dir)

downloadMODIS(AOI, product, startDate = "2001-01-01", endDate = "2020-12-31", dir)

patterns = getSubsets(list.files(dir, recursive = TRUE, full = TRUE)[1])

mosaicMODIS(dir = geo_path(),
            prefix = "gridmet",
            product = product,
            date = c("2001-01-01", "2020-12-31"),
            pattern = patterns[2],
            te = te,
            tr = tr,
            t_srs = t_srs,
            r = "bilinear",
            overwrite = FALSE,
            of = "GTiff")

complete_netcdf(dir = file.path(main_dir, product, 'conus/'),
                out = file.path(main_dir, product, paste0(product, ".nc")) ,
                keep = FALSE,
                name = "NDVI",
                long_name = "Normalized Difference Vegitation Index")

# LC ---------------------------------------------------------
# Yearly product
product = 'MCD12Q1.006'
dir = file.path(main_dir, product, 'raw')
fs::dir_create(dir)

downloadMODIS(AOI,
              product,
              startDate = "2019-01-01",
              dir,
              base.url = 'https://e4ftl01.cr.usgs.gov/MOTA/')

patterns = getSubsets(list.files(dir, recursive = TRUE, full = TRUE)[1])

## Usings SOIL 1 km grid
mosaicMODIS(dir,
            outDir = file.path(main_dir, product, 'conus'),
            pattern = patterns[1],
            te = c(-2357000, 277000, 2259000, 3173000),
            tr = c(1000,1000),
            t_srs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66',
            method = "near")

complete_netcdf(dir = file.path(main_dir, product, 'conus/'),
                out = file.path(main_dir, product, paste0(product, ".nc")) ,
                keep = FALSE,
                name = "LCType1",
                long_name = "LCType1")


