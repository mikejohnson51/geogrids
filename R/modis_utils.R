#' Scrape MODIS molt and mota archives
#' @param product a MODIS product
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom dplyr `%>%`
#' @family modis
#' @export

.modis_dir = function(product){
  molt = read_html('https://e4ftl01.cr.usgs.gov/MOLT/') %>%
    html_nodes("a") %>%
    html_attr('href')

  mota = read_html('https://e4ftl01.cr.usgs.gov/MOTA/') %>%
    html_nodes("a") %>%
    html_attr('href')

  av = c(paste0("https://e4ftl01.cr.usgs.gov/MOLT/", molt[-c(1:7)]),
    paste0("https://e4ftl01.cr.usgs.gov/MOTA/", mota[-c(1:7)]))

  grep(product, av, value = TRUE)
}

#' Download MODIS tiles
#' @param AOI the region to find data for (sf)
#' @param product a MODIS product
#' @param date the date(s) to download data for passed as a vector of length 1 (single date), or
#' 2 (range of dates)
#' @param dir the directory to cache data. Default is `geo_path()`
#' @param netrc the path to your netrc file. Defaults to `getNetrcPath()`
#' @export
#' @importFrom sf read_sf st_filter st_transform st_crs
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom httr GET write_disk config set_cookies progress
#' @importFrom dplyr group_by filter summarise n between tibble
#' @family modis

downloadMODIS = function(AOI,
                         product,
                         date = NULL,
                         dir   = geo_path(),
                         netrc = getNetrcPath()){

  subdir <- NULL

  if(is.null(date)){stop("date must be specified")}
  if(length(date) == 1){ date = c(date, date) }

  home_link  = .modis_dir(product)
  if(length(home_link) == 0){stop (product, " not recognized.")}

  modis_grid = sf::read_sf('inst/extdata/modis_grid.gpkg')
  ints = st_filter(modis_grid, sf::st_transform(AOI, st_crs(modis_grid)))

  tiles <-  paste0("h", sprintf("%02d", as.numeric(ints$h)),
                   "v", sprintf("%02d", as.numeric(ints$v)))

  message(length(tiles), " tiles cover this AOI")

  dates = read_html(home_link) %>%
    html_nodes("a") %>%
    html_attr('href') %>%
    as.Date(format = "%Y.%m.%d/")

  dates = dates[dplyr::between(dates, as.Date(date[1]), as.Date(date[2]))]
  dates = dates[!is.na(dates)]
  message(length(dates), " date(s) covers this period")

  tmpdir = file.path(dir, "MODIS", product, "raw")
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  ts = tibble(fullname = list.files(tmpdir,
                                    ignore.case = TRUE,
                                    recursive = TRUE,
                                    full.names = TRUE))

  ts$subdir   = gsub(paste0(tmpdir, "/"), "", dirname(ts$fullname))

  ts$basename = basename(ts$fullname)

  oo = ts %>%
      dplyr::filter(between(as.Date(subdir), as.Date(date[1]), as.Date(date[2]))) %>%
      dplyr::filter(grepl(paste(tiles, collapse = "|"), basename)) %>%
      dplyr::group_by(subdir) %>%
      dplyr::summarise(count = n())

  if(nrow(oo) == 0){
    dates_to_process = dates
  } else {
    dates_to_process = dates[!dates %in% as.Date(oo$subdir) | oo$count != length(tiles)]
  }

  need =  dates_to_process[!dir.exists(file.path(tmpdir, dates_to_process))]

  lapply(need, function(x){dir.create(file.path(tmpdir, x))})

  message("Need to download data for ", length(dates_to_process), ' dates' )

  if(length(dates_to_process) > 0){
    for(i in 1:length(dates_to_process)){
      path = file.path(home_link, gsub("-", ".", dates_to_process[i]))

      files = read_html(path) %>%
              html_nodes("a") %>%
              html_attr('href')

      f1 = grep(".hdf$", files, value = TRUE)
      f2 = grep(paste(tiles, collapse = "|"), f1, value = TRUE)

      urls = file.path(path, f2)

      tmp = file.path(tmpdir, dates_to_process[i], f2)

      message("Downloading: ", path)
      lapply(1:length(urls), function(x){
        if(!file.exists(tmp[x])){
          httr::GET(urls[x],
                    httr::write_disk(tmp[x], overwrite = TRUE),
                    httr::progress(),
                    httr::config(netrc = TRUE, netrc_file = netrc),
                    httr::set_cookies("LC" = "cookies"))
        }
      })
    }
  }
}

#' Find HDF subsets
#' @param file a HDF file path
#' @param product a MODIS product name
#' @export
#' @importFrom terra rast
#' @family grid

getSubsets = function(file = NULL, product = NULL){

  subdir <- NULL

  if(is.null(file)){
    xx = geo_cache_list(pattern = "hdf") %>%
      filter(grepl(product, subdir))

    file = xx$fullname[1]
  }

  if(!file.exists(file)){stop("File not found.")}

  names(terra::rast(file))
}


#' Mosaic/Warp Raw MODIS Tiles
#' @description Mosaics all MODIS HDF files for a given date to a new file with a flexible extent (te),
#' resolution (tr), CRS (t_srs). If GDAL options are left NULL, the GDAL defaults are used.
#' @param dir the directory to cache data. Default is `geo_path()`
#' @param product the MODIS product to search for
#' @param date the date of the mosaic to build passed as a vector of length 1 (single date), or
#' 2 (range of dates)
#' @param pattern the HDF file pattern to extract (see `getSubsets()`)
#' @param grid a grid abstraction build with `make_grid()`
#' @param r GDAL resampling method
#' @param overwrite should the files be overwriten?
#' @param  scale_factor product scale factor
#' @param range valid data range
#' @param cog should a COG directory be built?
#' @export
#' @importFrom dplyr mutate filter between tibble
#' @importFrom stringr str_replace
#' @importFrom sf gdal_utils
#' @family modis

mosaicMODIS = function(dir = geo_path(),
                       product,
                       date = NULL,
                       pattern = NULL,
                       grid = NULL,
                       r = "near",
                       overwrite = FALSE,
                       scale_factor = 1,
                       range = NULL,
                       cog = TRUE){

  subdir <- dest <- NULL

  if(is.null(date)){c("1900-01-01", "2100-01-01")}
  if(length(date) == 1){ date = c(date, date) }

  if(is.null(grid)){
    options = c("-of", "GTiff",
                "-r", r)
  } else {
    options = c("-of", "GTiff",
                "-te", grid$ext,
                "-tr", grid$resXY,
                "-t_srs", grid$prj,
                "-r", r)
  }

  home   =  file.path(dir, "MODIS", product, "raw")
  tifs   =  file.path(dir, "MODIS", product, "tiffs")
  cogs   =  file.path(dir, "MODIS", product, "mosaics_cog")
  outdir =  file.path(dir, "MODIS", product, "mosaic")
  dir.create(home, showWarnings = FALSE)
  dir.create(tifs, showWarnings = FALSE)
  dir.create(outdir, showWarnings = FALSE)
  if(cog) { dir.create(cogs, showWarnings = FALSE) }

  ext = ".tif" #ifelse(of == "GTiff", ".tif", ".nc")

  df   = tibble(dir = list.dirs(home)) %>%
    dplyr::mutate(subdir = basename(dir)) %>%
    dplyr::filter(!subdir %in% c("raw", "mosaics")) %>%
    dplyr::filter(between(as.Date(subdir), as.Date(date[1]), as.Date(date[2]))) %>%
    dplyr::mutate(dest = file.path(outdir, paste0(subdir, ext))) %>%
    dplyr::mutate(cog = file.path(cogs, paste0(subdir, ext)))

  if(!overwrite){
    df = dplyr::filter(df, !file.exists(dest))
  }

  message(nrow(df), " mosaics to make")

  for(i in 1:nrow(df)){

    xx = list.files(df$dir[i], full.names = TRUE)
    sub = file.path(tifs, df$subdir[i])
    dir.create(sub, showWarnings = FALSE)

    for(j in 1:length(xx)){
      ss =  terra::rast(xx[j])[[pattern]]
      ss = ss * scale_factor

      if(!is.null(range)){
        ss =  terra::clamp(ss, lower = min(range), upper = max(range), values = FALSE)
      }

      out = strsplit(basename(xx[j]), "[.]")[[1]]

      outfile = file.path(sub, paste0(out[3], "_",
                                      strsplit(pattern, "[:]")[[1]][2], ".tif"))
      terra::writeRaster(ss,  outfile, overwrite = TRUE)
    }

  unlink(df$dest[i])
  sf::gdal_utils(util = "warp",
                   source =  list.files(sub, full.names = TRUE),
                   destination  = df$dest[i],
                   options = options)


  if(cog){
    unlink(df$cog[i])
   sf::gdal_utils("translate",
                  source = df$dest[i],
                  destination  = df$cog[i],
                  options = c("-co", "TILED=YES",
                              "-co",  "COPY_SRC_OVERVIEWS=YES",
                              "-co",  "COMPRESS=DEFLATE"))

  }

    message(product, ": ", df$subdir[i])
  }
}

#' 8day to monthly mean
#' @description takes 8 day MODIS tiles and generates a monthly mean
#' mean((tile / 8)) / days_in_month
#' @param dir the directory with 8 day data
#' @param measurement are the 8 day files a single best `representative` (default) or a daily `sum`
##' @param date the date of the months to process to build passed as a vector of length 1 (single date),
##' or 2 (range of dates)
#' @param overwrite should the files be overwriten?
#' @param cog should a COG file (Cloud Optimized Geotif) be produced?

#' @export
#' @importFrom raster stack writeRaster mean
#' @importFrom dplyr filter group_indices mutate group_by
#' @family modis

day8_to_month = function(dir = '/Volumes/Transcend/ngen/MODIS/MOD15A2H.006/mosaics_cogs',
                         measurement = "representative",
                         date = NULL,
                         overwrite = FALSE,
                         cog = TRUE){

  files <- subdir <- month <- year <- g <- NULL

  if(length(date) == 1){ date = c(date, date) }

  out = file.path(dirname(dir), "monthly_mean")
  dir.create(out, showWarnings = FALSE)
  cog_dir = file.path(dirname(dir), "monthly_mean_cog")
  if(cog) { dir.create(cog_dir, showWarnings = FALSE) }

  df = tibble(files = list.files(dir, pattern = ".tif$", full.names = TRUE)) %>%
    mutate(subdir  = gsub(".tif", "", basename(files)),
           subdir  = subdir,
           year    = format(as.Date(subdir), "%Y"),
           month   = format(as.Date(subdir), "%m"))

  if(!is.null(date)){
    dates  = seq.Date(as.Date(date[1]), as.Date(date[2]), by = "m")
    df =  df %>%
      dplyr::filter(month %in% unique(format(dates, "%m"))) %>%
      dplyr::filter(year %in% unique(format(dates, "%Y")))
  }

  df = df %>%
    group_by(year,month) %>%
    mutate(subdir = paste(year,month,"01", sep = "-")) %>%
    mutate(days_in_month = .ndays(subdir),
           g = dplyr::cur_group_id())

  message(length(unique(df$g)), " months to process...")

  for(i in unique(df$g)){
    subs   = filter(df, g == i)
    dest  = file.path(out, paste0(subs$year[1], "-", subs$month[1], ".tif"))

    if(!file.exists(dest) | all(overwrite, file.exists(dest))){

      if(measurement == "sum"){
        suppressWarnings({
          o = (terra::mean(terra::rast(subs$files) / 8) * subs$days_in_month[1])
          terra::writeRaster(o, dest, overwrite = TRUE)
        })
      } else {
        suppressWarnings({
          o = terra::mean(terra::rast(subs$files), na.rm = TRUE)
          terra::writeRaster(o, dest, overwrite = TRUE)
        })
      }


      if(cog){
          dest_cog  = file.path(cog_dir, paste0(subs$year[1], "-", subs$month[1], ".tif"))
          unlink(dest_cog)
          sf::gdal_utils("translate",
                         source = dest,
                         destination  = dest_cog,
                         options = c("-co", "TILED=YES",
                                     "-co",  "COPY_SRC_OVERVIEWS=YES",
                                     "-co",  "COMPRESS=DEFLATE"))

        }


      message(subs$subdir[1], " complete")
    } else {
      message(subs$subdir[1], " exists")
    }
  }
}

.ndays <- function(d) {
  last_days <- 28:31
  rev(last_days[which(!is.na(
    as.Date( paste( substr(d, 1, 8),
                    last_days, sep = ''),
             '%Y-%m-%d')))])[1]
}


annual_netcdf = function(dir, year, outDir, keep = FALSE, name, long_name){

  tifs = grep(year, list.files(dir, full.names = TRUE, pattern = "tif"), value = TRUE)
  ncs = gsub("tif", "nc", tifs)
  unlink(ncs)
  for(x in 1:length(tifs)){
    sf::gdal_utils(util = "translate",
                   source = tifs[x],
                   destination  = ncs[x],
                   options = c("-of", "netCDF"))

    system(paste0('ncrename -v Band1,', name, " ", ncs[x]))
    system(paste0('ncatted -O -a long_name,',name,',o,c,"',long_name,'" ', ncs[x]))
    diff = as.Date(gsub(".nc", "", basename(ncs[x]))) - as.Date('1970-01-01')
    system(paste0("ncap2 -s 'defdim(\"time\",1);time[time]=", diff, ";time@long_name=\"Time Since 1970-01-01\" ' -O ", ncs[x], " ", ncs[x]))
  }

  path = file.path(outDir, paste0(year,".nc"))
  unlink(path)
  system(paste('ncecat -u time', paste(ncs, collapse = " "),  path))
  system(paste('ncks -4 -L 3 -O', path, path))
  if(!keep){   unlink(ncs) }
}

complete_netcdf = function(dir, out, keep = FALSE, name, long_name){

  tifs = list.files(dir, full.names = TRUE, pattern = "tif")
  ncs = gsub("tif", "nc", tifs)
  unlink(ncs)
  for(x in 1:length(tifs)){
    sf::gdal_utils(util = "translate",
                   source = tifs[x],
                   destination  = ncs[x],
                   options = c("-of", "netCDF"))

    system(paste0('ncrename -v Band1,', name, " ", ncs[x]))
    system(paste0('ncatted -O -a long_name,',name,',o,c,"',long_name,'" ', ncs[x]))
    diff = as.Date(gsub(".nc", "", basename(ncs[x]))) - as.Date('1970-01-01')
    system(paste0("ncap2 -s 'defdim(\"time\",1);time[time]=", diff, ";time@long_name=\"Time Since 1970-01-01\" ' -O ", ncs[x], " ", ncs[x]))
  }

  unlink(out)
  system(paste('ncecat -u time', paste(ncs, collapse = " "),  out))
  system(paste('ncks -4 -L 3 -O', out, out))
  if(!keep){   unlink(ncs) }
}

