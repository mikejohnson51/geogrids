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
  dir.create(tmpdir, showWarnings = FALSE)

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

  dates_to_process = dates[!dates %in% as.Date(oo$subdir) | oo$count != length(tiles)]

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
#' @importFrom sf gdal_utils
#' @importFrom utils glob2rx
#' @family grid

getSubsets = function(file = NULL, product = NULL){

  subdir <- NULL

  if(is.null(file)){
    xx = geo_cache_list(pattern = "hdf") %>%
      filter(grepl(product, subdir))

    file = xx$fullname[1]
  }

  if(!file.exists(file)){stop("File not found.")}

  gdalinfo_raw <- sf::gdal_utils(util = "info", file, quiet  = TRUE)
  rr = strsplit(gdalinfo_raw, "\\n")[[1]]

  subdataset_rawnames <-  rr[grepl(glob2rx("*SUBDATASET*NAME*"), rr)]
  subdataset_names <- sapply(X = seq(length(subdataset_rawnames)),
                             FUN = function(X) {
                               split1 <- strsplit(subdataset_rawnames[X], "=")
                               return(gsub("\"", "", split1[[1]][2]))
                             })

  gsub(file,"[file]",subdataset_names)
}

#' Mosaic/Warp Raw MODIS Tiles
#' @description Mosaics all MODIS HDF files for a given date to a new file with a flexible extent (te),
#' resolution (tr), CRS (t_srs). If GDAL options are left NULL, the GDAL defaults are used.
#' @param dir the directory to cache data. Default is `geo_path()`
#' @param prefix the prefix for the outputfiles, shoudl be specfic to the requested grid
#' @param product the MODIS product to search for
#' @param pattern the HDF file pattern to extract (see `getSubsets()`)
#' @param date the date of the mosaic to build passed as a vector of length 1 (single date), or
#' 2 (range of dates)
#' @param grid a grid abstraction build with `make_grid()`
#' @param of GDAL output file type (default = GTiff)
#' @param r GDAL resampling method
#' @param overwrite should the files be overwriten?
#' @export
#' @importFrom dplyr mutate filter between tibble
#' @importFrom stringr str_replace
#' @importFrom sf gdal_utils
#' @family modis

mosaicMODIS = function(dir = geo_path(),
                       prefix = "",
                       product,
                       date = NULL,
                       pattern = NULL,
                       grid = NULL,
                       r = "near",
                       overwrite = FALSE,
                       of = "GTiff"){

  subdir <- dest <- NULL

  if(is.null(date)){c("1900-01-01", "2100-01-01")}
  if(length(date) == 1){ date = c(date, date) }

  if(is.null(grid)){
    options = c("-of", of,
                "-r", r)
  } else {
    options = c("-of", of,
                "-te", grid$ext,
                "-tr", grid$resXY,
                "-t_srs", grid$prj,
                "-r", r)
  }

  home   =  file.path(dir, "MODIS", product, "raw")
  outdir = file.path(dir,  "MODIS", product, "mosaics")
  dir.create(outdir, showWarnings = FALSE)

  ext = ifelse(of == "GTiff", ".tif", ".nc")

  df   = tibble(dir = list.dirs(home)) %>%
    dplyr::mutate(subdir = basename(dir)) %>%
    dplyr::filter(!subdir %in% c("raw", "mosaics")) %>%
    dplyr::filter(between(as.Date(subdir), as.Date(date[1]), as.Date(date[2]))) %>%
    dplyr::mutate(dest = file.path(outdir, paste0(prefix, "_", subdir, ext)))

  if(!overwrite){
    df = dplyr::filter(df, !file.exists(dest))
  }

  message(nrow(df), " mosaics to make")

  for(i in 1:nrow(df)){

    files = stringr::str_replace(pattern,
                                 "\\[file\\]",
                                 list.files(df$dir[i], full.names = TRUE))

    unlink(df$dest[i])

    sf::gdal_utils(util = "warp",
                   source = files,
                   destination  = df$dest[i],
                   options = options)

    message(product, ": ", df$subdir[i])
  }
}

#' 8day to monthly mean
#' @description takes 8 day MODIS tiles and generates a monthly mean
#' mean((tile / 8)) / days_in_month
#' @param dir the directory to cache data. Default is `geo_path()`
#' @param product the MODIS product to search
##' @param date the date of the months to process to build passed as a vector of length 1 (single date),
##' or 2 (range of dates)
#' @param overwrite should the files be overwriten?
#' @param prefix the prefix to search for
#' @export
#' @importFrom raster stack writeRaster mean
#' @importFrom dplyr filter group_indices mutate group_by
#' @family modis

day8_to_month = function(dir = geo_path(),
                         product = NULL,
                         date = NULL,
                         prefix = NULL,
                         overwrite = FALSE){

  files <- subdir <- month <- year <- g <- NULL

  if(length(date) == 1){ date = c(date, date) }

  here = file.path(dir, "MODIS", product, "mosaics")

  out = file.path(dir, "MODIS", product, "monthly_means")
  dir.create(out, showWarnings = FALSE)

  df = tibble(files = list.files(here, recursive = TRUE, pattern = prefix,  full.names = TRUE))
  if(is.null(prefix)){ prefix = ""}

  df = df %>%
    mutate(subdir  = gsub(".tif", "", basename(files)),
           subdir  = gsub(paste0(prefix, "_"), "", subdir),
           year    = format(as.Date(subdir), "%Y"),
           month   = format(as.Date(subdir), "%m"))

  if(!is.null(date)){
    df =  df %>%
      dplyr::filter(month %in% unique(format(as.Date(date), "%m"))) %>%
      dplyr::filter(year %in% unique(format(as.Date(date), "%Y")))
  }


  df = df %>%
    group_by(year,month) %>%
    mutate(subdir = paste(year,month,"01", sep = "-")) %>%
    mutate(days_in_month = .ndays(subdir))

  df$g = dplyr::group_indices(df)

  message(length(unique(df$g)), " months to process...")

  for(i in unique(df$g)){
    subs   = filter(df, g == i)
    dest = file.path(out, basename(subs$files[1]))

    if(!file.exists(dest) | all(overwrite, file.exists(dest))){
      suppressWarnings({
        o = (terra::mean(terra::rast(subs$files)/8) * subs$days_in_month[1])
        terra::writeRaster(o,dest)
      })
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

