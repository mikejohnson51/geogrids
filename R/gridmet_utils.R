# Gridmet downloads
.na.omit = function(x){
  x[!is.na(x)]
}

#' Download GridMet Daily files
#' @description Daily gridmet files have daily values, and each year is stored as
#' a seperate file.
#' @param var what variables to download (1-many)
#' @param year what years (1979-2020)
#' @param dir the directory to cache data. Default is `geo_path()`
#' @return file to disk
#' @export
#' @importFrom httr GET write_disk progress
#' @family gridmet
#'
downloadGridMET = function(var, year, dir = geo_path()){

  Var1 <- Var2 <- NULL

  outdir = file.path(dir, "GRIDMET")
  base.url = 'https://www.northwestknowledge.net/metdata/data/'

  grid = tibble(expand.grid(var, year)) %>%
    mutate(url = paste0(base.url, Var1, '_', Var2, '.nc'),
           outpath = file.path(outdir, Var1, paste0(Var1,'_', Var2,'.nc')))

  lapply(var, function(x) {  dir.create(file.path(outdir, x), showWarnings = FALSE) })

  lapply(1:nrow(grid), function(i){
    if(!file.exists(grid$outpath[i])){ httr::GET(grid$url[i],
                                        httr::write_disk(grid$outpath[i]),
                                        httr::progress()) }
  })
}


#' Build Annual Averages and Period Normals
#' @description Calculate the annual mean from raw GridMet data for the years requested. Afterward, compute a normal for that period.
#' @param var what variables to download (1-many)
#' @param year what years (1979-2020)
#' @param dir the directory to cache data. Default is `geo_path()`
#' @return file to disk
#' @export
#' @importFrom httr GET write_disk progress
#' @family gridmet

build_normals = function(var,  years = c(2001:2020), dir = geo_path()){

  full <- NULL

  outdir = file.path(dir, "GRIDMET", "summeries")
  dir.create(outdir, showWarnings = FALSE)

  for(v in 1:length(var)){

    message("Processing: ", var[v] )
    fp = file.path(dir, 'GRIDMET', var[v])

    f  = tibble(full = list.files(fp, full.names = TRUE, pattern = ".nc")) %>%
      filter(grepl(paste(years, collapse = "|"), full)) %>%
      mutate(out = paste0(outdir, "/",
                          gsub(paste0(".", .getExtension(full)),
                                      "_annual-mean.tif", basename(full))))
    for(i in 1:nrow(f)){
      if(!file.exists(f$out[i])){
         ss = raster::raster(f$full[i])
         nc = RNetCDF::open.nc(f$full[i])

         atts     = ncmeta::nc_coord_var(nc)
         T_name   = .na.omit(unique(atts$T))
         X_name   = .na.omit(unique(atts$X))
         Y_name   = .na.omit(unique(atts$Y))
         var_name = atts$variable[!atts$variable %in% c(T_name, X_name, Y_name)]

         var_data = RNetCDF::var.get.nc(nc, var_name)
         var_data_mean = apply(var_data, c(1,2), mean)
         ss[] = as.vector(var_data_mean)
         RNetCDF::close.nc(nc)
         raster::writeRaster(ss, filename = f$out[i], overwrite = TRUE)
         message("\tFinished: ", basename(f$out[i]))
      }
    }

    fin = paste0(outdir, "/", var[v], "_normal_", min(years), "-", max(years), ".tif")

    if(!file.exists(fin)){
      o = raster::mean(raster::stack(f$out))
      raster::writeRaster(o,filename = fin)
    }
  }
}

#' Generate Aridity Index (PET/PR)
#' @param years what years (1979-2020). `pr` and `pet` data must already be downloaded. See `downloadGridMET()`.
#' @param dir the directory to cache data. Default is `geo_path()`
#' @return
#' @export
#' @importFrom raster raster writeRaster
#' @family gridmet

calculate_ai = function(years = c(2001:2020), dir = geo_path()){
  full <- NULL
  outdir = file.path(dir, "GRIDMET", "summeries")
  dir.create(outdir, showWarnings = FALSE)
  pr_files  = list.files(outdir, full.names = TRUE, pattern = "pr")
  pet_files = list.files(outdir, full.names = TRUE, pattern = "pet")

  for(i in 1:length(years)){
    pr_file  = grep(years[i], pr_files, value = TRUE)
    pr_file  = grep('annual', pr_file, value = TRUE)
    pet_file = grep(years[i], pet_files, value = TRUE)
    pet_file = grep('annual', pet_file, value = TRUE)
    ai_file  = gsub("pr","ai", pr_file)
    ai       = raster(pet_file) / raster(pr_file)
    writeRaster(ai, ai_file, overwrite = TRUE)
    message(ai_file)
  }

  # Get Normals
  pr_norm  = grep('normal', pr_files, value = TRUE)
  pet_norm = grep('normal', pet_files, value = TRUE)
  ai_norm_file  = gsub("pr", "ai", pr_norm)
  ai_norm  = raster(pet_norm) / raster(pr_norm)
  writeRaster(ai_norm, ai_norm_file, overwrite = TRUE)
  message(ai_norm_file)
}

#' Jennings Snow Threshold
#' @description [Jennings, Keith S.; Winchell, Taylor S.; Livneh, Ben; Molotch, Noah P. (2019), Data from: Spatial variation of the rain-snow temperature threshold across the Northern Hemisphere, Dryad, Dataset, ](https://doi.org/10.5061/dryad.c9h35)
#' @param dir the directory to cache data. Default is `geo_path()`
#' @return file to Jennings Snow Threshold
#' @export
#' @importFrom httr GET write_disk progress
#' @family snow

jennings_snow_threshold = function(dir = geo_path()){

  url = 'https://datadryad.org/stash/downloads/file_stream/67087'
  out = file.path(geo_path(), 'jennings_snow_threshold.tif')

  if(!file.exists(out)){ httr::GET(url,
                                   httr::write_disk(out),
                                   httr::progress())
  }

  return(out)
  message('Downloaded: ',  out)
}


