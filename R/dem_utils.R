#' Download the 1 (~30 meter) or 1/3 (~10 meter) arc-second National Elevation Dataset.
#' @param AOI An `sf` object to serve as a template for cropping.
#' @param res A character string representing the desired resolution of the NED. '1'
#' indicates the 1 arc-second NED (the default), while '13' indicates the 1/3 arc-second dataset.
#' @param outdir A character string indicating where raw downloaded files should be put.
#' @export
#' @importFrom sf read_sf st_transform st_filter st_crs st_union
#' @importFrom utils download.file
#' @importFrom dplyr mutate pull
#' @importFrom httr GET write_disk progress
#' @family Elevation
get_ned <- function(AOI,
                    res = "1",
                    outdir = '/Volumes/Transcend/ngen/DEM') {


  location = NULL

  AOI = AOI %>%
    st_union()

  outdir = file.path(outdir, res)
  suppressWarnings({
    dir.create(outdir, recursive = TRUE)
  })


  tfile = tempfile(fileext = ".gpkg")
  base = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/',res,"/TIFF/")
  indexfile = paste0(base, 'index.gpkg')
  download.file(indexfile, tfile, quiet = TRUE)

  tiles = sf::read_sf(tfile)
  AOI = sf::st_transform(AOI, sf::st_crs(tiles))
  url = sf::st_filter(tiles, AOI) %>%
    mutate(url = gsub('\\./', base, location)) %>%
    pull(url)

  message("Downloading ", length(url), " files...")

  local = file.path(outdir, basename(url))

  o = lapply(1:length(url), function(i){
    if(!file.exists(local[i])){ httr::GET(url[i],
                                          httr::write_disk(local[i]),
                                          httr::progress()) }
  })

  return(local)
}



merge_rasters <- function(input_rasters,
                          output_raster = tempfile(fileext = ".tif"),
                          options = character(0)) {

  unlink(output_raster)
    sf::gdal_utils(
        util = "warp",
        source = as.character(input_rasters),
        destination = output_raster,
        options = options
      )
}


