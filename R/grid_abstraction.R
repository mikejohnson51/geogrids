#' Build Grid Abstraction
#' @description Define a grid abstraction for a file or an extent, res/dim, and projection
#' @param file file path
#' @param ext c(xmin, ymin, xmax, ymax)
#' @param dimXY c(width, height)
#' @param resXY c(xres, yres)
#' @param prj srs_def
#' @importFrom raster res extent raster crs
#' @family grid
#' @return list
#' @export

make_grid = function(file = NULL,
                     ext = NULL,
                     dimXY = NULL,
                     resXY = NULL,
                     prj = NULL){


  if(!is.null(file)){
    r = suppressWarnings({ raster::raster(file) })

    ext = raster::extent(r)

    list(ext = c(ext[1],ext[3],ext[2],ext[4]),
         dimXY = dim(r)[2:1],
         resXY = raster::res(r),
         prj = as.character(raster::crs(r)))

  } else {

    if(is.null(prj)){ stop("prj is required") }
    if(is.null(ext)){ stop("ext is required") }
    if(is.null(dimXY) & is.null(resXY)){ stop("dimXY or resXY must be provided") }

    if(is.null(dimXY) & is.null(resXY)){
      tmp = c(abs(ext[1] - ext[3]) / resXY[1], abs(ext[2] - ext[4]) / resXY[2])
      if(sum(tmp == dimXY) == 2){ stop('dimXY and resXY do not agree') }
    }

    if(is.null(dimXY)){
      dimXY = c(abs(ext[1] - ext[3]) / resXY[1], abs(ext[2] - ext[4]) / resXY[2])
    }

    if(is.null(resXY)){
      resXY = c(abs(ext[1] - ext[3]) / dimXY[1], abs(ext[2] - ext[4]) / dimXY[2])
    }

    list(ext = ext, dimXY = dimXY, resXY = resXY, prj = prj)
  }
}


#' Wrap
#' @param file a vector of files paths
#' @param grid a grid abstraction to warp to (see `make_grid()`)
#' @param r resampling method see gdalwarp for options
#' @param filename character. Output filename
#' @return SpatRast object
#' @export
#' @importFrom sf gdal_utils
#' @importFrom terra rast
#' @family grid

geogrid_warp = function(file, grid = NULL, r = "near", filename = NULL){

  if(grepl("Rast", class(file))){
    file_og = tempfile(pattern = "og", fileext = ".tif")
    terra::writeRaster(file, file_og)
  } else {
    file_og = file
  }

  if(!is.null(filename)){
    filename = tempfile(pattern = "new", fileext = ".tif")
  }

  sf::gdal_utils("warp",
                   source = file_og,
                   destination   = filename,
                   options = c("-of", "GTiff",
                               "-te", grid$ext,
                               "-tr", grid$resXY,
                               "-t_srs", grid$prj,
                               "-r", r))

  return(terra::rast(filename))

}

