soil.crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66'

if(!param %in% HydroData::bsqDf$name){
  stop("Param not valid. Please check `HydroData::bsqDf$name` for valid options.")
}

df = HydroData::bsqDf[which(bsqDf$name == param), ]

dir = tempdir()

zip  <- tempfile(pattern = paste0(df$name, "_"), tmpdir = dir, fileext = ".bsq.gz")

resp <- httr::GET(df$url, httr::write_disk(zip, overwrite=TRUE), httr::progress())

R.utils::gunzip(zip, remove = F)

myfile = list.files(dir, full.names = T, pattern = paste0(df$name, "_"))
myfile = myfile[grepl(".bsq$", myfile)][1]

layers = ifelse(!is.null(layers), c(1:layers), max(eval(parse( text= df$layers))))
layers = min(max(layers), max(eval(parse( text= df$layers))))

r = raster::raster(crs = soil.crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66'

                   ext = extent(df$xmin, df$xmax, df$ymin, df$ymax),
                   res = df$cellsize)

b = raster::brick(r, values= FALSE, datatype= ifelse(df$scale > 1, "FLT4S", "INT1U"))

inputArrayDim = c(df$rows, df$cols, layers)

dim(b) = inputArrayDim

con <- file(myfile, open= "rb")

thisArray <- array(readBin(con, what= "integer",
                           size = df$bytes, signed= df$signed,
                           n = prod(inputArrayDim)), dim= inputArrayDim)
close(con)

if( length(layers) > 1) {
  b[] <- thisArray[ , , c(1:layers)]
} else {
  b[] <- thisArray
}

if( df$scale > 1) {
  b = raster::scale(b, center= FALSE, scale= rep(df$scale, layers))
}

names(b) <- sprintf( "%s.%d", df$name, 1:layers)


library(data.table)

summarise_soils = function(file, layers){
  
  depths = data.frame(layer = 1:11, thickness_cm = c(5,5,10,10,10,20,20,20,50,50,50)) %>% 
    mutate(depth = cumsum(thickness_cm))
  
  rast = raster::brick(file)[[1:layers]]
  dt  = data.table(rast[])
  
  dt2 = dt[, Map("*", .SD, depths$thickness_cm[1:layers]/100)]
  
  out = rast[[1]]
  out[] = rowSums(dt2) /depths$depth[layers]
  names(out) = gsub(".tif", "", basename(file))
  out
}


clays = summarise_soils('/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.tif', 8)
writeRaster(clays, filename = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/clay-1m-percent.tif")
sands = summarise_soils('/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/sand.tif', 8)
writeRaster(sands, filename = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/sand-1m-percent.tif")
silts = summarise_soils('/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/silt.tif', 8)
writeRaster(silts, filename = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/silt-1m-percent.tif")

f = list.files('/Volumes/Transcend/ngen/soil/CONUS_SOIL/', pattern = "tif", full.names = TRUE)
nc = gsub("tif", "nc", f)
names = c("clay", "sand", "silt")
long_name = c("clay % in top 100cm", "sand% in top 100cm", "silt% in top 100cm")

soil_tif_to_nc = function(path, out, name, long_name){
  system(paste('gdal_translate -of netCDF -co "FORMAT=NC4"', path, out))
  system(paste0('ncrename -v Band1,', name, " ", out))
  system(paste0('ncatted -O -a long_name,',name,',o,c,"',long_name,'" ', out))
}

for(i in 1:length(f)){
  soil_tif_to_nc(f[i], nc[i], name[i], long_name[i])
}

gdal_translate -of netCDF -co "FOMRAT=NC4" foo.tif foo.nc

writeRaster(s, filename = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/sand-silt-clay.nc")
s = stack(clays, sands, silts)




depths

bsqUrls <- list(
  clay=      "ftp://dbftp.essc.psu.edu/pub/data/1997-0006/clay.bsq.gz",
  sand=      "ftp://dbftp.essc.psu.edu/pub/data/1997-0006/sand.bsq.gz",
  silt=      "ftp://dbftp.essc.psu.edu/pub/data/1997-0006/silt.bsq.gz",
  bd=        "ftp://dbftp.essc.psu.edu/pub/data/1996-0127/bd.bsq.gz",
  awc=       "ftp://dbftp.essc.psu.edu/pub/data/1996-0087/awc.bsq.gz",
  kfact=     "ftp://dbftp.essc.psu.edu/pub/data/1998-0111/kfact.bsq.gz",
  layertext= "ftp://dbftp.essc.psu.edu/pub/data/1995-0792/layertext.bsq.gz",
  domfrag=   "ftp://dbftp.essc.psu.edu/pub/data/1997-0004/domfrag.bsq.gz",
  poros=     "ftp://dbftp.essc.psu.edu/pub/data/1996-0171/poros.bsq.gz",
  ph=        "ftp://dbftp.essc.psu.edu/pub/data/2001-0001/ph.bsq.gz",
  hsgpct=    "ftp://dbftp.essc.psu.edu/pub/data/1996-0088/hsgpct.bsq.gz",
  rockdepm=  "ftp://dbftp.essc.psu.edu/pub/data/1996-0001/rockdepm.bsq.gz",
  rockvol=   "ftp://dbftp.essc.psu.edu/pub/data/1997-0005/rockvol.bsq.gz",
  perm=      "ftp://dbftp.essc.psu.edu/pub/data/2000-0004/perm.bsq.gz",
  plast=     "ftp://dbftp.essc.psu.edu/pub/data/2000-0001/plast.bsq.gz")

out = paste0("/Volumes/Transcend/ngen/soil/CONUS_SOIL/bsq/", names(bsqUrls), ".bsq.gz")

lapply(1:length(bsqUrls), FUN = function(x){
  download.file(bsqUrls[[x]], out[x])
  R.utils::gunzip(out[x], remove = TRUE, overwrite = TRUE)
})


bsqVars <- list(
  clay=      "clay fraction",
  sand=      "sand fraction",
  silt=      "silt fraction",
  bd=        "bulk density",
  awc=       "available water capacity",
  kfact=     "surface erodibility factor",
  layertext= "texture class",
  domfrag=   "dominant fragment class",
  poros=     "porosity",
  ph=        "pH",
  hsgpct=    "hydrologic soil group fraction",
  rockdepm=  "depth to bedrock",
  rockvol=   "rock fragment volume",
  perm=      "permeability rate",
  plast=     "plasticity")

bsqDf <-
  data.frame(
    name= unlist( bsqVars), 
    file= out,
    layers= "1:11",
    bytes= 1,
    signed= FALSE,
    scale= 1,
    units= NA,
    stringsAsFactors= FALSE)

bsqDf[ c( "kfact", "perm", "bd", "poros"), "scale"] <- 100 
bsqDf[ "ph", "scale"] <- 10

bsqDf[ "kffact",] <- bsqDf[ "kfact",]
bsqDf[ "kffact", "name"] <- "surface erodibility factor, adjusted"
bsqDf[ "kfact", "layers"] <- "seq( 1, 21, by=2)"
bsqDf[ "kffact", "layers"] <- "seq( 2, 22, by=2)"

bsqDf[ "hsgpct", "layers"] <- "1:5"
bsqDf[ "awc", "layers"] <- "1:3"

bsqDf[ c("clay", "silt", "sand", "poros", "hsgpct", "rockvol", "plast"), "units"] <- "%"
bsqDf[ "rockdepm", "units"] <- "cm"
bsqDf[ "rockdepm", "layers"] <- 1
bsqDf[ "perm", "units"] <- "cm hr**-1"
bsqDf[ "bd", "units"] <- "kg m**-3"
bsqDf[ "awc", "units"] <- "?"
bsqDf[ c( "layertext", "domfrag"), "units"] <- "code"

bsqDf[ c( "perm", "bd"), "bytes"] <- 2

bsqDf <- bsqDf[ c( 1:6, nrow( bsqDf), 7:( nrow( bsqDf) -1)), ] 


df = bsqDf
name = "texture class"

brickFromBsqDf <- function(name, df, overwrite= FALSE) {
  meta = filter(df, name == !!name)
  myfile = gsub(".gz", "", meta$file)
  layers = eval(parse( text= meta$layers))
  
  r = raster::raster(crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66'
                     ext = raster::extent(-2357000, 2259000, 277000, 3173000),
                     res = 1000)
  
  b = raster::brick(r, values= FALSE, datatype= ifelse(meta$scale > 1, "FLT4S", "INT1U"))
  
  inputArrayDim = c(2896, 4616, max(layers))
  
  dim(b) = inputArrayDim
  
  con <- file(myfile, open= "rb")
  
  thisArray <- array(readBin(con, what = "integer",
                             size = meta$bytes, signed= meta$signed,
                             n = prod(inputArrayDim)), dim= inputArrayDim)
  close(con)
  
  if( length(layers) > 1) {
    b[] <- thisArray[ , , layers]
  } else {
    b[] <- thisArray
  }
  
  if( meta$scale > 1) {
    b = raster::scale(b, center = FALSE, scale = rep(meta$scale, layers))
  }
  
  c = brick('/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.tif')
  rockdepmDt <- data.table::data.table(c[])
  
  sf::gdal_utils("translate", 
                 source = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.tif",
                 destination = '/Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.nc',
                 options = c("-of", "NetCDF"),
                 quiet = FALSE)
  
  RNetCDF::open.nc('Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.nc4')
  
  system('gdal_translate -of netCDF /Volumes/Transcend/ngen/soil/CONUS_SOIL/TIFFS/clay.tif Volumes/Transcend/ngen/soil/CONUS_SOIL/clay.nc')
  
  setkey( rockdepmDt, cell)
  sf::gda
  
  names(b) <- sprintf( "%s.%d", row.names(meta), 1:max(layers))
  
  writeRaster(b, filename = "/Volumes/Transcend/ngen/soil/CONUS_SOIL/test.tif")
}
