# Build DEM
library(sf)
library(dplyr)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAZMS5YKH7GRON5L42",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-1")

gdb = '/Volumes/Transcend/WBD_National_GDB/WBD_National_GDB.gdb'
layers = st_layers(gdb)
hucs = read_sf(gdb, 'WBDHU2') %>%
  dplyr::filter(huc2 <= 18) %>%
  dplyr::select(huc2)


for(i in 1:18){
  AOI = hucs[i,] %>% st_make_valid()

  output =   paste0('/Volumes/Transcend/ngen/DEM/huc', AOI$huc2,'-res-1.tif')

  if(!file.exists(output)){
    files = get_ned(AOI, res = '1')
    merge_rasters(files, output_raster = output)
  }

  message(i)
}

