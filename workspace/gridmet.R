# Gridmet downloads

var = c("pet", "pr", "tmmn", "tmmx")
year = c(2000:2020)
outdir = '/Volumes/Transcend/ngen/gridmet'
grid = expand.grid(var, year)
urls = paste0('https://www.northwestknowledge.net/metdata/data/',
              grid$Var1,
              '_',
              grid$Var2,
              '.nc')

out = file.path(outdir, grid$Var1, paste0(grid$Var1,'_', grid$Var2,'.nc'))

fs::dir_create(file.path(outdir, var))

lapply(1:length(urls), function(i){
  if(!file.exists(out[i])){ httr::GET(urls[i], write_disk(out[i]), progress()) }
})



vars = c("pet", "pr")


for(i in 1:length(vars)){

  fp = file.path(outdir, vars[i])
  f = list.files(fp, full.names = TRUE, pattern = ".nc")
  new_dir = file.path(fp, 'annual')
  fs::dir_create(new_dir)

  nf = file.path(new_dir, basename(f))

  for(i in 1:length(nf)){
    system(paste('ncks -O --mk_rec_dmn day', f[i], nf[i]))
    system(paste("ncra -h -O -y ttl", nf[i], nf[i]))
    message(basename(nf[i]), " complete")
  }

  new_dir2 = file.path(new_dir, 'normal')
  fs::dir_create(new_dir2)
  files = list.files(new_dir, pattern = ".nc", full.names = TRUE)
  system(paste("ncea", paste(files, collapse = " "), file.path(new_dir2, "normal.nc")))
}

## Aridity Index

# Calculated as the annual mean PET/P
# copy pet to ai
norms = list.files(outdir, recursive = TRUE, pattern = "normal.nc", full.names = TRUE)
pet_normal = grep("pet", norms, value = TRUE)
pr_normal  = grep("pr", norms, value = TRUE)
ai_normal = '/Volumes/Transcend/ngen/climate/gridmet/ai/normal.nc'

fs::file_copy(pet_normal, ai_normal)
# copy pr into ai
system(paste("ncks -A -v precipitation_amount", pr_normal, ai_normal))
# calc ai
system(paste("ncap2 -A -s 'ai=potential_evapotranspiration/precipitation_amount'", ai_normal, ai_normal))

system(paste0('ncrename -v ai,', "aridity", " ", ai_normal))
system(paste0('ncatted -O -a long_name,','aridity',',o,c,"',"aridity index (PET/PPT)",'" ', ai_normal))
system(paste0('ncatted -O -a standard_name,','aridity',',o,c,"',"ai",'" ', ai_normal))
system(paste0('ncatted -O -a description,','aridity',',o,c,"',"Mean Annual PET divided by Mean Annual PPT",'" ', ai_normal))


## Snow Thresholds

file = '/Volumes/Transcend/ngen/climate/snow-thresholds/jennings_et_al_2018_file4_temp50_raster.tif'
out = '/Volumes/Transcend/ngen/climate/snow-thresholds/jennings_et_al_2018_conus.tif'
unlink(out)
sf::gdal_utils(util = "warp",
               source = file,
               destination  = out,
               options = c("-of", "GTiff",
                           '-t_srs', t_srs,
                           '-tr', tr,
                           '-te', te,
                           '-r', 'bilinear'))


# Tavg

for(i in 2000:2020){
  outfile = paste0('/Volumes/Transcend/ngen/gridmet/tavg/tavg_',i,'.nc')
  if(!file.exists(outfile)){
    call = paste0('cdo ensmean /Volumes/Transcend/ngen/gridmet/tmmn/tmmn_',i,'.nc /Volumes/Transcend/ngen/gridmet/tmmx/tmmx_',i,'.nc ', outfile)
    message("Making tavg ", i)
    system(call)
    system(paste('ncks -O -4 -L 5', outfile, outfile))
  }

}

