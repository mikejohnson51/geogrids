library(knitr)

setwd("/Users/mjohnson/github/geogrids/")

knit("to_build/01-basics.Rmd", "vignettes/01-basics.Rmd")
knit("to_build/02-dem_example.Rmd", "vignettes/02-dem_example.Rmd")

knit("to_build/03-characteristics.Rmd", "vignettes/03-characteristics.Rmd")

files = list.files(".", ".png", full.names = TRUE)
fs::file_move(files, paste0('vignettes/', basename(files)))

pkgdown::build_site()
