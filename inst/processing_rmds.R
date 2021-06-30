# Pre-compiled vignettes that depend on local data

library(knitr)
fs::file_copy("inst/basics.Rmd", 'vignettes/basics.Rmd.orig', overwrite = TRUE)
knit("vignettes/basics.Rmd.orig", "vignettes/basics.Rmd")

fs::dir_copy('./figure/', "./vignettes/figure", overwrite = TRUE)
fs::dir_delete('./figure')

pkgdown::build_site()
