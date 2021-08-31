## ---------------------------
## Generate LSTM (and other) traits for a hydrofabric
##
## Author: Mike (mikecp11@gmail.com)
##
## ---------------------------
## Notes:
##
## ---------------------------

# Move down a level in project
setwd("workspace")

## ------------------------------------------------------

# Load creds and packages
library(rmarkdown)
source("../inst/aws.R") # Personal file containing AWS access

# -------------------------------------------------------------------------
# User Defined Parameters

rpu_code   <- '01a'
version    = "beta"
dir = '/Users/mjohnson/github/hydroresolve/releases/'

hy_file = list.files(dir,pattern = "gpkg", full.names = TRUE, recursive = TRUE)

hy_file = grep(rpu_code, grep(version, hy_file, value = TRUE), value = TRUE)

# GO! ---------------------------------------------------------------------geog


## Step 1: NHD-navigate
render(
  input  = 'compute.Rmd',
  params = list(
    data_dir = "/Volumes/Transcend/ngen",
    hydrofabric_file = hy_file,
    years =  3,
    output_dir = file.path(dir, version, rpu_code),
    AWS_bucket =  'formulations-dev/hydrofabric/traits',
    version = version,
    plot = FALSE),
  envir        = new.env(),
  output_file  = paste0('temp/traits_', rpu_code, '.html')
)
