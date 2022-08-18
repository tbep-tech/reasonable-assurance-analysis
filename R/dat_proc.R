library(here)

# copy 2020 lulc data -------------------------------------------------------------------------

download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/lulc2020.RData", 
              destfile = here('data/lulc2020.RData'))

# copy 2020 subtidal data ---------------------------------------------------------------------
 
download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/sgdat2020.RData", 
              destfile = here('data/sgdat2020.RData'))
