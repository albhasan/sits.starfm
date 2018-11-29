# compute bricks of vegetation indexes

library(devtools)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::build()
devtools::install()

library(sits.starfm)
vi_bricks <- compute_vi("/home/alber/shared/brick")
print(vi_bricks)

# library(tidyverse)
# setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
# source("./R/util.R")
# source("./R/gdal_util.R")
# load("./R/sysdata.rda")
# brick_path <- "/home/alber/shared/brick"
