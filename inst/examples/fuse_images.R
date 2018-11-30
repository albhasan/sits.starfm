#!/usr/bin/env Rscript
################################################################################
# FUSE LANDSAT 8 AND MODIS IMAGES
#------------------------------------------------------------------------------
# Last update 2018-10-08
#------------------------------------------------------------------------------
# devtools::load_all()

library(devtools)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::build()
devtools::install()

library(sits.starfm)
# library(tidyverse)
# library(log4r)
# library(knitr)
# library(zoo)
# library(ensurer)

# logger                 <- log4r::create.logger()
# log4r::logfile(logger) <- file.path(paste0("fuse_images_", system('uname -n', intern = TRUE),".log"))
# log4r::level(logger)   <- "DEBUG"
# log4r::info(logger, "...............................................................................")


dump_file <- file.path(paste0("last.dump_",
                              stringr::str_replace(
                                  system('uname -n', intern = TRUE),
                                  "-", "_")))
dump_and_quit <- function() {
    # Save debugging info to file last.dump.rda
    dump.frames(dumpto = dump_file, to.file = TRUE)
    # Quit R with error status
    q(status = 1)
}
options(error = dump_and_quit)
# In a later interactive session ----
# load(dump_file)
# debugger()

#options( warn = 2 ) # TODO: remove for production

landsat_path = "/home/alber/landsat8"
modis_path = "/home/alber/MOD13Q1"
scene_shp = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
brick_path = "/home/alber/shared/brick"
cloud_threshold = 0.99
img_per_year = 23
image_step = 16
temp_dir = "/home/alber/shared/tmp"
# brick_scene = "225063"
# brick_scene = "233067"
# brick_scene = "226064"
#brick_year = 2014
#brick_year = 2015
#brick_year = 2016
#brick_year = 2017
#brick_bands = c("sr_band2", "sr_band4", "sr_band5", "sr_band7")

# NOTE:
# * nrow(.) > 0
# Description: Unable to build a brick for 225063 - 2016
# Description: Unable to build a brick for 226064 - 2014

#debugonce(run_starFM)
brick_233067_2015 <- build_brick(landsat_path = landsat_path,
                                 modis_path = modis_path, scene_shp = scene_shp,
                                 tile_shp = tile_shp,
                                 brick_scene = "225063",
                                 brick_year = 2017,
                                 brick_bands = c("sr_band2", "sr_band4", "sr_band5", "sr_band7"),
                                 brick_path = brick_path,
                                 cloud_threshold = cloud_threshold,
                                 img_per_year = img_per_year,
                                 temp_dir = temp_dir)

warnings()
options(error = NULL)
