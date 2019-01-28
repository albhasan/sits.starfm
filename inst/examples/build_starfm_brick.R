#!/usr/bin/env Rscript

# build a brick using Landsat 8 and MOD13Q1 images

library(devtools)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()

dump_file <- file.path(paste0("last.dump_", stringr::str_replace(system('uname -n', intern = TRUE), "-", "_")))
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

#debugonce(run_starFM)
brick_233067_2015 <- build_brick(landsat_path    = "/home/alber/landsat8",
                                 modis_path      = "/home/alber/MOD13Q1", 
                                 scene_shp       = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp",
                                 tile_shp        = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp",
                                 brick_scene     = "225063",
                                 brick_year      = 2017,
                                 brick_bands     = c("sr_band4", "sr_band5", "sr_band7"),
                                 brick_path      = "/home/alber/shared/brick",
                                 cloud_threshold = 0.99,
                                 n_best_img      = 4,
                                 img_per_year    = 23,
                                 image_step      = ceiling(365/img_per_year),
                                 temp_dir        = "/home/alber/shared/tmp")

warnings()
options(error = NULL)

