
# Build a brick by just piling Landsat 8 images

library(tidyverse)

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
library(devtools)
devtools::load_all()

# configuration
#brick_scene  <- "225063"
#brick_scene  <- "226064"
#brick_scene  <- "233067"
#brick_from   <- "2016-08-01"
#brick_to     <- "2017-07-30"
#brick_from   <- "2015-08-01"
#brick_to     <- "2016-07-30"
#brick_from   <- "2014-08-01"
#brick_to     <- "2015-07-30"
#brick_from   <- "2013-08-01"
#brick_to     <- "2014-07-30"
brick_bands  <- c("sr_band2", "sr_band4", "sr_band5", "sr_band7")
brick_path   <- "/home/alber/shared/brick_simple"
brick_n_img  <- 4
brick_prefix <- "LC8SR-SIMPLE"
landsat_path <- "/home/alber/landsat8"
modis_path   <- "/home/alber/MOD13Q1"
scene_shp    <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp     <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
temp_dir     <- "/home/alber/shared/tmp"

# handle temporal directories
tmp_dir <- temp_dir %>% file.path(paste("L8MOD", brick_scene, lubridate::year(brick_to), sep = "_"))
if (!dir.exists(tmp_dir)) dir.create(tmp_dir)

# select best images per year
brick_imgs <- build_brick_tibble2(landsat_path, modis_path, scene_shp, tile_shp,
                                  scenes = brick_scene, from = brick_from,
                                  to = brick_to, add_neighbors = FALSE) %>%
    dplyr::mutate(year = lubridate::year(img_date)) %>%
    dplyr::group_by(year) %>%
    dplyr::top_n(-as.integer(brick_n_img/2), cloud_cov) %>%
    dplyr::ungroup() %>%
    ensurer::ensure_that(nrow(.) == brick_n_img, err_desc = "Not enough images!")
brick_files <- brick_imgs %>% pile_up(file_col = "files",
    brick_bands = brick_bands, out_dir = brick_path, 
    brick_prefix = brick_prefix)
print(as.data.frame(brick_files))

compute_vi(brick_path, brick_pattern = paste0("^", brick_prefix, "_.*[.]tif$"),
                       vi_index = c("ndvi", "savi")) %>%
    print()

