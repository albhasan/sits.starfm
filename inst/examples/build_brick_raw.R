#!/usr/bin/env Rscript

# Build a brick by just piling Landsat 8 images (23 images a year)

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
suppressMessages(suppressPackageStartupMessages(library(dplyr)))
suppressMessages(suppressPackageStartupMessages(library(optparse)))
library(sits.starfm)

option_list = list(
    make_option(c("-s", "--scene"), type = "character", default = NULL,
        help = "landsat scene e.g. 225063",        metavar = "character"),
    make_option(c("-f", "--from"),  type = "character", default = "NULL",
        help = "brick start date e.g. 2015-08-01", metavar = "character"),
    make_option(c("-t", "--to"),    type = "character", default = "NULL",
        help = "brick end date e.g. 2016-08-01",   metavar = "character")
)
opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser)

if (any(is.null(opt$scene), is.null(opt$from), is.null(opt$to))) {
    print_help(opt_parser)
    stop("Invalid input!", call.=FALSE)
}

brick_scene <- opt$scene
brick_from  <- opt$from
brick_to    <- opt$to

# configuration
brick_scene  <- "225063"
#brick_scene  <- "226064"
#brick_scene  <- "233067"
brick_from   <- "2016-08-01"
brick_to     <- "2017-07-30"
#brick_from   <- "2015-08-01"
#brick_to     <- "2016-07-30"
#brick_from   <- "2014-08-01"
#brick_to     <- "2015-07-30"
#brick_from   <- "2013-08-01"
#brick_to     <- "2014-07-30"
brick_bands  <- c("sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band6", "sr_band7")
brick_path   <- "/home/alber/shared/brick_raw"
brick_n_img  <- 23
brick_prefix <- "LC8SR-RAW"
landsat_path <- "/home/alber/landsat8"
modis_path   <- "/home/alber/MOD13Q1"
scene_shp    <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp     <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
temp_dir     <- "/home/alber/shared/tmp"
no_data      <- -9999
gdal_options <- "BIGTIFF=YES"
gdal_format  <- "GTiff"

# handle temporal directories
tmp_dir <- temp_dir %>%
    file.path(paste("L8MOD", brick_scene, lubridate::year(brick_to), sep = "_"))
if (!dir.exists(tmp_dir))
    dir.create(tmp_dir)

b_raw <- build_brick_raw(landsat_path = landsat_path,
                         modis_path = modis_path,
                         scene_shp = scene_shp,
                         tile_shp = tile_shp,
                         brick_scene = brick_scene,
                         brick_from = brick_from,
                         brick_to = brick_to,
                         brick_bands = brick_bands,
                         brick_prefix = brick_prefix,
                         brick_path = brick_path,
                         brick_n_img = brick_n_img,
                         gdal_options = gdal_options,
                         gdal_format = gdal_format,
                         no_data = no_data,
                         tmp_dir = tmp_dir)

print("Finished!")

