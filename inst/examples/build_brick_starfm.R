#!/usr/bin/env Rscript

# Build a brick using Landsat 8 and  MOD13Q1 through the STARFM model.
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
suppressMessages(suppressPackageStartupMessages(library(dplyr)))
suppressMessages(suppressPackageStartupMessages(library(optparse)))
library(sits.starfm)

option_list = list(
    make_option(c("-s", "--scene"), type = "character", default = NULL, 
        help = "landsat scene e.g. 225063",        metavar = "character"),
    make_option(c("-y", "--year"),    type = "integer", default = "NULL", 
        help = "brick's PRODES year e.g. 2016",   metavar = "character")
)
opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser)

if (any(is.null(opt$scene), is.null(opt$year))) {
    print_help(opt_parser)
    stop("Invalid input!", call.=FALSE)
}

brick_scene <- opt$scene
brick_year  <- opt$year

img_per_year <- 23
build_brick_starfm(landsat_path    = "/home/alber/landsat8",
                   modis_path      = "/home/alber/MOD13Q1", 
                   scene_shp       = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp",
                   tile_shp        = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp",
                   brick_scene     = brick_scene,
                   brick_year      = brick_year,
                   brick_bands     = c("sr_band4", "sr_band5", "sr_band7"),
                   brick_path      = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_starfm_v2",
                   cloud_threshold = 0.01,
                   n_best_img      = img_per_year,
                   img_per_year    = img_per_year,
                   image_step      = ceiling(365/img_per_year))

print(sprintf("Finished processing brick starfm %s %s", brick_scene, brick_year))

