#!/usr/bin/env Rscript

# Build a brick by piling Harmonized Landsat8-Sentinel2 L30 images (46 images a year)

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
suppressMessages(suppressPackageStartupMessages(library(dplyr)))
suppressMessages(suppressPackageStartupMessages(library(optparse)))
library(sits.starfm)

option_list = list(
    make_option(c("-s", "--tile"), type = "character", default = NULL, 
        help = "Harmonized Landsat8-Sentinel2 tile. e.g. T19LFJ", metavar = "character"),
    make_option(c("-p", "--product"), type = "character", default = NULL, 
        help = "Harmonized Landsat8-Sentinel2 product e.g. L30", metavar = "character"),
    make_option(c("-f", "--from"),  type = "character", default = "NULL", 
        help = "Brick start date e.g. 2015-08-01", metavar = "character"),
    make_option(c("-t", "--to"),    type = "character", default = "NULL", 
        help = "Brick end date e.g. 2016-08-01",   metavar = "character")
)
opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser)

if (any(vapply(opt, is.null, logical(1)))) {
    print_help(opt_parser)
    stop("Invalid input!", call.=FALSE)
}

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/harmonized_landsat_sentinel2/data/hls"
brick_tile    <- opt$tile
brick_product <- opt$product
brick_from    <- opt$from 
brick_to      <- opt$to
brick_n_img   <- 23
out_dir       <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw"

# TODO: remove
library(devtools)
devtools::load_all()

if (brick_product == "L30"){
    brick_bands <- c(paste0("band0", c(1:7, 9)), paste0("band", 10:11))
} else if (brick_product == "S30") {
    brick_bands <- c(paste0("B0", 1:8), "B8A", "B09", paste0("B", 10:12))
} else{
    stop("Unknown product.")
}
brick_summary <- build_brick_hls_raw(in_dir, brick_tile, brick_product, brick_from, brick_to, 
                                     brick_bands, brick_n_img, 
                                     out_dir)

saveRDS(brick_summary, file = file.path(getwd(), "inst", "examples", paste(brick_tile, brick_product,brick_from, "summary.rds", sep = '_')))
print("Finished!")

