#!/usr/bin/env Rscript
.Deprecated("Use bash scripts instead.")
# Build a brick by piling Sentinel2 images

suppressMessages(suppressPackageStartupMessages(library(dplyr)))
suppressMessages(suppressPackageStartupMessages(library(optparse)))
library(sits.starfm)

option_list = list(
    make_option(c("-s", "--tile"), type = "character", default = NULL,
        help = "Sentinel2 tile. e.g. T19LGK",
        metavar = "character"),
    make_option(c("-f", "--from"),  type = "character", default = "NULL",
        help = "Brick start date e.g. 2016-08-01", metavar = "character"),
    make_option(c("-t", "--to"),    type = "character", default = "NULL",
        help = "Brick end date e.g. 2017-08-01",   metavar = "character")
)

opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser)

if (any(vapply(opt, is.null, logical(1)))) {
    print_help(opt_parser)
    stop("Invalid input!", call.=FALSE)
}

brick_tile    <- opt$tile
brick_from    <- opt$from
brick_to      <- opt$to


brick_summary <- build_brick_sentinel2(in_dir, brick_tile, brick_from, brick_to,
                                       brick_bands, brick_n_img, out_dir)

saveRDS(brick_summary, file = file.path(getwd(), "inst", "examples", paste(brick_tile, brick_product,brick_from, "summary.rds", sep = '_')))
print("Finished!")

