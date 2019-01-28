#!/usr/bin/env Rscript
# Build a brick by just piling Landsat 8 images
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
suppressMessages(suppressPackageStartupMessages(library(tidyverse)))
suppressMessages(suppressPackageStartupMessages(library(optparse )))
suppressMessages(suppressPackageStartupMessages(library(devtools )))
devtools::load_all()

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
    dplyr::slice(1:(as.integer(brick_n_img/2))) %>%
    dplyr::ungroup() %>%
    ensurer::ensure_that(nrow(.) == brick_n_img, err_desc = "Not enough images!")

# build bricks
brick_files <- brick_imgs %>% pile_up(file_col = "files",
                                      brick_bands = brick_bands, 
                                      brick_prefix = brick_prefix, 
                                      brick_scene = brick_scene, 
                                      out_dir = brick_path)
print(as.data.frame(brick_files))

# build vegetation indexes bricks 
compute_vi(brick_path, brick_pattern = paste0("^", brick_prefix, "_.*[.]tif$"),
                       vi_index = c("ndvi", "savi")) %>%
    print()

# build spectral mixture bricks
brick_imgs <- brick_imgs %>% dplyr::mutate(mixture = purrr::map(.$files, 
    function(x){
        x %>% unlist() %>% compute_mixture_model(out_dir = brick_path) %>%
            tibble::enframe(name = "end_member", value = "file_path") %>%
            tidyr::spread(key = "end_member", value = "file_path") %>%
            return()
    }))
brick_imgs %>% tidyr::unnest(mixture) %>% 
    dplyr::select(dark, substrate, vegetation) %>%
    lapply(function(x, out_dir, prefix){
               img_md <- x[1] %>% parse_img_name()
               img_date <- format(as.POSIXct(img_md["acquisition"], 
                                  format = "%Y%m%d"), "%Y-%m-%d")
               img_band <- unlist(strsplit(img_md[length(img_md)], 
                                  split = "[.]"))[1] 
               out_fn <- file.path(out_dir, paste(prefix, img_md["path_row"],
                                   img_date, img_band, "STACK_BRICK.tif", sep = '_'))
               x %>% gdal_merge(out_filename = out_fn, separate = TRUE, 
                                of = "GTiff", creation_option = "BIGTIFF=YES",
                                init = -3000, a_nodata = -3000) %>%
                   return()
           }, out_dir = brick_path, 
           prefix = brick_prefix) %>%
    print()

# remove 
brick_imgs %>% dplyr::pull(mixture) %>% unlist() %>% file.remove()

print("Finished!")

