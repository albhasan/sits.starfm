#!/usr/bin/Rscript
# Create a mask of the corners of the images of the same scene

# TODO:
stop("Unfinished - merge the corner masks by scene-year")
# gdal_calc.py throws errors due to differences on image size
# use gdalbuildvrt to avoid sizes differtences

suppressMessages(suppressPackageStartupMessages(library(tidyverse)))
suppressMessages(suppressPackageStartupMessages(library(optparse )))
suppressMessages(suppressPackageStartupMessages(library(devtools )))
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()

brick_scene <- c("225063", "226064", "233067") 
brick_from  <- "2014-08-01"
brick_to    <- "2017-07-30"
brick_pyear <- 2014:2017

brick_path   <- "/home/alber/shared/brick_maskcloud"
landsat_path <- "/home/alber/landsat8"
no_data      <- -9999
out_dir      <- "/home/alber/shared/mask_l8_corner"
tmp_dir      <- NULL

# handle temporal directories
if(is.null(tmp_dir))
    tmp_dir <- tempdir()
if (!dir.exists(tmp_dir))
    dir.create(tmp_dir)

# get a tibble of images, files, and corner masks
l8_img <- build_landsat_tibble(landsat_path,
        "^L[CTM]0[4-9]_L[0-3][A-Z]{2}_[0-9]{6}_[0-9]{8}_[0-9]{8}_[0-9]{2}_[A-Z]([0-9]|[A-Z])",
        from = brick_from, to = brick_to) %>%
        dplyr::filter(tile %in% brick_scene, prodes_year %in% brick_pyear) %>%
    dplyr::mutate(
        mask_area = purrr::map_chr(.$files, 
            function(fs){
                fs %>%
                    dplyr::filter(stringr::str_detect(file_path, pattern = "sr_band4")) %>%
                    dplyr::pull(file_path) %>%
                    get_landsat_corner_mask(out_file = tempfile(pattern = "mask_area_",
                                                                fileext = ".tif")) %>%
                    return()
                }))

# combine all the corner masks l8_im
merge_masks <- function(file_path, out_file){
    stopifnot(is.character(file_path))
    if (length(file_path) < 2) return(NA)
    for(i in seq_along(file_path)){
        fname <- tempfile(pattern = paste0("mask_area_aggreated_", i, '_'), fileext = ".tif")
        if (i == 1) {
            next()
        }else if (i == length(file_path)) {
            fname <- out_file 
        }
        gdal_calc(input_files = c(file_path[i-1], file_path[i]),
                  out_filename = fname,
                  expression = "(numpy.where(numpy.logical_or(A == 0, B == 0), 0, 1)).astype(int16)",
                  dstnodata = 0,
                  data_type = "Int16",
                  dry_run = FALSE)
    }
    return(fname)
}

# TODO: delete
# workaorund - just get the first corner mask
res <- l8_img %>%
    dplyr::arrange(tile, img_date) %>%
    dplyr::group_by(tile, prodes_year) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(sat_image, files, neigh, cloud_cov)) %>% 
    dplyr::mutate(mask_corner = file.path(out_dir, paste0("LC08_CORNERMASK_", tile, "_", prodes_year, ".tif")))
for (i in 1:nrow(res)){
    if(i > 0){
        print(sprintf("Pu~netero R copiando a %s", res$mask_corner[[i]]))
        file.copy(res$mask_area[[i]], res$mask_corner[[i]])
    }
}
#  - - 



for (s in brick_scene) {
    for (y in brick_pyear) {
        o_file <- file.path(out_dir, paste0("LC08_CORNERMASK_", s, '_', y, ".tif"))
        print(sprintf("Saving file %s", o_file))
        l8_img %>% dplyr::filter(tile == s, prodes_year == y) %>%
            dplyr::pull(mask_area) %>% merge_masks(out_file = o_file)
    }
}



