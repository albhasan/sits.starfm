#!/usr/bin/env Rscript
################################################################################
# FUSE LANDSAT 8 AND MODIS IMAGES
#------------------------------------------------------------------------------
# Last update 2018-10-08
#------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(log4r)
# devtools::load_all()
source("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm/R/util.R")
source("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm/R/gdal_util.R")


logger                 <- log4r::create.logger()
log4r::logfile(logger) <- file.path("fuse_images_207.log")
log4r::level(logger)   <- "DEBUG"
log4r::info(logger, "...............................................................................")

#options( warn = 2 ) # TODO: remove for production

build_brick <- function(landsat_path, modis_path, scene_shp, tile_shp,
                        brick_scene, brick_year, brick_bands,
                        brick_path, cloud_threshold, img_per_year = 23,
                        image_step = 16, temp_dir = NULL) {

    log4r::info(logger, "Validating inputs...")
    if (is.null(temp_dir))
        temp_dir <- tempdir()
    tmp_dir <- temp_dir %>%
        file.path(paste("L8MOD", brick_scene, brick_year, sep = "_"))
    starfm_dir <- temp_dir %>%
        file.path(paste("starfm", brick_scene, brick_year, sep = "_"))
    if (!dir.exists(tmp_dir))
        tmp_dir %>% dir.create()
    if (!dir.exists(starfm_dir))
        starfm_dir %>% dir.create()

    log4r::info(logger, "Assembling bricks' data...")
    brick_tb <- suppressWarnings(build_brick_tibble(landsat_path = landsat_path,
        modis_path = modis_path, scene_shp = scene_shp, tile_shp = tile_shp,
        scenes = brick_scene, from = paste(brick_year - 1, "08-01", sep = "-"),
        to = paste(brick_year, "09-30", sep = "-"), max_ts_hole = 1,
        min_miss_ratio = 0.95))

    log4r::info(logger, "Get brick's images...")
    brick_imgs <- brick_tb %>%
        dplyr::group_by(scene, prodes_year) %>%
        tidyr::nest(.key = "ts") %>%
        dplyr::filter(scene %in% brick_scene, prodes_year %in% brick_year) %>%
        dplyr::pull(ts) %>% dplyr::bind_rows() %>% dplyr::arrange(img_date) %>%
        dplyr::mutate(scene = substr(sat_image, 11, 16))

    log4r::info(logger, "Add extra images...")
    br_img_last_date <- brick_imgs %>% dplyr::slice(nrow(.)) %>%
        dplyr::pull(img_date)
    brick_imgs <- brick_tb %>%
        dplyr::filter(scene %in% brick_scene,
                      img_date %in% (br_img_last_date + image_step)) %>%
        dplyr::select(img_date, sat_image) %>% dplyr::bind_rows(brick_imgs) %>%
        dplyr::arrange(img_date)
    if (nrow(brick_imgs) < img_per_year) {
        stop(sprintf("Not enough images for building a brick: %s / %s", nrow(brick_imgs), img_per_year))
    }

    log4r::info(logger, "Find the next best image to compute StarFM...")
    brick_imgs$next_best <- suppressWarnings(
        lapply(1:nrow(brick_imgs), function(x, brick_imgs, cloud_threshold) {
            brick_imgs %>% get_next_image(ref_row_number = x,
                                          cloud_threshold = cloud_threshold) %>%
                return()
        }, brick_imgs = brick_imgs, cloud_threshold = cloud_threshold))

    log4r::info(logger, "Remove extra images...")
    brick_imgs <- brick_imgs %>% dplyr::slice(1:img_per_year)

    log4r::info(logger, "Iterating over images...")
    sfm_list <- list()
    for (row_n in 1:nrow(brick_imgs)) {
        log4r::info(logger, sprintf("    Row %s - Getting images t0 and t1", row_n))
        img1 <- brick_imgs %>% dplyr::slice(row_n) %>%
            dplyr::pull(next_best) %>% dplyr::bind_rows()
        img0 <- brick_imgs %>% dplyr::slice(row_n)

        # store the results
        sfm_files <- tibble::tibble(starfm = character(), t1_fine = character(),
                                    t1_coarse = character(),
                                    t0_fine = character(),
                                    t0_coarse = character(),
                                    sfm_conf = character())
        # validation
        if (is.na(img1$sat_image)) {
            log4r::warn(logger, sprintf("    Row %s - No suitable t1 image for scene %s on %s", row_n, brick_scene, img0$img_date))
            warning(sprintf("No suitable Landsat image was found for computing
                            the fusion model for scene %s on %s", brick_scene,
                            img0$img_date))
            sfm_list[[length(sfm_list) + 1]] <- sfm_files
            next()
        }
        mod_cloud_cover <- img0 %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>%
            dplyr::pull(cloud_cov) %>% mean()
        if (mod_cloud_cover > cloud_threshold) {
            log4r::warn(logger, sprintf("    Row %s - Cloudy MODIS images for scene %s on %s", row_n, brick_scene, img0$img_date))
            warning(sprintf("MODIS images are too cloudy for scene %s on %s",
                            brick_scene, img0$img_date))
            sfm_list[[length(sfm_list) + 1]] <- sfm_files
            next()
        }
        if (is.na(img0$sat_image))
            log4r::warn(logger, sprintf("    Row %s - Landsat image t0 is missing (scene %s, date %s)", row_n, brick_scene, img0$img_date))
            warning(sprintf("Landsat Image (scene %s and date %s) is missing!",
                            brick_scene, img0$img_date))

        # run the fusion ----
        for (band in brick_bands) {
            log4r::info(logger, sprintf("    Row %s - Processing band %s", row_n, band))
            out_filename =
                stringr::str_c(file.path(starfm_dir,
                                         stringr::str_c(
                                             stringr::str_c("starfm",
                                                            img0$sat_image,
                                                            band, sep = "_"),
                                             ".bin")))
            if (is.na(img0$sat_image))
                out_filename =
                    stringr::str_c(file.path(starfm_dir,
                                             stringr::str_c(stringr::str_c(
                                                 "starfm_NA", band,
                                                 sep = "_"), ".bin")))
            log4r::info(logger, sprintf("    Row %s - Running StarFM...", row_n))
            starfm_res <- run_starFM(img0_f = img0, img1_f = img1, band = band,
                                     out_filename = out_filename,
                                     tmp_dir = tmp_dir)
            log4r::debug(logger, sprintf("    Row %s - Done running StarFM...", row_n))
            sfm_files <- sfm_files %>%
                dplyr::add_row(starfm = starfm_res["starfm"],
                               t1_fine = starfm_res["t1_fine"],
                               t1_coarse = starfm_res["t1_coarse"],
                               t0_fine = starfm_res["t0_fine"],
                               t0_coarse = starfm_res["t0_coarse"],
                               sfm_conf = starfm_res["sfm_conf"])
        }
        log4r::info(logger, sfm_files) 
        sfm_list[[length(sfm_list) + 1]] <- sfm_files
    }

    if (length(sfm_list) != img_per_year)
        warning("Missing fusion images!")

    log4r::info(logger, "Adding StarFM files...")
    brick_imgs$starfm <- sfm_list

    # - - - 
    # TODO: remove  
    save(brick_imgs, file = file.path(starfm_dir, paste0(paste("brick_imgs", brick_scene, brick_year, sep = "_"), ".Rdata")))
    # - - - 

    log4r::info(logger, "Filling in the clouds...")
    brick_imgs$filled <- lapply(1:nrow(brick_imgs), function(x, brick_imgs) {
        fill_clouds(img = dplyr::slice(brick_imgs, x), tmp_dir = tmp_dir)
    }, brick_imgs = brick_imgs)

    log4r::info(logger, "Stacking up filled images...")
    filled_tb <- brick_imgs %>% tidyr::unnest(filled) %>%
        dplyr::mutate(band = get_landsat_band(filled))
    brick_path <- lapply(brick_bands, function(x, filled_tb, brick_path){
        out_fn <- file.path(brick_path,
                            paste0(paste("brick_LC08MOD", brick_scene, brick_year,
                                         x, sep = "_"), ".tif" ))
        paths <- filled_tb %>% dplyr::filter(band == x) %>%
            dplyr::pull(filled) %>%
            gdal_merge(out_filename = out_fn, separate = TRUE, of = "GTiff",
                       creation_option = "BIGTIFF=YES", init = -3000,
                       a_nodata = -3000)
        return(paths)
    }, filled_tb = filled_tb, brick_path = brick_path)
    save(brick_path, file = file.path(tmp_dir, "brick_path.Rdata"))

    log4r::info(logger, sprintf("Finished scene %s year %s", brick_scene, brick_year))
    return(brick_imgs)
}



#debugonce(run_starFM)
brick_233067_2015 <- build_brick(
    landsat_path = "/home/alber/landsat8"
    ,
    modis_path = "/home/alber/MOD13Q1"
    ,
    scene_shp = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
    ,
    tile_shp = "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
    ,
    brick_scene = "233067"
    ,
    brick_year = 2016
    ,
    brick_bands = "sr_band2"
    ,
    brick_path = "/home/alber/Documents/fusion/brick"
    ,
    cloud_threshold = 0.5
    ,
    img_per_year = 23
    ,
    image_step = 16
    ,
    temp_dir = "/home/alber/Documents/fusion/tmp"
)
warnings()

