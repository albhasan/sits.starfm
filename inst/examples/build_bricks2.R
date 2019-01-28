library(tidyverse)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")


# TODO: get the -3000 to -9999!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# configuration 
landsat_path <- "/home/alber/landsat8"
modis_path   <- "/home/alber/MOD13Q1"
scene_shp    <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp     <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
brick_scene  <- "225063"
#brick_from   <- "2016-08-01"
#brick_to     <- "2017-07-30"
brick_from   <- "2014-08-01"
brick_to     <- "2015-07-30"
brick_path   <- "/home/alber/shared/brick_interp_few_clouds"

max_ts_hole  <- 23
min_miss_ratio <- 0
temp_dir     <- "/home/alber/shared/tmp" 
brick_bands  <- c("sr_band2", "sr_band4", "sr_band5", "sr_band7")

library(tidyverse)
library(devtools)
devtools::load_all()
browseOnce <- function() {
  old <- getOption("error")
  function() {
    options(error = old)
    browser()
  }
}
options(error = browseOnce())


# - - - - 

brick_year   <- lubridate::year(brick_to)
# Helper function: Run StarFM for a single image
helper_run_starfm <- function(row_n, brick_imgs){
    img0 <- brick_imgs %>% dplyr::slice(row_n)
    img1 <- brick_imgs %>% dplyr::slice(row_n) %>% dplyr::pull(next_best) 
    if(length(unlist(img1)) == 1 && is.na(unlist(img1)))
        return(NA)
    img1 <- img1 %>% dplyr::bind_rows()
    if (is.na(img1) || is.na(img1$sat_image) || is.na(img0$sat_image))
        return(NA)
    return(lapply(brick_bands, helper_run_starfm_band, img0, img1))
}

# Helper function: Run StarFM for a single band 
helper_run_starfm_band <- function(band, img0, img1){
    out_filename = file.path(starfm_dir, 
                                 stringr::str_c(stringr::str_c("starfm", 
                                     brick_scene, img0$img_date, band, 
                                     sep = "_"), ".bin"))

    starfm_res <- run_starFM(img0_f = img0, img1_f = img1, band = band,
                                 out_filename = out_filename,
                                 tmp_dir = tmp_dir)
    return(starfm_res)
} 

# handle temporal directories
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

# select best images per year, fill in clouds using StarFM results
brick_imgs <- build_brick_tibble2(landsat_path, modis_path, scene_shp, tile_shp,
                                scenes = brick_scene, from = brick_from, 
                                to = brick_to, add_neighbors = FALSE) %>% 
                  dplyr::mutate(year = lubridate::year(img_date)) %>%
                  dplyr::group_by(year) %>%
                  dplyr::top_n(-2, cloud_cov) %>%
                  dplyr::ungroup() %>%
                  dplyr::mutate(files = purrr::map(1:nrow(.), 
                          function(x, brick_imgs, bands, replacement, out_dir, tmp_dir){
                              # mask clouds using -9999
                              brick_imgs %>% dplyr::slice(x) %>% 
                                  mask_clouds(bands, replacement, out_dir, tmp_dir) %>%
                                  dplyr::mutate(file_path = ifelse(is.na(masked), 
                                          file_path, stringr::str_replace(masked, "masked_", ""))) %>%
                                      dplyr::select(file_path) %>%
                                  return()
                          }, brick_imgs = ., bands = brick_bands, 
                          replacement = -9999, out_dir = starfm_dir, tmp_dir = tmp_dir)) %>%
                  dplyr::mutate(next_best = purrr::map(1:nrow(.), 
                      function(x, brick_imgs){
                          # add an image to complete the pair required by StarFM
                          brick_imgs %>% get_next_image(ref_row_number = x,
                              cloud_threshold = 1.0)
                      }, brick_imgs = .)) %>%
                  dplyr::mutate(starfm_res = purrr::map(1:nrow(.), 
                                                 helper_run_starfm, 
                                                 brick_imgs = .)) %>%
                  dplyr::mutate(starfm = purrr::map(.$starfm_res, 
                      function(x){
                          # format filled_imgs into tibbles
                          if (length(x) == 1) return(x)
                          x %>% purrr::map(
                              function(y) {
                                  # cast a named list into a tibble
                                  dplyr::as_tibble(as.list(y))
                              }) %>%
                              dplyr::bind_rows()
                      })) %>% 
                  dplyr::select(-starfm_res) %>%
                  dplyr::mutate(filled = purrr::map(1:nrow(.), 
                      function(x, brick_imgs) {
                          # fill in the image clouds using StarFM images
                          fill_clouds(img = dplyr::slice(brick_imgs, x), 
                                                tmp_dir = tmp_dir)
                      }, brick_imgs = .))

first_img_date <- brick_imgs %>% dplyr::slice(1) %>% dplyr::pull(img_date) %>% 
                      dplyr::first()
# there is no StarFM for the last image. Use the original
last_img <- brick_imgs %>% dplyr::slice(nrow(.)) %>% tidyr::unnest(files) %>% 
                dplyr::mutate(band = get_landsat_band(file_path)) %>%
                dplyr::filter(band %in% brick_bands) %>% rename(filled = file_path)
filled_imgs <- brick_imgs %>% tidyr::unnest(filled) %>% 
                   dplyr::filter(!is.na(filled)) %>%
                   dplyr::mutate(band = get_landsat_band(filled)) %>% 
                   dplyr::bind_rows(last_img)

brick_filenames <- purrr::map(brick_bands, function(x){
    band_short_name <- SPECS_L8_SR %>% dplyr::filter(band_designation == x) %>%
        dplyr::pull(short_name) %>% dplyr::first() %>%
        stringr::str_replace(" ", "_") %>% tolower()
    out_fn <- file.path(brick_path,
                        paste0(paste("LC8SR-MOD13Q1-STARFM", brick_scene,
                                     first_img_date, band_short_name,
                                     "STACK_BRICK", sep = "_"), ".tif" ))
    filled_imgs %>% dplyr::filter(band %in% x) %>% 
        dplyr::pull(filled) %>%
        gdal_merge(out_filename = out_fn, separate = TRUE, of = "GTiff",
        creation_option = "BIGTIFF=YES", init = -3000, a_nodata = -3000) %>%
        return()
})

