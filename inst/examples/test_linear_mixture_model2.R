
# COMPARE THE MIXTURE MODEL VALUES IN A BRICK AGAINST THOSE COMPUTED DIRECTLY FROM THE IMAGES.

library(dplyr)
library(sits.prodes)
library(sits.starfm)

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_raw2"
brick_file_pattern <- "^LC8SR-(MASKCLOUD|MOD13Q1-MYD13Q1|MOD13Q1-STARFM|RAW|SIMPLE)_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_[a-z0-9]{+}_STACK_BRICK.tif$"

my_scene <- "226064"
my_year <- 2016

get_pix_value_first_band <- function(file_path, my_pixel){
    "gdallocationinfo -valonly -b 1 -geoloc" %>% paste(file_path, my_pixel$x, my_pixel$y) %>%
        system(intern = TRUE) %>%
        as.numeric() %>%
        return()
}

# Get bricks of linear mixture model.
brick_md <- in_dir %>%
    list.files(pattern = brick_file_pattern, full.names = TRUE) %>%
    ensurer::ensure_that(length(.) > 0,
                         err_desc = sprintf("No brick files found at %s",
                                            in_dir)) %>%
    sits.prodes::get_brick_md() %>%
    tibble::as_tibble() %>%
    dplyr::filter(pathrow == my_scene, year == my_year) %>% 
    dplyr::filter(band %in% c("dark", "substrate", "vegetation")) %>%
    dplyr::rename(file_path = "path")

# Get the the first image of the bricks and then get the band values at my_pixel.
files_md <- sits.starfm::BRICK_IMAGES %>%
    dplyr::filter(scene == brick_md$pathrow[[1]],
                  img_date == brick_md$start_date[[1]]) %>%
    dplyr::pull(files) %>%
    unlist() %>%
    stringr::str_subset(pattern = ".*[.]tif$") %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(sat_image = substr(basename(file_path), 1, 40),
                  tile = substr(sat_image, 11, 16),
                  img_date = lubridate::ymd(substr(sat_image, 18, 25)),
                  proc_date = lubridate::ymd(substr(sat_image, 27, 34)),
                  prodes_year = sits.starfm:::match_prodes_year(img_date,
                                                                prodes_start = "-08-01"),
                  band = sits.starfm::get_landsat_band(file_path)) %>%
    dplyr::filter(band %in% sits.starfm:::END_MEMBERS_LANDSAT_8$band) %>%
    dplyr::arrange(band)

# Get coordinates of the middle of the image.
ext <- files_md %>% dplyr::slice(1) %>%
    dplyr::pull(file_path) %>%
    sits.prodes:::get_img_md() %>%
    list(
        mid_x = mean(.$extent_output[c(1,3)]),
        mid_y = mean(.$extent_output[c(2,4)])
     )
my_pixel <- list(x = ext$mid_x, y = ext$mid_y)

# get pixel values.
brick_md <- brick_md %>% dplyr::mutate(pix_value = purrr::map_dbl(.$file_path,
                                             get_pix_value_first_band,
                                             my_pixel = my_pixel))
files_md <- files_md %>% dplyr::mutate(pix_value = purrr::map_dbl(.$file_path,
                                             get_pix_value_first_band,
                                             my_pixel = my_pixel))

end_member_mt <- as.matrix(sits.starfm:::END_MEMBERS_LANDSAT_8[3:5])
comp_mix_model <- files_md$pix_value %*% end_member_mt
computed_tb <- tibble::tibble(band = colnames(comp_mix_model), comp = as.vector(comp_mix_model))

# Get the values from the brick themselves
brick_md <- brick_md %>% dplyr::left_join(computed_tb, by = "band") %>%
    dplyr::mutate(diff = pix_value - comp)

brick_md

