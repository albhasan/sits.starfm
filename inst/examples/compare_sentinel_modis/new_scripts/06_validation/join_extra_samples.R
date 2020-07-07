################################################################################
# JOIN THE SAMPLES  AND THE EXTRA-SAMPLES INTO A SINGLE FILE.
################################################################################
library(dplyr)
library(sf)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

extra_bands_file   <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/extra_samples_deforestation_bands.shp"
extra_indices_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/extra_samples_deforestation_indices.shp"
samples_file       <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_approx_3l.rds"
stopifnot(all(sapply(c(extra_bands_file, extra_indices_file, samples_file), file.exists)))

read_extra <- function(shp_file){
    shp_file %>%
        sf::read_sf() %>%
        sf::st_transform(crs = 4326) %>%
        add_coords() %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::rename(label = "obs_label") %>%
        dplyr::mutate(start_date = lubridate::as_date("2018-08-12"),
                      end_date   = lubridate::as_date("2019-08-28"),
                      cube        = NA,
                      time_series = NA) %>%
        dplyr::select(-x, -y, -cell, -id_label) %>%
        dplyr::select(longitude, latitude, start_date, end_date, label, cube, time_series) %>%
        return()
}

extra_bands    <- read_extra(extra_bands_file)
extra_indices  <- read_extra(extra_indices_file)

samples_tb <- samples_file %>%
    readRDS() %>%
    dplyr::bind_rows(extra_bands) %>%
    saveRDS(file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/validation_bands.rds")
samples_tb <- samples_file %>%
    readRDS() %>%
    dplyr::bind_rows(extra_indices) %>%
    saveRDS(file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/validation_indices.rds")

