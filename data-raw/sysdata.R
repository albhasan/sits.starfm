#!/usr/bin/env Rscript
# create the data for the package

# Specifications Landsat 8 surface reflectance - Table 7.1 pag 21 LaSRC product guide
# Short names adapted from "The Spectral Response of the Landsat-8 Operational Land Imager" Table 1, page 10233

library(dplyr)
library(purrr)
library(lubridate)
library(devtools)
library(ensurer)

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()

SPECS_L8_SR <- tibble::tibble(
    band_designation = c("sr_band1", "sr_band2", "sr_band3", "sr_band4",
                         "sr_band5", "sr_band6", "sr_band7", "pixel_qa",
                         "sr_aerosol", "radsat_qa"),
    band_name = c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5", "Band 6",
                  "Band 7", "Level-2 Pixel Quality Band", "Aerosol QA",
                  "Radiometric Saturation QA"),
    data_type = c("INT16", "INT16", "INT16", "INT16", "INT16", "INT16", "INT16",
                  "UINT16", "UINT8", "UINT16"),
    units = c("Reflectance", "Reflectance", "Reflectance", "Reflectance",
              "Reflectance", "Reflectance", "Reflectance", "Bit Index",
              "Bit Index", "Bit Index"),
    range = list(c(-2000, 16000), c(-2000, 16000), c(-2000, 16000),
                 c(-2000, 16000), c(-2000, 16000), c(-2000, 16000),
                 c(-2000, 16000), c(0, 32768), c(0, 255), c(0, 32768)),
    valid_range = list(c(0, 10000), c(0, 10000), c(0, 10000), c(0, 10000),
                       c(0, 10000), c(0, 10000), c(0, 10000), c(0, 32768),
                       c(0, 255), c(0, 3839)),
    fill_value = c(-9999, -9999, -9999, -9999, -9999, -9999, -9999, 1, NA, 1),
    saturate_value = c(20000, 20000, 20000, 20000, 20000, 20000, 20000, NA, NA,
                       NA),
    scale_factor = c(1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, NA, NA,
                     NA),
    mod13q1_name = c(NA, "blue", NA, "red", "nir", NA, "mir", NA, NA, NA),
    short_name = c("Ultra Blue", "Blue", "Green", "Red", "NIR", "SWIR1",
                    "SWIR2", "Quality", "Aerosol",
                    "Saturation")
)

# Spefications MOD13Q1 V6 -
# https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1_v006
SPECS_MOD13Q1 <- tibble::tibble(
    SDS_name = c("250m 16 days NDVI", "250m 16 days EVI",
                 "250m 16 days VI Quality", "250m 16 days red reflectance",
                 "250m 16 days NIR reflectance",
                 "250m 16 days blue reflectance",
                 "250m 16 days MIR reflectance",
                 "250m 16 days view zenith angle",
                 "250m 16 days sun zenith angle",
                 "250m 16 days relative azimuth angle",
                 "250m 16 days composite day of the year",
                 "250m 16 days pixel reliability"),
    description = c("16 day NDVI average", "16 day EVI average",
                    "VI quality indicators", "Surface Reflectance Band 1",
                    "Surface Reflectance Band 2", "Surface Reflectance Band 3",
                    "Surface Reflectance Band 7",
                    "View zenith angle of VI Pixel",
                    "Sun zenith angle of VI pixel",
                    "Relative azimuth angle of VI pixel",
                    "Day of year VI pixel", "Quality reliability of VI pixel"),
    Units = c("NDVI", "EVI", "Bit field", "Reflectance", "Reflectance",
              "Reflectance", "Reflectance", "Degree", "Degree", "Degree",
              "Julian day", "Rank"),
    data_type = c("16-bit signed integer", "16-bit signed integer",
                  "16-bit unsigned integer", "16-bit signed integer",
                  "16-bit signed integer", "16-bit signed integer",
                  "16-bit signed integer", "16-bit signed integer",
                  "16-bit signed integer", "16-bit signed integer",
                  "16-bit signed integer", "8-bit signed integer"),
    fill_value = c(-3000, -3000, 65535, -1000, -1000, -1000, -1000, -10000,
                   -10000, -4000, -1, -1),
    valid_range = list(c(-2000, 10000), c(-2000, 10000), c(0, 65534),
                       c(0, 10000), c(0, 10000), c(0, 10000), c(0, 10000),
                       c(0, 18000), c(0, 18000), c(-18000, 18000), c(1, 366),
                       c(0, 3)),
    scale_factor = c(1e-04, 1e-04, NA, 1e-04, 1e-04, 1e-04, 1e-04, 0.01, 0.01,
                     0.01, NA, NA),
    common_name = c("ndvi", "evi", "quality", "red", "nir", "blue", "mir",
                    "view zenith angle", "sun zenith angle",
                    "relative azimuth angle", "day of the year", "reliability"),
    gdal_prefix = rep("\"HDF4_EOS:EOS_GRID:\"", times = 12),
    gdal_suffix = paste0("\":MODIS_Grid_16DAY_250m_500m_VI:",
                         SDS_name, "\""),
    l8_sr_designation = c(NA, NA, NA, "sr_band4", "sr_band5", "sr_band2",
                          "sr_band7", NA, NA, NA, NA, NA))

c_names <- c("wavelength", "substrate", "vegetation", "dark")

END_MEMBERS_LANDSAT_7 <- c(0.483000, 0.218413, 0.100880, 0.083704,
                           0.560000, 0.344440, 0.098638, 0.047546,
                           0.662000, 0.535987, 0.067241, 0.023937,
                           0.835000, 0.669174, 0.585458, 0.010864,
                           1.648000, 0.754645, 0.208614, 0.003250,
                           2.206000, 0.671638, 0.088058, 0.002208) %>%
    matrix(ncol = 4, byrow = TRUE, dimnames = list(NULL, c_names)) %>%
    dplyr::as_tibble() %>% dplyr::mutate(band = c("sr_band1", "sr_band2",
        "sr_band3", "sr_band4", "sr_band5", "sr_band7")) %>%
    dplyr::select(band, wavelength, substrate, vegetation, dark)

END_MEMBERS_LANDSAT_8 <- c(0.482600, 0.217556, 0.107935, 0.085274,
                           0.561300, 0.336629, 0.101411, 0.048318,
                           0.654600, 0.542132, 0.066796, 0.026065,
                           0.864600, 0.698451, 0.639724, 0.010515,
                           1.609000, 0.836586, 0.219278, 0.002342,
                           2.201000, 0.741504, 0.102166, 0.001322) %>%
    matrix(ncol = 4, byrow = TRUE, dimnames = list(NULL, c_names)) %>%
    dplyr::as_tibble() %>% dplyr::mutate(band = c("sr_band2", "sr_band3",
        "sr_band4", "sr_band5", "sr_band6", "sr_band7")) %>%
    dplyr::select(band, wavelength, substrate, vegetation, dark)


usethis::use_data(END_MEMBERS_LANDSAT_7, END_MEMBERS_LANDSAT_8, SPECS_L8_SR,
                  SPECS_MOD13Q1, internal = TRUE, overwrite = TRUE)


# metadata of the images used to build the bricks
landsat_path <- "/home/alber/landsat8"
modis_path   <- "/home/alber/MOD13Q1"
scene_shp    <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp     <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
stopifnot(all(vapply(c(landsat_path, modis_path), dir.exists, logical(1))))
stopifnot(all(vapply(c(scene_shp, tile_shp), file.exists, logical(1))))
brick_scene  <- c("225063", "226064", "233067")
brick_from   <- paste0(2013:2016, "-08-01") %>% rep(times = length(brick_scene)) %>% lubridate::date() %>% as.list()
brick_to     <- paste0(2014:2017, "-07-30") %>% rep(times = length(brick_scene)) %>% lubridate::date() %>% as.list()
brick_scene  <- brick_scene %>% rep(each = length(2013:2016)) %>% as.list()
BRICK_IMAGES <- purrr::pmap(list(brick_scene, brick_from, brick_to), function(scene, from, to){
    build_brick_tibble2(landsat_path, modis_path, scene_shp, tile_shp,
                                     scenes = scene, from = from,to = to,
                                     add_neighbors = FALSE) %>%
        dplyr::mutate(year = lubridate::year(img_date)) %>%
        return()
}) %>% dplyr::bind_rows() %>% ensurer::ensure_that(nrow(.) > 0, err_desc = "Images not found!")


usethis::use_data(BRICK_IMAGES, internal = FALSE, overwrite = TRUE)