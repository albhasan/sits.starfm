#!/usr/bin/env Rscript

# create the data for the package

library(dplyr)
library(purrr)
library(lubridate)
library(ensurer)
library(sits.starfm)

#setwd("/home/alber/Documents/ghProjects/sits.starfm")
#devtools::load_all()

#---- BRICK_IMAGES ----
# metadata of the images used to build the bricks
landsat_path <- "/home/alber/landsat8"
modis_path   <- "/home/alber/MOD13Q1"
scene_shp    <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/wrs2_descending.shp"
tile_shp     <- "/home/alber/Documents/data/experiments/l8mod-fusion/data/shp/modis-tiles.shp"
stopifnot(all(vapply(c(landsat_path, modis_path), dir.exists, logical(1))))
stopifnot(all(vapply(c(scene_shp, tile_shp), file.exists, logical(1))))

brick_scene  <- c("225063", "226064", "233067")
brick_from   <- paste0(2013:2016, "-08-01") %>%
  rep(times = length(brick_scene)) %>%
  lubridate::date() %>%
  as.list()
brick_to     <- paste0(2014:2017, "-07-30") %>%
  rep(times = length(brick_scene)) %>%
  lubridate::date() %>%
  as.list()
brick_scene  <- brick_scene %>%
  rep(each = length(2013:2016)) %>%
  as.list()
BRICK_IMAGES <- purrr::pmap(list(brick_scene, brick_from, brick_to),
                            function(scene, from, to){
                              build_brick_landsat_modis(landsat_path,
                                                        modis_path, scene_shp,
                                                        tile_shp,
                                                        scenes = scene,
                                                        from = from, to = to,
                                                        add_neighbors = FALSE) %>%
                                dplyr::mutate(year = lubridate::year(img_date)) %>%
                                return()
                            }) %>%
  dplyr::bind_rows() %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Images not found!") %>%
  ensurer::ensure_that(nrow(.) == length(unique(.$sat_image)),
                       err_des = "Some images are duplicated.")
# Get one file (band 1) of each image.
tmp_img_path <- BRICK_IMAGES %>%
    dplyr::select(sat_image, files) %>%
    tidyr::unnest() %>%
    dplyr::filter(stringr::str_detect(basename(file_path), "_sr_band1.tif")) %>%
    dplyr::group_by(sat_image) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(img_extent = purrr::map(file_path, function(x){
        ext <- x %>%
            raster::raster() %>%
            raster::projectExtent(crs = "+proj=longlat +datum=WGS84") %>%
            raster::extent() %>%
            attributes()
        ext[["class"]] <- NULL
        return(unlist(ext))
    })) %>%
    dplyr::select(-file_path)
BRICK_IMAGES <- BRICK_IMAGES %>%
    dplyr::left_join(tmp_img_path, by = "sat_image")

BRICK_IMAGES %>%
    dplyr::select(sat_image, scene, files) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
        scene_sat_image = purrr::map_chr(.$sat_image, function(x){unlist(stringr::str_split(x, "_"))[3]}),
        scene_file =      purrr::map_chr(.$file_path, function(x){unlist(stringr::str_split(basename(x), "_"))[3]})
    ) %>%
    dplyr::mutate(test_res = all.equal(scene, scene_sat_image, scene_file)) %>%
    dplyr::pull(test_res) %>%
    ensurer::ensure_that(all(.), err_desc = "Scene missmatch")

#---- BRICK_HLS_IMAGES ----
fileext <- NULL
BRICK_HLS_IMAGES <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/harmonized_landsat_sentinel2/data/hls" %>%
    build_hls_tibble(pattern = "*hdf$") %>%
    dplyr::select(-fileext)

#---- END_MEMBERS_LANDSAT_7 ----
# values obtained from "Global cross-calibration of Landsat spectral mixture models" - https://www.sciencedirect.com/science/article/pii/S0034425717300500
# file name Landsat 7 ETM+ Global Endmembers
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

#---- END_MEMBERS_LANDSAT_8 ----
# values obtained from "Global cross-calibration of Landsat spectral mixture models" - https://www.sciencedirect.com/science/article/pii/S0034425717300500
# Landsat 8 OLI Global Endmembers
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

#---- SPECS_HLS_L30 ----
# SKAKUN, S. et al. Harmonized Landsat Sentinel-2 ( HLS ) Product User ’ s Guide.
# [s.l: s.n.]. <https://hls.gsfc.nasa.gov/>.
# Table 9: list of the SDS of the L30 product
# (SR = Surface Reflectance,
# NBAR = Nadir BRDF-normalized Reflectance,
# TOA Refl. = Top of Atmosphere Reflectance,
# TOA BT = Top of Atmosphere Brightness temperature)
SPECS_HLS_L30 <- tibble::tribble(
    ~SDS_name, ~OLI_band_number, ~Units,        ~Data_type, ~Scale, ~Fill_value, ~Spatial_Resolution, ~Description,
    "band01",  "1",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band02",  "2",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band03",  "3",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band04",  "4",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band05",  "5",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band06",  "6",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band07",  "7",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "band09",  "9",              "reflectance", "int16",    0.0001, -1000,       30,                  "TOA Refl.",
    "band10",  "10",             "degree C",    "int16",    0.01,   -1000,       30,                  "TOA BT",
    "band11",  "11",             "degree C",    "int16",    0.01,   -1000,       30,                  "TOA BT",
    "QA",      NA,               NA,            "uint8",    NA,     255,         30,                  "Quality bits"
)

#---- SPECS_HLS_NOMENCLATURE ----
# SKAKUN, S. et al. Harmonized Landsat Sentinel-2 ( HLS ) Product User ’ s Guide.
# [s.l: s.n.]. <https://hls.gsfc.nasa.gov/>.
# Table 3: HLS spectral bands nomenclature
#
# GRIFFITHS, P.; NENDEL, C.; HOSTERT, P. Intra-annual reflectance composites
# from Sentinel-2 and Landsat for national-scale crop and land cover mapping.
# Remote Sensing of Environment, v. 220, n. October 2017, p. 135–151, jan. 2019.
# Table 1: Specifications of the Sentinel-2 and Landsat-8 missions
SPECS_HLS_NOMENCLATURE <- tibble::tribble(
    ~Band_name,           ~OLI_band_number, ~MSI_band_number, ~HLS_band_code_name_L8, ~HLS_band_code_name_S2, ~short_name, ~center_wavelength_msi, ~center_wavelength_oli,
    "Coastal Aerosol",    '1',              '1',              'band01',               'B01',                  "aerosol",                     443,                     443,
    "Blue",               '2',              '2',              'band02',               'B02',                  "blue",                        490,                     482,
    "Green",              '3',              '3',              'band03',               'B03',                  "green",                       560,                     561,
    "Red",                '4',              '4',              'band04',               'B04',                  "red",                         665,                     655,
    "Red-Edge 1",         NA,               '5',              NA,                     'B05',                  "rededge1",                    705,                      NA,
    "Red-Edge 2",         NA,               '6',              NA,                     'B06',                  "rededge2",                    740,                      NA,
    "Red-Edge 3",         NA,               '7',              NA,                     'B07',                  "rededge3",                    783,                      NA,
    "NIR Broad",          NA,               '8',              NA,                     'B08',                  "nirbroad",                    842,                      NA,
    "NIR Narrow",         '5',              '8A',             'band05',               'B8A',                  "nirnarrow",                   865,                     865,
    "SWIR 1",             '6',              '11',             'band06',               'B11',                  "swir1",                      1610,                    1609,
    "SWIR 2",             '7',              '12',             'band07',               'B12',                  "swir2",                      2190,                    2201,
    "water vapor",        NA,               '9',              NA,                     'b09',                  "vapor",                       945,                      NA,
    "Cirrus",             '9',              '10',            'band09',                'B10',                  "cirrus",                     1375,                    1373,
    "Thermal Infrared 1", '10',             NA,              'band10',                NA,                     "infrared1",                    NA,                   10900,
    "Thermal Infrared 2", '11',             NA,              'band11',                NA,                     "infrared2",                    NA,                   12000
)


#---- SPECS_HLS_QA ----
# SKAKUN, S. et al. Harmonized Landsat Sentinel-2 ( HLS ) Product User ’ s Guide.
# [s.l: s.n.]. <https://hls.gsfc.nasa.gov/>.
# Table 10: Description of the bits in the one-byte Quality Assessment layer i
# for the 3 products. Bits are listed from the MSB (bit 7) to the LSB (bit 0)
SPECS_HLS_QA <- tibble::tribble(
    ~Bit_number, ~QA_description,           ~Bit_combination, ~mask_value,
    0,           "Cirrus",                  "1",              1,
    1,           "Cloud",                   "1",              2,
    2,           "Adjacent cloud",          "1",              4,
    3,           "Cloud shadow",            "1",              8,
    4,           "Snow/ice",                "1",              16,
    5,           "Water",                   "1",              32,
    67,          "Aerosol quality low",     "01",             64,
    67,          "Aerosol quality average", "10",             128,
    67,          "Aerosol quality high",    "11",             192
)

#---- SPECS_HLS_S30 ----
# SKAKUN, S. et al. Harmonized Landsat Sentinel-2 ( HLS ) Product User ’ s Guide.
# [s.l: s.n.]. <https://hls.gsfc.nasa.gov/>.
# Table 8: list of the SDS of the S30 product i
# (SR = Surface Reflectance,
# NBAR = Nadir BRDF-Adjusted Reflectance,
# TOA Refl. = Top of Atmosphere Reflectance)
SPECS_HLS_S30 <- tibble::tribble(
    ~SDS_name, ~MSI_band_number, ~Units,        ~Data_type, ~Scale, ~Fill_value, ~Spatial_Resolution, ~Description,
    "B01",     "1",              "reflectance", "int16",    0.0001, -1000,       30,                  "SR",
    "B02",     "2",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B03",     "3",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B04",     "4",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B05",     "5",              "reflectance", "int16",    0.0001, -1000,       30,                  "SR",
    "B06",     "6",              "reflectance", "int16",    0.0001, -1000,       30,                  "SR",
    "B07",     "7",              "reflectance", "int16",    0.0001, -1000,       30,                  "SR",
    "B08",     "8",              "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B8A",     "8A",             "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B09",     "9",              "reflectance", "int16",    0.0001, -1000,       30,                  "TOA Refl.",
    "B10",     "10",             "reflectance", "int16",    0.0001, -1000,       30,                  "TOA Refl.",
    "B11",     "11",             "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "B12",     "12",             "reflectance", "int16",    0.0001, -1000,       30,                  "NBAR",
    "QA",      NA,               NA,            "uint8",    NA,     255,         30,                  "Quality bits"
)

#---- SPECS_L8_SR ----
# Adapted from:
# U.S. GEOLOGICAL SURVERY. Landsat8 Surface Reflectance code (LaSRC) Version 4.3 PRODUCT: Product Guide. n. March, p. 40, 2018.
# Table 7-1 Surface Reflectance Specifications
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

#---- SPECS_MOD13Q1 ----
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

#--- Save ----
usethis::use_data(
    BRICK_HLS_IMAGES,
    BRICK_IMAGES,
    END_MEMBERS_LANDSAT_7,
    END_MEMBERS_LANDSAT_8,
    SPECS_HLS_L30,
    SPECS_HLS_NOMENCLATURE,
    SPECS_HLS_QA,
    SPECS_HLS_S30,
    SPECS_L8_SR,
    SPECS_MOD13Q1,
    internal = TRUE, overwrite = TRUE)

