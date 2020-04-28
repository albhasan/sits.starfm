# copy the images for the test brick.

stop("run only once")

library(dplyr)
library(tidyr)
library(lubridate)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#---- Configuration ----

landsat_dir <- "/disks/d7/LANDSAT"
sentinel_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/sentinel2/L1C"

experiment_tiles <- tribble(
    ~landsat8, ~sentinel2,
    "225063",  "T20LKP",
    "226064",  "T22MBU",
    "233067",  "T22MDA"
)

img_year_landsat  <- 23
img_year_sentinel <- 36
img_miss_rate <- 0.25 # A quarter of the images could be misssing.

#---- Script ----

# List the Sentinel-2 L1C images and run sen2cor on them.

# Landsat-8 images, surface reflectance, compressed.
landsat_tb <- landsat_dir %>%
    list.files(pattern = "^LC[0-9]{18}T[0-9]-[A-Z]{2}[0-9]{14}[.]tar[.]gz",
               recursive = TRUE, full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(landsat_tar = value) %>%
    dplyr::mutate(file_name = basename(landsat_tar)) %>%
    tidyr::separate(col = file_name, sep = c(4, 10, 18, 20, 22),
                    into = c("type", "scene", "landsat_date", "collection",
                             "tier")) %>%
    dplyr::mutate(landsat_date = lubridate::as_date(landsat_date),
                  pyear = prodes_year(landsat_date)) %>%
    dplyr::distinct(scene, landsat_date, tier, .keep_all = TRUE)

# Sentinel images L1C downloaded using Sen2Agri.
sentinel_tb <- sentinel_dir %>%
    list.dirs(recursive = TRUE, full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(sentinel_safe = value) %>%
    dplyr::filter(stringr::str_ends(sentinel_safe, pattern = "SAFE")) %>%
    dplyr::mutate(dir_name = basename(sentinel_safe)) %>%
    tidyr::separate(col = dir_name, sep = '_',
        into = c("mission", "level", "sentinel_date", "baseline", "orbit", "tile",
                 "processing")) %>%
    dplyr::mutate(sentinel_date = sentinel_date %>%
                      stringr::str_sub(1, 8) %>%
                      lubridate::as_date(),
                  pyear = prodes_year(sentinel_date),
                  n_jp2 = purrr::map_int(sentinel_safe, count_jp2)) %>%
    dplyr::filter(n_jp2 > 14) %>%
    dplyr::select(-n_jp2) %>%
    dplyr::distinct(tile, sentinel_date, mission, orbit, .keep_all = TRUE)

# Number of images available per sensor per year.
series_sentinel <- sentinel_tb %>%
    dplyr::group_by(mission, level, tile, orbit, pyear) %>%
    dplyr::summarize(pyear_img = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pyear_img > (1 - img_miss_rate) * img_year_sentinel) %>%
    dplyr::rename(n_sentinel = pyear_img) %>%
    dplyr::right_join(experiment_tiles, by = c("tile" = "sentinel2")) %>%
    print(n = Inf)
# Number of images available per sensor per year.
series_landsat <- landsat_tb %>%
    dplyr::group_by(scene, pyear) %>%
    dplyr::summarize(pyear_img = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pyear_img > (1 - img_miss_rate) * img_year_landsat) %>%
    dplyr::arrange(scene, pyear) %>%
    dplyr::rename(n_landsat = pyear_img) %>%
    dplyr::right_join(experiment_tiles, by = c("scene" = "landsat8")) %>%
    print(n = Inf)

# Valid images for a brick.
series_bricks <- series_landsat %>%
    dplyr::inner_join(series_sentinel, by = c("scene" = "landsat8",
                                              "pyear" = "pyear")) %>%
    dplyr::select(scene, tile, pyear, n_landsat, n_sentinel, mission, orbit) %>%
    print(n = Inf)
rm(series_landsat, series_sentinel)

# Filter images
landsat_tb <- landsat_tb %>%
    dplyr::right_join(dplyr::select(series_bricks, scene, pyear),
                      by = c("scene", "pyear")) %>%
    tidyr::nest(files_landsat = c(landsat_tar, landsat_date, tier)) %>%
    dplyr::mutate(files_landsat = purrr::map(files_landsat, distinct_landsat)) %>%
    dplyr::select(type, scene, pyear, collection, files_landsat) %>%
    dplyr::arrange(type, scene, pyear, collection)
sentinel_tb <- sentinel_tb %>%
    dplyr::right_join(dplyr::select(series_bricks, tile, pyear, mission, orbit,
                                    scene),
                      by = c("tile", "pyear", "mission", "orbit")) %>%
    tidyr::nest(files_sentinel = c(sentinel_safe, sentinel_date, baseline,
                                   processing)) %>%
    dplyr::select(level, tile, pyear, mission, orbit, files_sentinel, scene) %>%
    dplyr::arrange(level, tile, pyear, mission, orbit)
# Join landsat_tb and sentinel_tb
images_tb <- sentinel_tb %>%
    dplyr::left_join(landsat_tb, by = c("scene", "pyear")) %>%
    dplyr::mutate(n_scene = purrr::map_int(files_landsat, nrow),
                  n_tile  = purrr::map_int(files_sentinel, nrow)) %>%
    # NOTE: Initial test includes only one.
    dplyr::arrange(n_tile, n_scene) %>%
    dplyr::select(-n_tile, -n_scene) %>%
    dplyr::slice(dplyr::n())

# Make a copy of the images to a directory for further processing:
# Fmask
# Sen2Cor
sentinel_l1c_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C"
landsat_sr_dir   <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/landsat_sr"
images_tb %>%
    dplyr::mutate(sentinel_l1c_dir = purrr::map(files_sentinel, cp_tb_files,
                                                col = sentinel_safe,
                                                out_dir = sentinel_l1c_dir),
                  landsat_sr_dir = purrr::map(files_landsat, cp_tb_files,
                                              col = landsat_tar,
                                              out_dir = landsat_sr_dir))


#==============================================================================
# TODO: Run by hand fmask and sen2cor on the copies of the sentinel images.
#==============================================================================

