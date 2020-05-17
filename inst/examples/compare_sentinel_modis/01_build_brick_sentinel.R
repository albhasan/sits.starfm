#-------------------------------------------------------------------------------
# BUILD BRICKS SENTINEL OF VEGETATION INDEXES
#-------------------------------------------------------------------------------

library(dplyr)
library(parallel)
library(raster)

tmp_directory <- "/disks/d3/tmp"
dir.create(file.path(tmp_directory, "masked"))

raster::rasterOptions(tmpdir = tmp_directory)
raster::tmpDir()

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

sentinel_l2a_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A";
sentinel_l1c_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C";
out_raw_dir          <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw";
out_porous_dir       <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/porous";
out_interpolated_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx";

# Get Fmask 4 masks.
fmask_tb <- sentinel_l1c_dir %>%
    list.files(pattern = "[A-Z0-9]{3}_[A-Z0-9]{6}_A[0-9]{6}_[0-9]{8}T[0-9]{6}_Fmask4[.]tif",
               full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(fmask_path = value) %>%
    dplyr::mutate(file_name = basename(fmask_path),
                  fmask_safe = stringr::str_extract(fmask_path,
                                                   pattern = ".+?(?=SAFE)"),
                  fmask_safe = stringr::str_c(fmask_safe, "SAFE")) %>%
    tidyr::separate(col = file_name, into = c("level", "tile", NA, "fmask_date",
                                              NA), sep = '_') %>%
    dplyr::mutate(img_date = stringr::str_sub(fmask_date, 1, 8),
                  img_date = lubridate::as_date(img_date)) %>%
    dplyr::select(-level)

# Get sentinel images.
sentinel_tb <- sentinel_l2a_dir %>%
    sits.starfm::build_sentinel_tibble() %>%
    dplyr::rename(safe_path = file_path) %>%
    dplyr::mutate(pyear = prodes_year(img_date),
                  files = purrr::map(files, dplyr::filter,
                                     resolution %in% c("10m", "20m"))) %>%
    dplyr::select(-tile) %>%
    tidyr::unnest(files) %>%
    dplyr::filter(band %in% c("B02", "B03", "B04", "B05", "B06",
                              "B07", "B08", "B8A", "B11", "B12")) %>%
    dplyr::filter(!band %in% c("B02", "B03", "B04", "B08") | !resolution =="20m") %>%
    ensurer::ensure_that(nrow(.) > 0, err_des = "Images not found!") %>%
    dplyr::left_join(fmask_tb, by = c("tile", "img_date"))

# Mask the Sentinel-2 images.
sentinel_tb[["img_masked"]] <- parallel::mclapply(1:nrow(sentinel_tb),
                                                  helper_mask,
                                                  sentinel_tb = sentinel_tb,
                                                  out_dir = file.path(tmp_directory,
                                                                      "masked"),
                                                  mc.cores = 12)

# Pile the images into POROUS BRICKS.
porous_brick_tb <- sentinel_tb %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile_vrt(.x, out_dir = out_porous_dir,
                                       cmd = "/usr/bin/gdalbuildvrt -separate %s %s",
                                       var_file = img_masked),
                     keep = TRUE) %>%
    dplyr::bind_rows()

# Build INTERPOLATED BRICKS using Rolf's code.
# TODO: run!
system("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interpolate_bricks.sh", intern = TRUE)

# Compute raw vegetation indexes.
raw_vegind_tb <- sentinel_tb %>%
    dplyr::select(-processing, -img_masked, -resolution) %>%
    tidyr::pivot_wider(names_from = band, values_from = file_path) %>%
    dplyr::mutate(raw_vrt_file = purrr::pmap_chr(dplyr::select(., B02, B03, B04, B08, B11),
                                                 helper_vrt_vi))
raw_vegind_tb[["vi_tb"]] <- parallel::mclapply(raw_vegind_tb$raw_vrt_file,
                                               helper_vi, mc.cores = 8)

# Pile raw vegetation indexes into RAW BRICK.
# TODO: Change code to use VRTs.
raw_brick_vi_tb <- raw_vegind_tb %>%
    tidyr::unnest(vi_tb) %>%
    dplyr::select(-tidyselect::starts_with("B(0|1|8)")) %>%
    #tidyr::pivot_longer(cols = c("evi", "ndmi", "ndvi", "savi", "mtvi", "osavi", "rdvi"), names_to = "band", values_to = "file_path") %>%
    #tidyr::pivot_longer(cols = c("evi", "ndmi", "ndvi", "savi"), names_to = "band", values_to = "file_path") %>%
    tidyr::pivot_longer(cols = c("evi", "ndmi"), names_to = "band", values_to = "file_path") %>%
    # NOTE: Because of pivoting, we removed each file's resolution
    #stop("TODO: Check the resolutions!") %>%
    dplyr::mutate(resolution = "10m") %>%
    dplyr::arrange(img_date) %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile2(.x, out_dir = out_raw_dir), keep = TRUE) %>%
    #dplyr::group_map(~ helper_pile_vrt(.x, out_dir = out_raw_dir, cmd = "/usr/bin/gdalbuildvrt -separate %s %s", var_file = file_path), keep = TRUE) %>%
    dplyr::bind_rows()
# raw_brick_vi_tb <- readRDS("/home/alber/Documents/ghProjects/sits.starfm/inst/TO_DELETE_raw_brick_vi_tb.rds")

# Mask the vegetation indexes.
raw_vegind_tb <- raw_vegind_tb %>%
    tidyr::unnest(vi_tb)
raw_vegind_tb[["evi_masked"]]        <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = evi,        out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
raw_vegind_tb[["ndmi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = ndmi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
raw_vegind_tb[["ndvi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = ndvi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
raw_vegind_tb[["savi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = savi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
#raw_vegind_tb[["gemi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = gemi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
#raw_vegind_tb[["mtvi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = mtvi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
#raw_vegind_tb[["osavi_masked"]]      <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = osavi,      out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)
#raw_vegind_tb[["rdvi_masked"]]       <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = rdvi,       out_dir = file.path(tmp_directory, "masked"), mc.cores = 8)

# Pile masked vegetation indexes into POROUS BRICKS.
# TODO: Change code to use VRTs.
porous_brick_vi_tb <- raw_vegind_tb %>%
    dplyr::select(-tidyselect::starts_with("B(0|1|8)")) %>%
    #tidyr::pivot_longer(cols = c("evi_masked", "gemi_masked", "mtvi_masked", "ndvi_masked", "osavi_masked", "rdvi_masked", "savi_masked"), names_to = "band", values_to = "file_path") %>%
    #tidyr::pivot_longer(cols = c("evi_masked", "ndmi_masked", "ndvi_masked", "savi_masked"), names_to = "band", values_to = "file_path") %>%
    tidyr::pivot_longer(cols = c("evi_masked", "ndmi_masked"), names_to = "band", values_to = "file_path") %>%
    # NOTE: Because of pivoting, we removed each file's resolution
    #stop("TODO: Check the resolutions!") %>%
    dplyr::mutate(resolution = "10m") %>%
    dplyr::arrange(img_date) %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile2(.x, out_dir = out_porous_dir), keep = TRUE) %>%
    #dplyr::group_map(~ helper_pile_vrt (.x, out_dir = out_porous_dir, cmd = "/usr/bin/gdalbuildvrt -separate %s %s", var_file = file_path), keep = TRUE) %>%
    dplyr::bind_rows()


# Build INTERPOLATED VI BRICKS using Rolf's code.
system("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interpolate_vegetation_indexes.sh", intern = TRUE)

