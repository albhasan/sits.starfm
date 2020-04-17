#-------------------------------------------------------------------------------
# BUILD BRICKS SENTINEL OF VEGETATION INDEXES
#-------------------------------------------------------------------------------

# TODO: Use the name of the inteprolation in the name of the brick


library(dplyr)
library(parallel)
library(raster)

tmp_directory <- "/disks/d3/tmp"
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
                  files_10m = purrr::map(files, dplyr::filter,
                                         resolution == "10m")) %>%
    dplyr::select(-files, -tile) %>%
    tidyr::unnest(files_10m) %>%
    dplyr::filter(band %in% c("B02", "B03", "B04", "B08")) %>%
    ensurer::ensure_that(nrow(.) > 0, err_des = "Images not found!") %>%
    dplyr::left_join(fmask_tb, by = c("tile", "img_date"))

# Pile the images into RAW BRICKS.
raw_brick_tb <- sentinel_tb %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile_raw(.x, out_dir = out_raw_dir),
                     keep = TRUE) %>%
    dplyr::bind_rows()

# Mask the Sentinel-2 images.
sentinel_tb[["img_masked"]] <- parallel::mclapply(1:nrow(sentinel_tb),
                                                  helper_mask,
                                                  sentinel_tb = sentinel_tb,
                                                  out_dir = tmp_directory,
                                                  mc.cores = 8)

# Pile the images into POROUS BRICKS.
interpolated_brick_tb <- sentinel_tb %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile_masked(.x, out_dir = out_porous_dir),
                                          keep = TRUE) %>%
    dplyr::bind_rows()

# Build INTERPOLATED BRICKS using Rolf's code.
# TODO: Convert this in part of the script instead of copy paste to the terminal.
script=/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interp_sentinel-2.R
in_dir=/disks/d3/brick_sentinel2_interpolated/porous
out_dir=/disks/d3/brick_sentinel2_interpolated/approx

Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif;

# Compute raw vegetation indexes.
raw_vegind_tb <- sentinel_tb %>%
    dplyr::select(-processing, -img_masked) %>%
    tidyr::pivot_wider(names_from = band, values_from = file_path) %>%
    dplyr::mutate(raw_vrt_file = purrr::pmap_chr(dplyr::select(., B02, B03, B04, B08),
                                                 helper_vrt_vi))
raw_vegind_tb[["vi_tb"]] <- parallel::mclapply(raw_vegind_tb$raw_vrt_file,
                                               helper_vi, mc.cores = 8)

# Pile raw vegetation indexes into RAW BRICK.
# source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")
# raw_vegind_tb <- readRDS("/home/alber/Documents/ghProjects/sits.starfm/TO_DELETE_raw_vegind_tb.rds")
raw_brick_vi_tb <- raw_vegind_tb %>%
    tidyr::unnest(vi_tb) %>%
    dplyr::select(-B02, -B03, -B04, -B08) %>%
    tidyr::pivot_longer(cols = c("gemi", "mtvi", "ndvi", "ndwi", "osavi",
                                 "rdvi", "savi"),
                        names_to = "band",
                        values_to = "file_path") %>%
    dplyr::arrange(img_date) %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile2(.x, out_dir = out_raw_dir), keep = TRUE) %>%
    dplyr::bind_rows()
# raw_brick_vi_tb <- readRDS("/home/alber/Documents/ghProjects/sits.starfm/inst/TO_DELETE_raw_brick_vi_tb.rds")

# Mask the vegetation indexes.
raw_vegind_tb <- raw_vegind_tb %>%
    tidyr::unnest(vi_tb)
raw_vegind_tb[["gemi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = gemi,  out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["mtvi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = mtvi,  out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["ndvi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = ndvi,  out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["ndwi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = ndwi,  out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["osavi_masked"]] <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = osavi, out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["rdvi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = rdvi,  out_dir = tmp_directory, mc.cores = 8)
raw_vegind_tb[["savi_masked"]]  <- parallel::mclapply(1:nrow(raw_vegind_tb), helper_mask2, img_tb = raw_vegind_tb, var = savi,  out_dir = tmp_directory, mc.cores = 8)

# Pile masked vegetation indexes into POROUS BRICKS.
porous_brick_vi_tb <- raw_vegind_tb %>%
    dplyr::select(-B02, -B03, -B04, -B08) %>%
    tidyr::pivot_longer(cols = c("gemi_masked", "mtvi_masked", "ndvi_masked",
                                 "ndwi_masked", "osavi_masked", "rdvi_masked",
                                 "savi_masked"),
                        names_to = "band",
                        values_to = "file_path") %>%
    dplyr::arrange(img_date) %>%
    dplyr::group_by(mission, level, baseline,
                    orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile2(.x, out_dir = out_porous_dir), keep = TRUE) %>%
    dplyr::bind_rows()

# Build INTERPOLATED VI BRICKS using Rolf's code.
# TODO: Convert this in part of the script instead of copy paste to the terminal.
script=/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interp_sentinel-2.R
in_dir=/disks/d3/brick_sentinel2_interpolated/porous
out_dir=/disks/d3/brick_sentinel2_interpolated/approx

Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_gemi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_gemi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndwi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndwi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_masked_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_masked_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif

