#-------------------------------------------------------------------------------
# BUILD A BRICK OF SENTINEL IMAGES FILLING IN THE CLOUDS WITH INTERPOLATION
# USING ROLF's INTERPOLATION CODE.
#-------------------------------------------------------------------------------

.Deprecated("Use build_brick_sentinel.R")
library(dplyr)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

sentinel_l2a_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A"
sentinel_l1c_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated"

# Tibble of Fmask 4 cloud masks.
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

# Sentinel images L2A processed using Sen2Cor.
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
    dplyr::left_join(fmask_tb, by = c("tile", "img_date")) %>%
    # Mask the Sentinel-2 images.
    dplyr::mutate(img_masked = purrr::map2_chr(file_path, fmask_path,
                                               mask_sentinel,
                                               out_dir = "/disks/d3/sentinel2_masked"))

# Build brick of masked images.
brick_tb <- sentinel_tb %>%
    dplyr::group_by(mission, level, baseline, orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile_interpolated(.x, out_dir = "/disks/d3/brick_sentinel2_interpolated/porous"),
                     keep = TRUE) %>%
    dplyr::bind_rows()

# Run Rolf code.
system("Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif;")
system("Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif;")
system("Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif;")
system("Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif;")

#-------------------------------------------------------------------------------
# Fixed error. Caused by full /tmp directory.
# Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif;
# Loading required package: sp
#   |======================================================================| 100%
#
#       /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif[1] "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif"
#   alber@esensing-006:/disks/d3/brick_sentinel2_interpolated/porous$   Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif;                                                                                           Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif;
#   Loading required package: sp
#     |======================================================================| 100%
#
#         /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif[1] "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif"
#     Loading required package: sp
#       |======================================================================| 100%
#
#           /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif[1] "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif"
#       alber@esensing-006:/disks/d3/brick_sentinel2_interpolated/porous$   Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif;
#       Loading required package: sp
#         |======================================================================| 100%
#
#             Error in .local(.Object, ...) : Dataset copy failed
#         Calls: jobs ... <Anonymous> -> new -> initialize -> initialize -> .local
#         Execution halted

#-------------------------------------------------------------------------------
# Old error. Caused by bug in script.
# Rscript  /home/alber/Documents/ghProjects/sits.starfm/inst/examples/interpolation_rolf/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_interpolated/porous/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif
# Loading required package: sp
# Processing...Merging result...Error in .rasterObjectFromFile(x, objecttype = "RasterBrick", ...) :
#   provide a valid filename
# Calls: jobs ... do.call -> lapply -> FUN -> FUN -> .rasterObjectFromFile
# Execution halted
