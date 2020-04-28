#-------------------------------------------------------------------------------
# CLASSIFY SENTINEL-2 BRICKS
#-------------------------------------------------------------------------------

library(caret)
library(dplyr)
library(raster)
library(sits)

tmp_directory <- "/disks/d3/tmp"
dir.create(file.path(tmp_directory, "masked"))
raster::rasterOptions(tmpdir = tmp_directory)
raster::tmpDir()

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#-------------------------------------------------------------------------------
# K-FOLDS
#-------------------------------------------------------------------------------

raw_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing")

raw_samples_tb %>%
    sits::sits_kfold_validate() %>%
    (function(x){
        flevels <- x %>%
            dplyr::select(predicted, reference) %>%
            unlist() %>%
            unique()
        if (length(flevels) < 2) {
            warning("Found less than 2 labels!")
            return(NA)
        }
        con_mat <- caret::confusionMatrix(data = factor(x[["predicted"]],
                                                        flevels),
                                          reference = factor(x[["reference"]],
                                                             flevels))
    }) %>%
    print()
rm(list = ls(all.names = TRUE))

#-------------------------------------------------------------------------------
# ANALYSIS OF IMPORTANCE OF BANDS.
# NOTE: The most important bands "blue" "bnir" "green" "mtvi" "ndvi" "osavi" "red" "savi"
# NOTE: The most important time steps are "1" "2" "3" "4" "5" "6" "9" "34" "35" "36"
#-------------------------------------------------------------------------------
# The most important images are the first and last 3.
model_rforest_raw <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000))
model_rforest_approx <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000))
approx_importance <- ranger::importance(environment(model_rforest_approx)$result_ranger)
raw_importance    <- ranger::importance(environment(model_rforest_raw)$result_ranger)
importance <- dplyr::bind_cols(approx = approx_importance, raw = raw_importance,
                               names = names(approx_importance))
importance %>%
    #dplyr::filter(stringr::str_detect(names, pattern = "mtvi|ndvi|osavi|savi")) %>%
    dplyr::top_frac(n = 0.1, wt = raw) %>%
    dplyr::pull(names) %>%
    stringr::str_match("[^0-9]+") %>%
    #stringr::str_match("[0-9]+") %>%
    unique() %>%
    sort()
rm(list = ls(all.names=TRUE))

#-------------------------------------------------------------------------------
# CLASSIFICATION ONE INDEX - RANDOM FOREST - APPROX.
#-------------------------------------------------------------------------------
use_bands <- c("ndvi")
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/ndvi_rf-1000"

approx_brick_tb <- approx_brick_path %>%
    get_brick(n_expected_bands = 12) %>%
    # NOTE: Fix brick name!
    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
    dplyr::filter(band %in% use_bands) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = sprintf("Bands not found: %s", use_bands))

approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_select_bands(ndvi)

approx_cube <- sits::sits_cube(service = "BRICK",
                               name = "sentinel-bricks",
                               satellite = "SENTINEL2",
                               sensor = "MSI",
                               timeline = seq(unique(approx_brick_tb$img_date),
                                              by = 10, length.out = 36),
                               bands = approx_brick_tb$band,
                               files = approx_brick_tb$file_path)

model_rforest_approx <- sits::sits_train(approx_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 1000))
saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))

probs_approx_tb <- sits::sits_classify(data = approx_cube,
                                       ml_model = model_rforest_approx,
                                       multicores = 16, memsize = 4,
                                       output_dir = out_dir,
                                       version = "approx_001")

model_svn_approx <- sits::sits_train(approx_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 1000))
saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))

rm(list = ls(all.names=TRUE))



==========================================================================================





















#-------------------------------------------------------------------------------
# CLASSIFICATION ONE INDEX - RANDOM FOREST - RAW.
#-------------------------------------------------------------------------------
use_bands <- c("ndvi")
raw_brick_path    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_1_index_raw"

raw_brick_tb <- raw_brick_path %>%
    get_brick(n_expected_bands = 11) %>%
    dplyr::filter(band %in% use_bands) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = sprintf("Bands not found: %s", use_bands))

raw_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_select_bands(ndvi)

raw_cube <- sits::sits_cube(service = "BRICK",
                            name = "sentinel-bricks-raw",
                            satellite = "SENTINEL2",
                            sensor = "MSI",
                            timeline = seq(unique(raw_brick_tb$img_date),
                                           by = 10, length.out = 36),
                            bands = raw_brick_tb$band,
                            files = raw_brick_tb$file_path)

model_rforest_raw <- sits::sits_train(raw_samples_tb,
                                      ml_method = sits::sits_rfor(num_trees = 500))
saveRDS(model_rforest_raw, file = file.path(out_dir, "model_rforest_raw.rds"))

probs_raw_tb <- sits::sits_classify(data = raw_cube,
                                    ml_model = model_rforest_raw,
                                    multicores = 16, memsize = 4,
                                    output_dir = out_dir,
                                    version = "raw_001")
rm(list = ls(all.names=TRUE))
#-------------------------------------------------------------------------------
# CLASSIFICATION BANDS - RANDOM - FOREST
# NOTE: Error with c("blue", "bnir", "green","mtvi", "ndvi", "osavi", "red", "savi")
#-------------------------------------------------------------------------------
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#use_bands <- c("blue",  "green", "red")
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_3_bands"
use_bands <- c("blue",  "green", "ndvi",  "red",   "savi")
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_5_bands"

approx_brick_tb <- approx_brick_path %>%
    get_brick(n_expected_bands = 12) %>%
    # NOTE: Fix brick name!
    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
    dplyr::filter(band %in% use_bands) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = sprintf("Bands not found: %s", use_bands))

approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    #sits::sits_select_bands(blue, green, red)
    sits::sits_select_bands(blue, green, ndvi, red, savi)

approx_cube <- sits::sits_cube(service = "BRICK",
                               name = "sentinel-bricks",
                               satellite = "SENTINEL2",
                               sensor = "MSI",
                               timeline = seq(unique(approx_brick_tb$img_date),
                                              by = 10, length.out = 36),
                               bands = approx_brick_tb$band,
                               files = approx_brick_tb$file_path)

model_rforest_approx <- sits::sits_train(approx_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 1000))
saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))

probs_approx_tb <- sits::sits_classify(data = approx_cube,
                                       ml_model = model_rforest_approx,
                                       multicores = 16, memsize = 4,
                                       output_dir = out_dir,
                                       version = "approx_001")

rm(list = ls(all.names=TRUE))
#-------------------------------------------------------------------------------
# CLASSIFICATION 3 INDICES - RANDOM FOREST - APPROX
#-------------------------------------------------------------------------------
use_bands <- c("mtvi", "ndvi", "osavi")
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_3_indices"

approx_brick_tb <- approx_brick_path %>%
    get_brick(n_expected_bands = 11) %>%
    # NOTE: Fix brick name!
    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
    dplyr::filter(band %in% use_bands) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = sprintf("Bands not found: %s", use_bands))

approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_select_bands(mtvi, ndvi, osavi)

approx_cube <- sits::sits_cube(service = "BRICK",
                               name = "sentinel-bricks",
                               satellite = "SENTINEL2",
                               sensor = "MSI",
                               timeline = seq(unique(approx_brick_tb$img_date),
                                              by = 10, length.out = 36),
                               bands = approx_brick_tb$band,
                               files = approx_brick_tb$file_path)

model_rforest_approx <- sits::sits_train(approx_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 1000))
saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))

probs_approx_tb <- sits::sits_classify(data = approx_cube,
                                       ml_model = model_rforest_approx,
                                       multicores = 16, memsize = 4,
                                       output_dir = out_dir,
                                       version = "approx_001")
rm(list = ls(all.names=TRUE))

#-------------------------------------------------------------------------------
# CLASSIFICATION ALL BANDS AND INDEXES - RANDOM FOREST
#-------------------------------------------------------------------------------
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
porous_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/porous"
raw_brick_path    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"

raw_brick_tb <- raw_brick_path %>%
    get_brick(n_expected_bands = 12)
porous_brick_tb <- porous_brick_path %>%
    get_brick(n_expected_bands = 12)
approx_brick_tb <- approx_brick_path %>%
    get_brick(n_expected_bands = 12) %>%
    # NOTE: Fix brick name!
    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx"))

#saveRDS(raw_brick_tb,    file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/raw_brick_tb.rds")
#saveRDS(porous_brick_tb, file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/porous_brick_tb.rds")
#saveRDS(approx_brick_tb, file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/approx_brick_tb.rds")

# Create a cube for each brick.
approx_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
                            satellite = "SENTINEL2", sensor = "MSI",
                            timeline = seq(unique(approx_brick_tb$img_date),
                                           by = 10, length.out = 36) ,
                            bands = approx_brick_tb$band,
                            files = approx_brick_tb$file_path)
raw_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
                            satellite = "SENTINEL2", sensor = "MSI",
                            timeline = seq(unique(raw_brick_tb$img_date),
                                           by = 10, length.out = 36) ,
                            bands = raw_brick_tb$band,
                            files = raw_brick_tb$file_path)
porous_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
                            satellite = "SENTINEL2", sensor = "MSI",
                            timeline = seq(unique(porous_brick_tb$img_date),
                                           by = 10, length.out = 36) ,
                            bands = porous_brick_tb$band,
                            files = porous_brick_tb$file_path)

raw_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts()
porous_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/porous_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts()
approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts()

model_rforest_approx <- sits::sits_train(approx_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 500))
probs_approx_tb <- sits::sits_classify(data = approx_cube,
                                       ml_model = model_rforest_approx,
                                       multicores = 16, memsize = 4,
                                       output_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest",
                                       version = "approx_001")
saveRDS(model_rforest_approx, file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest/model_rforest_approx.rds")

# TODO: Run the classification for porous and raw.

# TODO: Run SVN?
#model_svn_approx <- sits::sits_train(approx_samples_tb, ml_method = sits::sits_svm())

model_rforest_raw <- sits::sits_train(raw_samples_tb,
                                         ml_method = sits::sits_rfor(num_trees = 500))


rm(list = ls(all.names=TRUE))

#-------------------------------------------------------------------------------
# CLASSIFICATION 3 INDEXES - LSTM - APPROX
#-------------------------------------------------------------------------------
use_bands <- c("mtvi", "ndvi", "osavi")
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/lstm_fcn_3_indices"

approx_brick_tb <- approx_brick_path %>%
    get_brick(n_expected_bands = 11) %>%
    # NOTE: Fix brick name!
    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
    dplyr::filter(band %in% use_bands) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = sprintf("Bands not found: %s", use_bands))

approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
    readRDS() %>%
    clean_ts() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
    sits::sits_select_bands(mtvi, ndvi, osavi)

approx_cube <- sits::sits_cube(service = "BRICK",
                               name = "sentinel-bricks",
                               satellite = "SENTINEL2",
                               sensor = "MSI",
                               timeline = seq(unique(approx_brick_tb$img_date),
                                              by = 10, length.out = 36),
                               bands = approx_brick_tb$band,
                               files = approx_brick_tb$file_path)


model_lstm_cnn_approx <- sits::sits_train (approx_samples_tb,
                                           ml_method = sits_LSTM_FCN())
saveRDS(model_lstm_cnn_approx, file = file.path(out_dir, "model_lstm-cnn_approx.rds"))

probs_approx_tb <- sits::sits_classify(data = approx_cube,
                                       ml_model = model_lstm_cnn_approx,
                                       multicores = 16, memsize = 4,
                                       output_dir = out_dir,
                                       version = "approx_001")
rm(list = ls(all.names=TRUE))
