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

#---- Util ----

# Helper for doing the classification.
classify <- function(used_bands, used_labels, brick_dir, samples_file,
                     sits_method, out_dir, version_number){
    samples_tb <- samples_file %>%
        readRDS() %>%
        select_bands(used_bands) %>%
        dplyr::filter(label %in% used_labels) %>%
        ensurer::ensure_that(length(unique(.$label)) == length(used_labels),
                             err_desc = "Missing labels!") %>%
        clean_ts() %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
    brick_tb <- brick_dir %>%
        get_brick() %>%
        dplyr::filter(resolution == "10m") %>%
        dplyr::filter(band %in% used_bands) %>%
        ensurer::ensure_that(nrow(.) == length(used_bands),
                             err_desc = sprintf("Bands not found: %s",
                                                used_bands))
    stopifnot(all(match(brick_tb$band, colnames(samples_tb$time_series[[1]])[-1]) == 1:length(brick_tb$band)))
    cube <- sits::sits_cube(service = "BRICK",
                            name = "sentinel-bricks",
                            satellite = "SENTINEL2",
                            sensor = "MSI",
                            timeline = seq(unique(brick_tb$img_date),
                                           by = 10, length.out = 36),
                            bands = brick_tb$band,
                            files = brick_tb$file_path)
    write(sits::sits_bands(cube), file = file.path(out_dir, "sits_bands.txt"))
    write(sits::sits_labels(samples_tb)$label,
          file = file.path(out_dir, "sits_labels.txt"))
    model <- samples_tb %>%
        sits::sits_train(ml_method = sits_method) %>%
        (function(x){
             saveRDS(x, file = file.path(out_dir, "model.rds"))
             return(x)
        })
    probability_map <- sits::sits_classify(data = cube, ml_model = model,
                                           multicores = 16, memsize = 4,
                                           output_dir = out_dir,
                                           version = version_number)
    classification_map <- sits::sits_label_classification(probability_map,
                                                          smoothing = "bayesian",
                                                          output_dir = out_dir,
                                                          version = version_number)
    return(list(probability_map, classification_map))
}


#---- Setup ----


band_combination <- list()
#band_combination[[1]] <- c("swir1")
#band_combination[[2]] <- c("swir2")
#band_combination[[3]] <- c("blue", "bnir", "green", "red")
#band_combination[[4]] <- c("green", "ndvi")
#band_combination[[5]] <- c("blue", "green", "ndvi")
#band_combination[[6]] <- c("ndvi")
band_combination[[1]] <- c("blue", "bnir", "green", "nnir", "red", "swir1", "swir2")
band_combination[[2]] <- c("evi","ndmi", "ndvi")
band_combination[[3]] <- c("evi", "ndvi", "savi")

samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb_v2.rds"
brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
version_number <- "005"

#---- Classify using Random Forest ----


res_rf_1000 <- list()
for(used_bands in band_combination){
    used_labels <- c("Deforestation", "Forest", "Pasture")
    sits_method <- NULL
    out_dir <- NULL
    print("--------------------------------------------------------------------------------")
    print(sprintf("Using bands: %s", paste(used_bands, collapse = '-')))
    print(sprintf("Brick images: %s", brick_dir))
    sits_method <- sits::sits_rfor(num_trees = 1000)
    out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2",
                         paste(used_bands, collapse = '-'),
                         "random-forest_1000/")
    if (!dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE)
    print(sprintf("Saving results to: %s", out_dir))
    res_rf_1000[[paste(used_bands, collapse = '-')]] <- classify(used_bands,
                                                                 used_labels,
                                                                 brick_dir,
                                                                 samples_file,
                                                                 sits_method,
                                                                 out_dir,
                                                                 version_number)
}


#---- Classify using the first or last 2 images of the brick without Deforestation ----


for(used_bands in band_combination){
    #-------------------
    selected_labels <- c("Pasture", "Forest")
    slice_where <- "first"
    #slice_where <- "last"
    slice_n_rows <- 2
    result_base_dir <- paste0("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_", slice_where, "_v2")
    brick_dir <- paste0("/disks/d3/brick_sentinel2_interpolated/", paste(slice_where, slice_n_rows, sep = "_"))
    #-------------------
    sits_method <- sits::sits_rfor(num_trees = 1000)
    out_dir <- file.path(result_base_dir, paste(used_bands, collapse = '-'),
                         "random-forest_1000/")
    if (!dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE)
    brick_tb <- brick_dir %>%
        list.files(pattern = "*[.]vrt", full.names = TRUE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(fname = tools::file_path_sans_ext(basename(file_path))) %>%
        tidyr::separate(col = fname, into = c(NA, NA, NA, "tile", "img_date",
                                              "band", "resolution"),
                        sep = "_") %>%
        dplyr::mutate(img_date = stringr::str_sub(img_date, 1, 8),
                      img_date = lubridate::as_date(img_date),
                      file_band = band,
                      band = dplyr::recode(file_band,
                                           "B02" = "blue",
                                           "B03" = "green",
                                           "B04" = "red",
                                           "B08" = "bnir",
                                           "B8A" = "nnir",
                                           "B11" = "swir1",
                                           "B12" = "swir2")) %>%
        dplyr::filter(resolution == "10m") %>%
        dplyr::filter(band %in% used_bands) %>%
        dplyr::arrange(band) %>%
        ensurer::ensure_that(nrow(.) == length(used_bands),
                             err_desc = sprintf("Bands not found: %s",
                                                used_bands))
    samples_tb <- samples_file %>%
        readRDS() %>%
        select_bands(used_bands) %>%
        clean_ts() %>%
        dplyr::filter(label %in% selected_labels) %>%
        #dplyr::mutate(end_date = start_date + 11) %>%
        dplyr::mutate(time_series = purrr::map(time_series, slice_n,
                                               n = slice_n_rows,
                                               where = slice_where)) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
    samples_tb$start_date <- samples_tb$time_series[[1]][[1]][[1]] - 4
    samples_tb$end_date <- samples_tb$time_series[[1]][[1]][[2]] + 4
    stopifnot(all(match(brick_tb$band, colnames(samples_tb$time_series[[1]])[-1]) == 1:length(brick_tb$band)))
    cube <- sits::sits_cube(service = "BRICK",
                            name = "sentinel-bricks",
                            satellite = "SENTINEL2",
                            sensor = "MSI",
                            timeline = samples_tb$time_series[[1]][[1]],
                            bands = brick_tb$band,
                            files = brick_tb$file_path)
    write(sits::sits_bands(cube), file = file.path(out_dir, "sits_bands.txt"))
    write(sits::sits_labels(samples_tb)$label,
          file = file.path(out_dir, "sits_labels.txt"))
    model <- samples_tb %>%
        sits::sits_train(ml_method = sits_method) %>%
        (function(x){
             saveRDS(x, file = file.path(out_dir, "model.rds"))
             return(x)
        })
    probability_map <- sits::sits_classify(data = cube, ml_model = model,
                                           multicores = 16, memsize = 1,
                                           output_dir = out_dir,
                                           version = version_number)
    classification_map <- sits::sits_label_classification(probability_map,
                                                          smoothing = "bayesian",
                                                          output_dir = out_dir,
                                                          version = version_number)
}



#---- Classify using SVM ----


#res_svm <- list()
#for(used_bands in band_combination){
#    sits_method <- NULL
#    out_dir <- NULL
#    print("--------------------------------------------------------------------------------")
#    print(sprintf("Using bands: %s", paste(used_bands, collapse = '-')))
#    print(sprintf("Brick images: %s", brick_dir))
#    sits_method <- sits::sits_svm()
#    out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results2/approx_v2",
#                         paste(used_bands, collapse = '-'),
#                         "svm/")
#    if (!dir.exists(out_dir))
#        dir.create(out_dir, recursive = TRUE)
#    print(sprintf("Saving results to: %s", out_dir))
#    res_svm[[paste(used_bands, collapse = '-')]] <- classify(used_bands,
#                                                             brick_dir,
#                                                             samples_file,
#                                                             sits_method,
#                                                             out_dir)
#}

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

##-------------------------------------------------------------------------------
## CLASSIFICATION RANDOM FOREST - APPROX - EVI2 NDVI
##-------------------------------------------------------------------------------
#use_bands <- c("evi2", "ndvi")
#out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results2/approx", paste(use_bands, collapse = '-'), "random-forest_1000/")
#brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds"
#version_number <- "001"
#dir.create(out_dir)
#brick_tb <- brick_dir %>%
#    get_brick() %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) == length(use_bands),
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#samples_tb <- samples_file %>%
#    readRDS() %>%
#    clean_ts() %>%
#    select_bands(use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
#cube <- sits::sits_cube(service = "BRICK",
#                        name = "sentinel-bricks",
#                        satellite = "SENTINEL2",
#                        sensor = "MSI",
#                        timeline = seq(unique(brick_tb$img_date),
#                                       by = 10, length.out = 36),
#                        bands = brick_tb$band,
#                        files = brick_tb$file_path)
#model <- samples_tb %>%
#    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000)) %>%
#    (function(x){
#         saveRDS(x, file = file.path(out_dir, "model.rds"))
#         return(x)
#    })
#probability_map <- sits::sits_classify(data = cube, ml_model = model,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = version_number)
#classification_map <- sits::sits_label_classification(probability_map,
#                                                      smoothing = "bayesian",
#                                                      output_dir = out_dir)
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION RANDOM FOREST - APPROX - GREEN,NDVI,RED,SAVI
##-------------------------------------------------------------------------------
#use_bands <- c("green", "ndvi", "red", "savi")
#out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results2/approx", paste(use_bands, collapse = '-'), "random-forest_1000/")
#brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds"
#version_number <- "001"
#dir.create(out_dir)
#brick_tb <- brick_dir %>%
#    get_brick() %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) == length(use_bands),
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#samples_tb <- samples_file %>%
#    readRDS() %>%
#    clean_ts() %>%
#    select_bands(use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
#cube <- sits::sits_cube(service = "BRICK",
#                        name = "sentinel-bricks",
#                        satellite = "SENTINEL2",
#                        sensor = "MSI",
#                        timeline = seq(unique(brick_tb$img_date),
#                                       by = 10, length.out = 36),
#                        bands = brick_tb$band,
#                        files = brick_tb$file_path)
#model <- samples_tb %>%
#    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000)) %>%
#    (function(x){
#         saveRDS(x, file = file.path(out_dir, "model.rds"))
#         return(x)
#    })
#probability_map <- sits::sits_classify(data = cube, ml_model = model,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = version_number)
#classification_map <- sits::sits_label_classification(probability_map,
#                                                      smoothing = "bayesian",
#                                                      output_dir = out_dir)
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION RANDOM FOREST - APPROX - BNIR,EVI2,NDVI
##-------------------------------------------------------------------------------
#use_bands <- c("bnir", "evi2", "ndvi")
#out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results2/approx", paste(use_bands, collapse = '-'), "random-forest_1000/")
#brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds"
#version_number <- "001"
#dir.create(out_dir)
#brick_tb <- brick_dir %>%
#    get_brick() %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) == length(use_bands),
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#samples_tb <- samples_file %>%
#    readRDS() %>%
#    clean_ts() %>%
#    select_bands(use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
#cube <- sits::sits_cube(service = "BRICK",
#                        name = "sentinel-bricks",
#                        satellite = "SENTINEL2",
#                        sensor = "MSI",
#                        timeline = seq(unique(brick_tb$img_date),
#                                       by = 10, length.out = 36),
#                        bands = brick_tb$band,
#                        files = brick_tb$file_path)
#model <- samples_tb %>%
#    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000)) %>%
#    (function(x){
#         saveRDS(x, file = file.path(out_dir, "model.rds"))
#         return(x)
#    })
#probability_map <- sits::sits_classify(data = cube, ml_model = model,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = version_number)
#classification_map <- sits::sits_label_classification(probability_map,
#                                                      smoothing = "bayesian",
#                                                      output_dir = out_dir)
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION RANDOM FOREST - APPROX - GREEN NDVI
##-------------------------------------------------------------------------------
#use_bands <- c("green", "ndvi")
#out_dir <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results2/approx", paste(use_bands, collapse = '-'), "random-forest_1000/")
#brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds"
#version_number <- "001"
#
#dir.create(out_dir)
#brick_tb <- brick_dir %>%
#    get_brick() %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) == length(use_bands),
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#samples_tb <- samples_file %>%
#    readRDS() %>%
#    clean_ts() %>%
#    select_bands(use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
#cube <- sits::sits_cube(service = "BRICK",
#                        name = "sentinel-bricks",
#                        satellite = "SENTINEL2",
#                        sensor = "MSI",
#                        timeline = seq(unique(brick_tb$img_date),
#                                       by = 10, length.out = 36),
#                        bands = brick_tb$band,
#                        files = brick_tb$file_path)
#model <- samples_tb %>%
#    sits::sits_train(ml_method = sits::sits_rfor(num_trees = 1000)) %>%
#    (function(x){
#         saveRDS(x, file = file.path(out_dir, "model.rds"))
#         return(x)
#    })
#probability_map <- sits::sits_classify(data = cube, ml_model = model,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = version_number)
#classification_map <- sits::sits_label_classification(probability_map,
#                                                      smoothing = "bayesian",
#                                                      output_dir = out_dir)
#                                       version = version_number)
#rm(list = ls(all.names=TRUE))

#===============================================================================


##-------------------------------------------------------------------------------
## CLASSIFICATION ONE INDEX - RANDOM FOREST - APPROX.
##-------------------------------------------------------------------------------
#use_bands <- c("ndvi")
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/ndvi_rf-1000"
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 12) %>%
#    # NOTE: Fix brick name!
#    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    sits::sits_select_bands(ndvi)
#approx_cube <- sits::sits_cube(service = "BRICK",
#                               name = "sentinel-bricks",
#                               satellite = "SENTINEL2",
#                               sensor = "MSI",
#                               timeline = seq(unique(approx_brick_tb$img_date),
#                                              by = 10, length.out = 36),
#                               bands = approx_brick_tb$band,
#                               files = approx_brick_tb$file_path)
#model_rforest_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 1000))
#saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_rforest_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = "approx_001")
#model_svn_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 1000))
#saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION OF PCI 1 & 2 MADE OF RGB AND BNIR
##-------------------------------------------------------------------------------
#use_bands <- c("pc1rgbnir", "pc2rgbnir")
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/pc12rgbnir_rf-1000"
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 14) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    sits::sits_select_bands(pc1rgbnir, pc2rgbnir)
#approx_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
#                               satellite = "SENTINEL2", sensor = "MSI",
#                               timeline = seq(unique(approx_brick_tb$img_date),
#                                              by = 10, length.out = 36),
#                               bands = approx_brick_tb$band,
#                               files = approx_brick_tb$file_path)
#model_rforest_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 1000))
#saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_rforest_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = "approx_001")
#==========================================================================================
##-------------------------------------------------------------------------------
## CLASSIFICATION ONE INDEX - RANDOM FOREST - RAW.
##-------------------------------------------------------------------------------
#use_bands <- c("ndvi")
#raw_brick_path    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_1_index_raw"
#raw_brick_tb <- raw_brick_path %>%
#    get_brick(n_expected_bands = 11) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#raw_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    sits::sits_select_bands(ndvi)
#raw_cube <- sits::sits_cube(service = "BRICK",
#                            name = "sentinel-bricks-raw",
#                            satellite = "SENTINEL2",
#                            sensor = "MSI",
#                            timeline = seq(unique(raw_brick_tb$img_date),
#                                           by = 10, length.out = 36),
#                            bands = raw_brick_tb$band,
#                            files = raw_brick_tb$file_path)
#model_rforest_raw <- sits::sits_train(raw_samples_tb,
#                                      ml_method = sits::sits_rfor(num_trees = 500))
#saveRDS(model_rforest_raw, file = file.path(out_dir, "model_rforest_raw.rds"))
#probs_raw_tb <- sits::sits_classify(data = raw_cube,
#                                    ml_model = model_rforest_raw,
#                                    multicores = 16, memsize = 4,
#                                    output_dir = out_dir,
#                                    version = "raw_001")
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION BANDS - RANDOM - FOREST
## NOTE: Error with c("blue", "bnir", "green","mtvi", "ndvi", "osavi", "red", "savi")
##-------------------------------------------------------------------------------
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
##use_bands <- c("blue",  "green", "red")
##out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_3_bands"
#use_bands <- c("blue",  "green", "ndvi",  "red",   "savi")
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_5_bands"
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 12) %>%
#    # NOTE: Fix brick name!
#    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    #sits::sits_select_bands(blue, green, red)
#    sits::sits_select_bands(blue, green, ndvi, red, savi)
#approx_cube <- sits::sits_cube(service = "BRICK",
#                               name = "sentinel-bricks",
#                               satellite = "SENTINEL2",
#                               sensor = "MSI",
#                               timeline = seq(unique(approx_brick_tb$img_date),
#                                              by = 10, length.out = 36),
#                               bands = approx_brick_tb$band,
#                               files = approx_brick_tb$file_path)
#model_rforest_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 1000))
#saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_rforest_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = "approx_001")
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION 3 INDICES - RANDOM FOREST - APPROX
##-------------------------------------------------------------------------------
#use_bands <- c("mtvi", "ndvi", "osavi")
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest_3_indices"
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 11) %>%
#    # NOTE: Fix brick name!
#    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    sits::sits_select_bands(mtvi, ndvi, osavi)
#approx_cube <- sits::sits_cube(service = "BRICK",
#                               name = "sentinel-bricks",
#                               satellite = "SENTINEL2",
#                               sensor = "MSI",
#                               timeline = seq(unique(approx_brick_tb$img_date),
#                                              by = 10, length.out = 36),
#                               bands = approx_brick_tb$band,
#                               files = approx_brick_tb$file_path)
#model_rforest_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 1000))
#saveRDS(model_rforest_approx, file = file.path(out_dir, "model_rforest_approx.rds"))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_rforest_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = "approx_001")
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION ALL BANDS AND INDEXES - RANDOM FOREST
##-------------------------------------------------------------------------------
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#porous_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/porous"
#raw_brick_path    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"
#raw_brick_tb <- raw_brick_path %>%
#    get_brick(n_expected_bands = 12)
#porous_brick_tb <- porous_brick_path %>%
#    get_brick(n_expected_bands = 12)
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 12) %>%
#    # NOTE: Fix brick name!
#    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx"))
##saveRDS(raw_brick_tb,    file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/raw_brick_tb.rds")
##saveRDS(porous_brick_tb, file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/porous_brick_tb.rds")
##saveRDS(approx_brick_tb, file =  "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/approx_brick_tb.rds")
## Create a cube for each brick.
#approx_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
#                            satellite = "SENTINEL2", sensor = "MSI",
#                            timeline = seq(unique(approx_brick_tb$img_date),
#                                           by = 10, length.out = 36) ,
#                            bands = approx_brick_tb$band,
#                            files = approx_brick_tb$file_path)
#raw_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
#                            satellite = "SENTINEL2", sensor = "MSI",
#                            timeline = seq(unique(raw_brick_tb$img_date),
#                                           by = 10, length.out = 36) ,
#                            bands = raw_brick_tb$band,
#                            files = raw_brick_tb$file_path)
#porous_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
#                            satellite = "SENTINEL2", sensor = "MSI",
#                            timeline = seq(unique(porous_brick_tb$img_date),
#                                           by = 10, length.out = 36) ,
#                            bands = porous_brick_tb$band,
#                            files = porous_brick_tb$file_path)
#raw_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts()
#porous_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/porous_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts()
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts()
#model_rforest_approx <- sits::sits_train(approx_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 500))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_rforest_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest",
#                                       version = "approx_001")
#saveRDS(model_rforest_approx, file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/random_forest/model_rforest_approx.rds")
## TODO: Run the classification for porous and raw.
## TODO: Run SVN?
##model_svn_approx <- sits::sits_train(approx_samples_tb, ml_method = sits::sits_svm())
#model_rforest_raw <- sits::sits_train(raw_samples_tb,
#                                         ml_method = sits::sits_rfor(num_trees = 500))
#rm(list = ls(all.names=TRUE))
##-------------------------------------------------------------------------------
## CLASSIFICATION 3 INDEXES - LSTM - APPROX
##-------------------------------------------------------------------------------
#use_bands <- c("mtvi", "ndvi", "osavi")
#approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"
#out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results/lstm_fcn_3_indices"
#approx_brick_tb <- approx_brick_path %>%
#    get_brick(n_expected_bands = 11) %>%
#    # NOTE: Fix brick name!
#    dplyr::mutate(band = stringr::str_replace(band, "_masked", "_approx")) %>%
#    dplyr::filter(band %in% use_bands) %>%
#    ensurer::ensure_that(nrow(.) > 0,
#                         err_desc = sprintf("Bands not found: %s", use_bands))
#approx_samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds" %>%
#    readRDS() %>%
#    clean_ts() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
#    sits::sits_select_bands(mtvi, ndvi, osavi)
#approx_cube <- sits::sits_cube(service = "BRICK",
#                               name = "sentinel-bricks",
#                               satellite = "SENTINEL2",
#                               sensor = "MSI",
#                               timeline = seq(unique(approx_brick_tb$img_date),
#                                              by = 10, length.out = 36),
#                               bands = approx_brick_tb$band,
#                               files = approx_brick_tb$file_path)
#model_lstm_cnn_approx <- sits::sits_train (approx_samples_tb,
#                                           ml_method = sits_LSTM_FCN())
#saveRDS(model_lstm_cnn_approx, file = file.path(out_dir, "model_lstm-cnn_approx.rds"))
#probs_approx_tb <- sits::sits_classify(data = approx_cube,
#                                       ml_model = model_lstm_cnn_approx,
#                                       multicores = 16, memsize = 4,
#                                       output_dir = out_dir,
#                                       version = "approx_001")
#rm(list = ls(all.names=TRUE))
