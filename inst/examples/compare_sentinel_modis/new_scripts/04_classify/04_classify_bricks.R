#!/usr/bin/Rscript
#-------------------------------------------------------------------------------
# CLASSIFY SENTINEL-2 BRICKS
#-------------------------------------------------------------------------------
suppressMessages(library(caret))
suppressMessages(library(dplyr))
suppressMessages(library(raster))
suppressMessages(library(sits))

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
    stop("This script takes a parameters: An input file (RDS) of time series of samples.",  call. = FALSE)
}

samples_file <- args[[1]]

tmp_directory <- "/disks/d3/tmp"
dir.create(file.path(tmp_directory, "masked"))
raster::rasterOptions(tmpdir = tmp_directory)
raster::tmpDir()

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#---- Setup ----

band_combination <- list()
band_combination[[1]] <- c("blue", "bnir", "green", "nnir", "red", "swir1", "swir2")
band_combination[[2]] <- c("evi","ndmi", "ndvi")

brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"
b_type <- "approx" # Brick type.
version_number <- "006"

out_file_template <- samples_file %>%
    basename() %>%
    tools::file_path_sans_ext()

#---- Util ----

# Helper for doing the classification.
classify <- function(used_bands, used_labels, brick_dir, samples_file,
                     sits_method, out_dir, version_number){
    samples_tb <- samples_file %>%
        readRDS() %>%
        dplyr::mutate(label = dplyr::recode(label,
                                            Deforestatio = "deforestation",
                                            NatNonForest = "natural non-forest",
                                            NonForest    = "non-forest")) %>%
        dplyr::mutate(label = tools::toTitleCase(label)) %>%
        select_bands(used_bands) %>%
        dplyr::filter(label %in% used_labels) %>%
        ensurer::ensure_that(length(unique(.$label)) == length(used_labels),
                             err_desc = "Missing labels!") %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
    brick_tb <- brick_dir %>%
        get_brick_md() %>%
        dplyr::filter(brick_type == b_type, resolution == "10m") %>%
        dplyr::filter(band %in% used_bands) %>%
        dplyr::arrange(tile, img_date, band) %>%
        ensurer::ensure_that(!"" %in% .$band) %>%
        ensurer::ensure_that(length(unique(.$tile)) == 1,
                             err_desc = sprintf("More than one tile found: %s",
                                                brick_dir)) %>%
        ensurer::ensure_that(length(unique(.$img_date)) == 1,
                             err_desc = sprintf("More than one date found: %s",
                                                brick_dir)) %>%
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

#---- Classify using Random Forest ----

res_rf_1000 <- list()
for(used_bands in band_combination){
    used_labels <- c("Deforestation", "Forest", "Pasture")
    # TODO: leave all the labels, exectp for water
    stop("TODO")
    sits_method <- NULL
    out_dir <- NULL
    print("--------------------------------------------------------------------------------")
    print(sprintf("Using bands: %s", paste(used_bands, collapse = '-')))
    print(sprintf("Brick images: %s", brick_dir))
    sits_method <- sits::sits_rfor(num_trees = 1000)
    out_dir <- file.path(paste0("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results6/",
                                paste(b_type, out_file_template, sep = "-")),
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
