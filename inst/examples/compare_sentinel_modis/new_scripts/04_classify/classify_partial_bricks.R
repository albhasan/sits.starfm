#!/usr/bin/Rscript
#-------------------------------------------------------------------------------
# CLASSIFY SENTINEL-2 BRICKS
#-------------------------------------------------------------------------------
.Deprecated("")
suppressMessages(library(dplyr))
suppressMessages(library(raster))
suppressMessages(library(sits))

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
    stop("This script takes a parameter: An input file (RDS) of time series of samples.",  call. = FALSE)
}

samples_file <- args[[1]]

tmp_directory <- "/disks/d3/tmp"
dir.create(file.path(tmp_directory, "masked"))
raster::rasterOptions(tmpdir = tmp_directory)
raster::tmpDir()

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#---- Util ----

#---- Setup ----

brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw/first_2"
version_number <- "006"

out_file_template <- samples_file %>%
    basename() %>%
    tools::file_path_sans_ext()

#---- Classify using Random Forest ----

slice_where <- "first"
slice_n_rows <- 2
brick_dir <- "/disks/d3/brick_sentinel2_raw/first_2"
b_type <- "approx" # Brick type.

#------------------------------------------------------------------------------
used_bands <- NA
original_source <- NA

if (stringr::str_detect(samples_file, "_all_bands_")) {
    classification_type <- "bands"
    used_bands <- c("blue", "bnir", "green", "nnir", "red", "swir1", "swir2")
    original_source <- "validation_evi_ndmi_ndvi_v1" # NOTE: Cross bands and indexes when selecting the samples to use!!!
}else if (stringr::str_detect(samples_file, "_indices_")) {
    classification_type <- "indices"
    used_bands <- c("evi","ndmi", "ndvi")
    original_source <- "validation_all_bands_v1"     # NOTE: Cross bands and indexes when selecting the samples to use!!!
}else{
    stop("Unknown type of classification!")
}

samples_tb <- samples_file %>%
    readRDS() %>%
    dplyr::mutate(label = dplyr::recode(label,
                                        "Deforestatio"       = "Forest",
                                        "Forest"             = "Forest",
                                        "Pasture"            = "Pasture",
                                        "NonForest"          = "Non-Forest",
                                        "NatNonForest"       = "Natural_Non-Forest",
                                        "Water"              = "Water",
                                        .default = NA_character_)) %>%
    dplyr::filter(!label %in% "Water") %>%
    ensurer::ensure_that(!all(is.na(.$label)), err_desc = "Missing labels!") %>%
    ensurer::ensure_that(length(unique(.$label)) == 4,
                         err_desc = "Wrong labels!") %>%
    select_bands(used_bands) %>%
    dplyr::mutate(time_series = purrr::map(time_series, slice_n,
                                           n = slice_n_rows,
                                           where = slice_where)) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples missing")
samples_tb$start_date <- samples_tb$time_series[[1]][[1]][[1]] - 4
samples_tb$end_date <- samples_tb$time_series[[1]][[1]][[2]] + 4

sits_method <- sits::sits_rfor(num_trees = 1000)

out_dir <- file.path(paste0("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results6/",
                            paste(b_type, out_file_template, set = '-')),
                     paste(used_bands, collapse = '-'),
                     paste(slice_where,  slice_n_rows, sep = '_'),
                     "random-forest_1000/")

#--------
if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

# TODO: check if brick_type is what I think it is.
brick_tb <- brick_dir %>%
    list.files(pattern = "*[.]vrt", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(fname = tools::file_path_sans_ext(basename(file_path))) %>%
    tidyr::separate(col = fname, into = c(NA, NA, NA, "tile", "img_date",
                                          "band", "brick_type", "resolution"),
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
    dplyr::filter(brick_type == b_type, resolution == "10m") %>%
    dplyr::filter(band %in% used_bands) %>%
    dplyr::arrange(band) %>%
    ensurer::ensure_that(nrow(.) == length(used_bands),
                         err_desc = sprintf("Bands not found: %s",
                                            used_bands))
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

