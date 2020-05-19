#-------------------------------------------------------------------------------
# GET THE TIME SERIES OF THE SAMPLES USING SENTINEL-2 BRICKS.
#-------------------------------------------------------------------------------

library(dplyr)
library(parallel)
library(readr)
library(sits)
library(sf)

tmp_directory <- "/disks/d3/tmp"
dir.create(file.path(tmp_directory, "masked"))
raster::rasterOptions(tmpdir = tmp_directory)
raster::tmpDir()

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

sample_shp <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/samples_20LKP_v2.shp"
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"

approx_brick_tb <- approx_brick_path %>%
    get_brick() %>%
    dplyr::filter(resolution == "10m") %>%
    ensurer::ensure_that(all(table(.$file_band) == 1),
                         err_desc = "Repeated bands found!") %>%
    ensurer::ensure_that(!any(is.na(.$band)), err_desc = "Unknown bands!")

# Create a coverage for each brick.
approx_cube <- sits::sits_cube(service = "BRICK", name = "sentinel-bricks",
                            satellite = "SENTINEL2", sensor = "MSI",
                            timeline = seq(unique(approx_brick_tb$img_date),
                                           by = 10, length.out = 36) ,
                            bands = approx_brick_tb$band,
                            files = approx_brick_tb$file_path)

# Export SHP to CSV.
# NOTE: This is necessary because sits::sits_get_data ignores the shp's labels.
sample_csv <- sample_shp %>%
    sf::read_sf() %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::rename(label = Label) %>%
    dplyr::mutate(start_date = "2018-08-01",
                  end_date   = "2019-07-31") %>%
    dplyr::select(longitude, latitude, start_date, end_date, label) %>%
    (function(x){
        csv_file <- tempfile(pattern = "samples_shp2csv_", fileext = ".csv")
        readr::write_csv(x, path = csv_file)
        return(csv_file)
    })

# Get the time series for each sample.
approx_samples_tb <- sits::sits_get_data(approx_cube, file = sample_csv)

# Report invalid samples.
approx_samples_tb %>%
    clean_ts(report = TRUE) %>%
    (function(x){
         print("Bands available:")
         print(sort(sits::sits_bands(x)))
         print("Number of samples:")
         print(nrow(x))
         invisible(x)
    }) %>%
    dplyr::mutate(na_col   = purrr::map(time_series, function(x){
                  na_vec <- sapply(lapply(x, is.na), sum)
                  return(names(na_vec[na_vec > 0]))
                })) %>%
    dplyr::pull(na_col) %>%
    unlist() %>%
    table() %>%
    print()

# Save clean samples.
approx_samples_tb %>%
    clean_ts() %>%
    (function(x){
         print("Bands available:")
         print(sort(sits::sits_bands(x)))
         print("Number of samples:")
         print(nrow(x))
         invisible(x)
         plot(x)
    }) %>%
    saveRDS(file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb_v2.rds")



################################################################################
# SAMPLES FOR THE FIRST CLASSIFICATION
################################################################################

# Export SHP to CSV.
sample4first_shp <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/validation_tudo_final4.shp"

samples4first_csv <- sample4first_shp %>%
    ensurer::ensure_that(file.exists(.), err_desc = "File not found!") %>%
    sf::read_sf() %>%
    sf::st_transform(crs = 4326L) %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(label = dplyr::recode(label,
                                            Deforestatio = "Deforestation",
                                            Forest       = "Forest",
                                            NatNonForest = "Natural_Non-Forest",
                                            NonFerest    = "Non-Forest",
                                            NonForest    = "Non-Forest",
                                            Pasture      = "Pasture",
                                            Water        = "Water")) %>%
    tidyr::drop_na() %>%
    ensurer::ensure_that(length(unique(.$label)) == 6,
                         err_desc = sprintf("Unknown labels found: %s",
                                            paste(sort(unique(.$label))))) %>%
    ensurer::ensure_that(!all(is.na(.$label)),
                         err_desc = "Labels cannot be NA!") %>%
    dplyr::mutate(start_date  = "2018-08-01",
                  end_date    = "2019-07-31",
                  coverage    = layer,
                  time_series = NA) %>%
    ensurer::ensure_that(!all(is.na(.$coverage)),
                         err_desc = "Coverage must describe the samples' origin file!") %>%
    dplyr::select(longitude, latitude, start_date,
                  end_date, label, coverage, time_series) %>%
    dplyr::mutate(start_date = "2018-08-01",
                  end_date   = "2019-07-31") %>%
    (function(x){
        csv_file <- tempfile(pattern = "samples4first_shp2csv_", fileext = ".csv")
        readr::write_csv(x, path = csv_file)
        return(csv_file)
    })

# Get the time series for each sample.
samples4first_tb <- sits::sits_get_data(approx_cube, file = samples4first_csv)

# Report invalid samples.
samples4first_tb %>%
    clean_ts(report = TRUE) %>%
    (function(x){
         print("Bands available:")
         print(sort(sits::sits_bands(x)))
         print("Number of samples:")
         print(nrow(x))
         invisible(x)
    }) %>%
    dplyr::mutate(na_col   = purrr::map(time_series, function(x){
                  na_vec <- sapply(lapply(x, is.na), sum)
                  return(names(na_vec[na_vec > 0]))
                })) %>%
    dplyr::pull(na_col) %>%
    unlist() %>%
    table() %>%
    print()

# NOTE: Some NAs in NDMI were interpolated.
samples4first_tb$time_series[[ 41]]$ndmi <- approx(samples4first_tb$time_series[[ 41]]$ndmi, n = 36)$y
samples4first_tb$time_series[[382]]$ndmi <- approx(samples4first_tb$time_series[[382]]$ndmi, n = 36)$y
samples4first_tb$time_series[[441]]$ndmi <- approx(samples4first_tb$time_series[[441]]$ndmi, n = 36)$y

original_layer <- sample4first_shp %>%
    sf::read_sf() %>%
    sf::st_transform(crs = 4326L) %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(lon6 = round(longitude * 1000000),
                  lat6 = round(latitude  * 1000000)) %>%
    dplyr::select(lon6, lat6, layer)

samples4first_tb %>%
    dplyr::mutate(lon6 = round(longitude * 1000000),
                  lat6 = round(latitude  * 1000000)) %>%
    dplyr::left_join(original_layer, by = c("lon6", "lat6")) %>%
    ensurer::ensure_that(!all(is.na(.$layer))) %>%
    dplyr::mutate(cube = layer) %>%
    dplyr::select(-lon6, -lat6, -layer) %>%
    ensurer::ensure_that(!all(is.na(.$cube))) %>%
    saveRDS(file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/validation_tudo_final4.rds")

