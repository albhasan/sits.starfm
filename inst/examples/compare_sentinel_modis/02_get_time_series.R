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

raw_brick_path    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"
porous_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/porous"
approx_brick_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_interpolated/approx"

#raw_brick_tb <- raw_brick_path %>%
#    get_brick()
#porous_brick_tb <- porous_brick_path %>%
#    get_brick()
approx_brick_tb <- approx_brick_path %>%
    get_brick() %>%
    dplyr::filter(resolution == "10m") %>%
    ensurer::ensure_that(all(table(.$file_band) == 1),
                         err_desc = "Repeated bands found!") %>%
    ensurer::ensure_that(!any(is.na(.$band)), err_desc = "Unknown bands!")

# Create a coverage for each brick.
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
    (function(x){
         xy <- x %>%
             sf::st_coordinates() %>%
             magrittr::set_colnames(c("longitude", "latitude")) %>%
             tidyr::as_tibble()
         x %>%
             dplyr::bind_cols(xy) %>%
             return()
    }) %>%
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
#raw_samples_tb    <- sits::sits_get_data(raw_cube,    file = sample_csv)
#porous_samples_tb <- sits::sits_get_data(porous_cube, file = sample_csv)
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


#saveRDS(raw_samples_tb   , file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/raw_samples_tb_v2.rds")
#saveRDS(porous_samples_tb, file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/porous_samples_tb_v2.rds")
