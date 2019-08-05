suppressMessages(library(sits.starfm))
suppressMessages(library(dplyr))
suppressMessages(library(testthat))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

path_to_images <- c(
    hls = file.path(base_path, "data/raster/harmonized_landsat_sentinel2/data/hls")
)

no_dirs <- function(){
    !all(vapply(path_to_images, dir.exists, logical(1)))
}

check_preconditions <- function() {
    if (no_dirs())
        testthat::skip("Some directories are unavailable")
}

test_that("HLS images available", {
    check_preconditions()
    path_to_images["hls"] %>% 
        list.files(pattern = "*hdf$", recursive = TRUE) %>%
        length() %>%
        testthat::expect_gt(., 0)
    #build_hls_tibble(in_dir = path_to_images["hls"], pattern = "*hdf$", 
    #                 prodes_year_start = "-08-01") %>%
    #    nrow() %>%
    #    testthat::expect_gt(., 0)
})

