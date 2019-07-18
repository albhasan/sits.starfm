context("Test the bricks")
library(sits.starfm)
library(dplyr)
library(testthat)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
path_to_bricks <- c(
    mod13         = file.path(base_path, "data", "raster", "bricks_modis_cropped"),
    l8mod_interp  = file.path(base_path, "data", "raster", "brick_interp"),
    l8mod_starfm  = file.path(base_path, "data", "raster", "brick_starfm"),
    l8_simple     = file.path(base_path, "data", "raster", "brick_simple"),
    l8_mask_cloud = file.path(base_path, "data", "raster", "brick_mask_cloud")
)

no_dirs <- function(){
    !all(vapply(path_to_bricks, dir.exists, logical(1)))
}

check_preconditions <- function() {
    if (no_dirs())
        testthat::skip("Some directories are unavailable")
}

test_that("Interpolated bricks", {
    check_preconditions()
<<<<<<< HEAD
    path_to_bricks["l8mod_interp"] %>% 
        list.files(pattern = "*tif$") %>%
        length() %>% 
        testthat::expect_gt(., 0)
=======
    path_to_bricks["l8mod_interp"] %>% list.files(pattern = "*tif$") %>%
        length() %>% testthat::expect_gt(., 0)
>>>>>>> fa8749fbc73d539dad9b4b7e8477cb79e3d569a0
})


# test_that("str_length is number of characters", {
#     expect_equal(str_length("a"), 1)
#     expect_equal(str_length("ab"), 2)
#     expect_equal(str_length("abc"), 3)
# })
#
# test_that("str_length of factor is length of level", {
#     expect_equal(str_length(factor("a")), 1)
#     expect_equal(str_length(factor("ab")), 2)
#     expect_equal(str_length(factor("abc")), 3)
# })
#
# test_that("str_length of missing is missing", {
#     expect_equal(str_length(NA), NA_integer_)
#     expect_equal(str_length(c(NA, 1)), c(NA, 1))
#     expect_equal(str_length("NA"), 2)
# })
