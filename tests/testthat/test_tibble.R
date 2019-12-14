# TEST THE FUNCTION IN tibble.R

library(sits.starfm)

test_that("Only image files are retrieved.", {
    # Create some test files
    files <- tempfile(fileext = c(".tif", ".TIF", ".hdf", ".HDF", ".fake", ".FAKE"))
    file.create(files)

    # TODO: Call the function on files and check they were found.
#    res <- tempdir() %>%
#        build_tibble()
#    expect_equal(nrow(res), length(files) - 2)
})

test_that("Parse a Landsat's filename", {
    res <- parse_img_name("/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/landsat8/LC08_226_068/LC08_L1TP_226068_20180308_20180320_01_T2_pixel_qa.tif")
    expect_equal(length(res), 9)
    expect_true(all( c("header", "level", "path_row", "acquisition",
                        "processing", "collection", "category") %in% names(res)))
})

test_that("Parse a MODIS's filename", {
    res <- parse_img_name("/home/alber/MOD13Q1/2013/MOD13Q1.A2013353.h14v09.006.2015272025344.hdf")
    expect_equal(length(res), 6)
    expect_true(all( c("product", "acquisition", "tile", "collection", "production", "format") %in% names(res)))
})

