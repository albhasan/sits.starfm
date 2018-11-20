context("Utilitary functions")
library(sits.starfm)
library(dplyr)


test_that("add_missing_dates works", {
    x <- tibble::tibble(sat_image = c("LC08_L1TP_225063_20171008_20171023_01_T2",
                                      "LC08_L1TP_225063_20171109_20171121_01_T2"),
                        img_date = as.Date(c("2017-10-08", "2017-11-09")))
    d_res <- add_missing_dates(x = x, step = 16, prodes_start = '-08-01')
    expect_equal(nrow(d_res), 23)
    expect_equal(sum(is.na(d_res$sat_image)), 21)
})


test_that("gdal_match_name works", {
    res_band2 <- gdal_match_name(path_modis = "/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf", band = "sr_band2")
    res_band4 <- gdal_match_name(path_modis = "/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf", band = "sr_band4")
    res_band5 <- gdal_match_name(path_modis = "/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf", band = "sr_band5")
    res_band7 <- gdal_match_name(path_modis = "/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf", band = "sr_band7")
    band2 <- "\"HDF4_EOS:EOS_GRID:\"/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf\":MODIS_Grid_16DAY_250m_500m_VI:250m 16 days blue reflectance\""
    band4 <- "\"HDF4_EOS:EOS_GRID:\"/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf\":MODIS_Grid_16DAY_250m_500m_VI:250m 16 days red reflectance\""
    band5 <- "\"HDF4_EOS:EOS_GRID:\"/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf\":MODIS_Grid_16DAY_250m_500m_VI:250m 16 days NIR reflectance\""
    band7 <- "\"HDF4_EOS:EOS_GRID:\"/home/alber/MOD13Q1/2011/MOD13Q1.A2011353.h13v09.006.2015230024001.hdf\":MODIS_Grid_16DAY_250m_500m_VI:250m 16 days MIR reflectance\""
    expect_match(res_band2, band2)
    expect_match(res_band4, band4)
    expect_match(res_band5, band5)
    expect_match(res_band7, band7)
})


test_that("get_tile_neighbors works", {
    h13v14 <- c("h12v13", "h13v13", "h14v13", "h12v14", "h14v14", "h12v15",
                "h13v15", "h14v15")
    l225063 <- c("224062", "225062", "226062", "224063", "226063", "224064",
                 "225064", "226064")
    res_h13v14 <- get_tile_neighbors("h13v14")
    res_l225063 <- get_tile_neighbors("225063")
    expect_equal(res_h13v14, h13v14)
    expect_equal(res_l225063, l225063)
})


test_that("match_prodes_year works", {
    expect_equal(match_prodes_year(adate = as.Date("2010-11-14"), prodes_start = "-08-01"), 2011)
    expect_equal(match_prodes_year(adate = as.Date("2010-01-01"), prodes_start = "-08-01"), 2010)
    expect_equal(match_prodes_year(adate = as.Date("2009-12-31"), prodes_start = "-08-01"), 2010)
})


test_that("max_hole works", {
    x <- tibble::tibble(sat_image = paste("IMG", 1:23, sep = "_"),
                        img_date = seq(from = as.Date("2017-08-01"), by = 16, length.out = 23))
    s <- sample(1:23, 1)
    x$sat_image[s] <- NA
    expect_equal(max_hole(x), 1)
})
