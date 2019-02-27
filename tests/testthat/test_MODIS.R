context("MODIS related functions")
library(sits.starfm)

path_modis <- "/home/alber/MOD13Q1"
check_api <- function() {
    if (!dir.exists(path_modis)) {
        skip("MOD13Q1 not available")
    }
}
