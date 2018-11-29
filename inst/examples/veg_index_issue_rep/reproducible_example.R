# Reproducble example - gdal_calc.py

# 1 - get test tifs
# gdal_translate -srcwin 4000 4000 5 5 brick_LC08MOD_233067_2017_sr_band5.tif file1.tif
# gdal_translate -srcwin 4000 4000 5 5 brick_LC08MOD_233067_2017_sr_band4.tif file2.tif

# 2 - compute NDVI
system('gdal_calc.py -A file1.tif -B file2.tif --calc="((A - B) / (A + B +0.00001)) * 10000" --outfile=result.tif --allBands=A --allBands=B')

library(tidyverse)

# get pixel values from tif
get_ts <- function(brick_path, pix_x, pix_y){
    cmd <- paste("gdallocationinfo", brick_path, pix_x, pix_y)
    system(cmd, intern = TRUE) %>%
        .[gtools::even(seq_along(.))] %>% .[2:length(.)] %>% strsplit(" ") %>%
        lapply(dplyr::last) %>% unlist() %>% as.numeric() %>% return()
}

imgs <- c(A = "file1.tif", B = "file2.tif", results_gdal_calc = "result.tif")

veg_ind <- lapply(imgs, get_ts, pix_x = 0, pix_y = 0) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
        results_R = ((A - B) / (A + B + 0.00001) * 10000),
    ) %>% dplyr::select(A, B, results_R, results_gdal_calc)
veg_ind %>% as.data.frame()
