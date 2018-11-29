# Compare the values stored in the vegetation indexes to those computed in R

library(tidyverse)

get_ts <- function(brick_path, pix_x, pix_y){
    cmd <- paste("gdallocationinfo", brick_path, pix_x, pix_y)
    system(cmd, intern = TRUE) %>%
        .[gtools::even(seq_along(.))] %>% .[2:length(.)] %>% strsplit(" ") %>%
        lapply(dplyr::last) %>% unlist() %>% as.numeric() %>% return()
}

pix_x <- 4000
pix_y <- 4000
imgs <- c(
    b5   = "/home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band5.tif",
    b4   = "/home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band4.tif",
    ndvi = "/home/alber/shared/brick/brick_LC08MOD_225063_2014_ndvi.tif",
    savi = "/home/alber/shared/brick/brick_LC08MOD_225063_2014_savi.tif")

stopifnot(all(file.exists(imgs)))

veg_ind <- lapply(imgs, get_ts, pix_x = pix_x, pix_y = pix_y) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
        ndvi_comp = ((b5 - b4) / (b5 + b4 + 0.00001)*10000),
        savi_comp = ((b5 - b4) / (b5 + b4 + 5000.00001)) * 1.5 * 10000
    ) %>% dplyr::select(b5, b4, ndvi, ndvi_comp, savi, savi_comp)
veg_ind %>% as.data.frame()


#gdal_calc.py -A /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band5.tif -B /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band4.tif --calc="((A.astype(float64) - B.astype(float64)) / (A.astype(float64) + B.astype(float64)) + 0.00001) * 10000" --outfile=deleteme.tif --overwrite --allBands=A
#gdallocationinfo deleteme.tif 4000 4000


# 8567.35338

# python
# import numpy
# b5 = numpy.array([2929], dtype = 'i2')
# b4 = numpy.array([226], dtype = 'i2')
# ((b5.astype(numpy.float64) - b4.astype(numpy.float64)) / (b5.astype(numpy.float64) + b4.astype(numpy.float64)) + 0.00001) * 10000
# array([8567.45340729])
