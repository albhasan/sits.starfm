# Test the vegetation index values
library(purrr)
library(devtools)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()

# configuration ---
#brick_type <- "interpolated"
#brick_type <- "starfm"
#brick_type <- "simple"
brick_type <- "hlsl30_raw"

# number of pixels to sample
n_pix <- 20

# get brick metadata
if (brick_type == "starfm") {
    brick_path <- "/home/alber/shared/brick"
    brick_pattern <- "^LC8SR-MOD13Q1-STARFM_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_ndvi_STACK_BRICK.tif"
    band_names <- c(nir = "nir", red = "red")
}else if (brick_type == "interpolated") {
    brick_path <- "/home/alber/shared/brick_interp"
    brick_pattern <- "^LC8SR-MOD13Q1-MYD13Q1_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_ndvi_STACK_BRICK.tif"
    band_names <- c(nir = "nir", red = "red")
}else if (brick_type == "simple") {
    brick_path <- "/home/alber/shared/brick_simple"
    brick_pattern <- "^LC8SR-SIMPLE_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_ndvi_STACK_BRICK.tif"
    band_names <- c(nir = "nir", red = "red")
}else if (brick_type == "hlsl30_raw") {
    brick_path <- "/home/alber/shared/brick_hls_raw"
    brick_pattern <- "^HLSL30-RAW_[A-Z][0-9]{2}[A-Z]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_(evi|evi2|msavi|nbr|nbr2|ndmi|ndvi|savi)_STACK_BRICK.tif"
    band_names <- c(blue = "band02", nir = "band05", red = "band04", swir = "band06", swir2 = "band07")
}else{
    stop("Unknown brick")
}

# get random pixels' positions
sample_pixs <- list(pix_x = sample.int(7000, n_pix),
                    pix_y = sample.int(7000, n_pix)) %>% 
    dplyr::as_tibble()
pix_names <- sample_pixs %>%
    dplyr::mutate(p_xy = stringr::str_c(pix_x, pix_y, sep = "_")) %>%
    dplyr::pull(p_xy)

brick_tb <- brick_path %>%
    list.files(pattern = brick_pattern, full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "No files found.") %>%
    dplyr::rename(brick_file = value) %>%
    dplyr::mutate(band = brick_file %>% 
                      basename() %>% 
                      stringr::str_extract(pattern = "_[a-z]+[0-9]?_") %>% 
                      stringr::str_sub(2, -2)) %>%
    tidyr::spread(key =  "band", value = "brick_file") %>%
    dplyr::mutate(blue  = stringr::str_replace(ndvi, "_ndvi_", paste0('_', band_names["blue"], '_')),
                  nir   = stringr::str_replace(ndvi, "_ndvi_", paste0('_', band_names["nir"], '_')),
                  red   = stringr::str_replace(ndvi, "_ndvi_", paste0('_', band_names["red"], '_')),
                  swir  = stringr::str_replace(ndvi, "_ndvi_", paste0('_', band_names["swir"], '_')),
                  swir2 = stringr::str_replace(ndvi, "_ndvi_", paste0('_', band_names["swir2"], '_'))) %>%
    dplyr::select(blue, nir, red, swir, swir2, evi, evi2, msavi, nbr, nbr2, ndmi, ndvi, savi) %>%
    dplyr::mutate(ts = purrr::pmap(., function(blue, nir, red, swir, swir2, evi, 
                                               evi2, msavi, nbr, nbr2, ndmi, 
                                               ndvi, savi, sample_pixs, pix_names){
        brick_paths <- list(blue = blue, nir =  nir, red = red, swir = swir, 
                            swir2 = swir2, evi = evi, evi2 = evi2, msavi = msavi, 
                            nbr = nbr, nbr2 = nbr2, ndmi = ndmi, ndvi = ndvi, savi = savi)
        l <- purrr::pmap(as.list(sample_pixs), function(pix_x, pix_y){
            parallel::mclapply(brick_paths, get_brick_ts, pix_x = pix_x,
                               pix_y = pix_y, mc.cores = parallel::detectCores()) %>%
                dplyr::as_tibble() %>%
                dplyr::mutate(BLUE  =  blue/10000,
                              RED   =   red/10000,
                              NIR   =   nir/10000,
                              SWIR  =  swir/10000,
                              SWIR2 = swir2/10000,
                              c_evi   = 2.5 * ((NIR - RED)/(NIR + 6 * RED - 7.5 * BLUE + 1.001)) * 10000,
                              c_evi2  = 2.5 * ((NIR - RED)/(NIR + 2.4 * RED + 1.001)) * 10000, 
                              c_msavi = (2 * NIR + 1 - sqrt((2 * NIR + 1)^2 - 8 * (NIR - RED)))/2.001 * 10000,
                              c_nbr   = (NIR - SWIR2)/(NIR + SWIR2 + 0.001) * 10000,
                              c_nbr2  = (SWIR - SWIR2)/(SWIR + SWIR2 + 0.001) * 10000,
                              c_ndmi  = (NIR - SWIR) /(NIR + SWIR + 0.001) * 10000, # ndwi
                              c_ndvi  = (NIR - RED)/(NIR + RED + 0.001) * 10000,
                              c_savi  = ((NIR - RED)/(NIR + RED + 0.5)) * 1.5 * 10000,
                              diff_evi2 = evi2 - c_evi2,
                              diff_ndvi = ndvi - c_ndvi, 
                              diff_savi = savi - c_savi)
            }) 
        names(l) <- pix_names
        purrr::map2(l, pix_names, function(tb, pname){
            tb %>% 
                dplyr::mutate(pix_name = pname) %>%
                return()
        }) %>%
            dplyr::bind_rows() %>%
            tidyr::drop_na() %>%
            dplyr::filter(nir  != -9999,
                          red  != -9999,
                          swir != -9999) %>%
#            dplyr::filter(abs(diff_evi2) > 1,
#                          abs(diff_ndvi) > 1,
#                          abs(diffndmi) > 1,
#                          abs(diff_savi) > 1) %>%
            return()
        }, 
        sample_pixs = sample_pixs, pix_names = pix_names)) 



# Are the differences between the brick and computed NDVI less than one?
brick_tb$valid <- sapply(brick_tb$ts, function(ts_ls){
    res <- sapply(ts_ls, function(ts_tb){
        ts_tb %>% dplyr::pull(diff_ndvi) %>%
            vapply(function(x) abs(x) < 1, logical(1)) %>% all()
    })
    if (any(is.na(res))) {
        warning("NAs found in ", names(res[is.na(res)]))
        res <- res[!is.na(res)]
    }
    return(all(res))
})
print(brick_tb)

# where are those differences?
invalid <- brick_tb %>% dplyr::filter(valid == FALSE) %>%
    ensurer::ensure_that(nrow(.) > 0) %>%
    tidyr::unnest() %>%
    dplyr::select(-valid) %>%
    dplyr::mutate(valid = vapply(ts, function(x){all(x$diff_ndvi < 1)}, logical(1))) %>%
    dplyr::filter(!valid)

if(nrow(invalid) > 0){
    for (i in 1:nrow(invalid))
        invalid %>% dplyr::slice(i) %>% dplyr::pull(ts) %>% dplyr::bind_rows() %>%
        dplyr::mutate(valid = abs(diff_ndvi) < 1) %>% as.data.frame() %>%
        print()
}else{
    message("No invalid values found!")
}

