# Test the vegetation index values

setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::load_all()


library(purrr)


# configuration ---
brick_type <- "interpolated"
#brick_type <- "starfm"

# number of pixels to sample
n_pix <- 20


# get brick metadata
if (brick_type == "starfm") {
    brick_path <- "/home/alber/shared/brick"
    brick_pattern <- "^LC8SR-MOD13Q1-STARFM_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_ndvi_STACK_BRICK.tif"
}else if (brick_type == "interpolated") {
    brick_path <- "/home/alber/shared/brick_interp"
    brick_pattern <- "^LC8SR-MOD13Q1-MYD13Q1_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_ndvi_STACK_BRICK.tif"
}else{
    stop("Unknown brick")
}
brick_tb <- brick_path %>%
    list.files(pattern = brick_pattern, full.names = TRUE) %>%
    dplyr::as_tibble() %>% dplyr::rename(ndvi = value) %>%
    dplyr::mutate(red = stringr::str_replace(ndvi, "_ndvi_", "_red_"),
                  nir = stringr::str_replace(ndvi, "_ndvi_", "_nir_")) %>%
    dplyr::select(nir, red, ndvi)


# get random pixels' positions
sample_pixs <- list(pix_x = sample.int(7000, n_pix),
                    pix_y = sample.int(7000, n_pix)) %>% dplyr::as_tibble()
pnames <- sample_pixs %>%
    dplyr::mutate(p_xy = stringr::str_c(pix_x, pix_y, sep = "_")) %>%
    dplyr::pull(p_xy)


# get time sereis for random pixels
brick_tb$ts <- purrr::pmap(as.list(brick_tb), function(nir, red, ndvi){
    brick_paths <- list(red = red, nir =  nir, ndvi = ndvi)
    l <- purrr::pmap(as.list(sample_pixs), function(pix_x, pix_y){
        parallel::mclapply(brick_paths, get_brick_ts, pix_x = pix_x,
                           pix_y = pix_y, mc.cores = parallel::detectCores()) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(c_ndvi = (nir - red)/(nir + red + 0.0001) * 10000,
                          diff = ndvi - c_ndvi)
    })
    names(l) <- pnames
    return(l)
})


# Are the differences between the brick and computed NDVI less than one
brick_tb$valid <- sapply(brick_tb$ts, function(ts_ls){
    res <- sapply(ts_ls, function(ts_tb){
        ts_tb %>% dplyr::pull(diff) %>%
            vapply(function(x) abs(x) < 1, logical(1)) %>% all()
    })
    if (any(is.na(res))) {
        warning("NAs found in ", names(res[is.na(res)]))
        res <- res[!is.na(res)]
    }
    return(all(res))
})
print(brick_tb)

# where are those diefferences
invalid <- brick_tb %>% dplyr::filter(valid == FALSE) %>%
    ensurer::ensure_that(nrow(.) > 0) %>%
    tidyr::unnest() %>%
    dplyr::select(-valid) %>%
    dplyr::mutate(valid = vapply(ts, function(x){all(x$diff < 1)}, logical(1))) %>%
    dplyr::filter(!valid)
for (i in 1:nrow(invalid))
    invalid %>% dplyr::slice(i) %>% dplyr::pull(ts) %>% dplyr::bind_rows() %>%
    dplyr::mutate(valid = abs(diff) < 1) %>% as.data.frame() %>%
    print()

