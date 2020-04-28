# Remove invalid samples of time series.
#
# @param  sits_tb A sits_tibble.
# @return A sits_tibble.
clean_ts <- function(sits_tb){
    sits_tb %>%
        sits::sits_prune() %>%
        tidyr::drop_na() %>%
        dplyr::mutate(has_na   = purrr::map_lgl(time_series, function(x){return(any(is.na(x)))}),
                      has_null = purrr::map_lgl(time_series, function(x){return(any(is.null(x)))}),
                      n_cols = purrr::map_int(time_series, ncol),
                      n_rows = purrr::map_int(time_series, nrow)) %>%
        dplyr::filter(has_na   == FALSE,
                      has_null == FALSE,
                      n_cols > 1,
                      n_rows > 0) %>%
        dplyr::select(-has_na, -has_null, -n_cols, -n_rows) %>%
        return()
}


# Compute vegetation indexes of Sentinel images.
#
# @param vrt_file A lenght-one character. Path to a VRT file of Sentinel-2 10 meter bands B02, B03, B04, and B08.
# @param out_file A lenght-one character. Path to a file.
# @param out_file A lenght-one character. Short name of a index.
# @return    A character. A temporal file.
compute_vi_sentinel <- function(vrt_file, out_file, index_name){
    # SAVI https://github.com/sentinel-hub/custom-scripts/blob/master/sentinel-2/savi/script.js
    # NDWI https://doi.org/10.1080/01431169608948714
    #
    index_name <- toupper(index_name)
    cmd <- list()
    cmd[["EVI2"]] <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 --outfile=%s --calc='(2.5 * (A - B)/(A + 24000*B + 10000.001)*10000).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["NDVI"]] <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 --outfile=%s --calc='((A - B)/(A + B + 0.001)*10000).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["NDWI"]] <- sprintf("gdal_calc.py -A %s --A_band=2 -B %s --B_band=4 --outfile=%s --calc='((A - B)/(A + B + 0.001)*10000).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["SAVI"]] <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 --outfile=%s --calc='((A - B)/(A + B + 4280.001) * (10000 + 4280)).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
# GEMI Cao et al 2009 Pinty and verstraete 1992
# n = ((2*(A*A - B*B) + 1.5*A + 0.5*B)/(A + B + 5000.0001))
# GEMI = (n*(1 - 0.25*n) - (B - 1250)/(10000.0001 - B)) * 10000
    cmd[["GEMI"]] <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s  --B_band=3 --outfile=%s --calc='((((2*(A*A - B*B) + 1.5*A + 0.5*B)/(A + B + 5000.0001))*(1 - 0.25*((2*(A*A - B*B) + 1.5*A + 0.5*B)/(A + B + 5000.0001))) - (B - 1250)/(10000.0001 - B)) * 10000).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["MTVI"]]  <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 -C %s --C_band=2 --outfile=%s --calc='(1.2 * (1.2 * (A - C) - 2.5 * (B - C))).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, vrt_file, out_file)
    cmd[["OSAVI"]] <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 --outfile=%s --calc='((1 + 0.16) * (A - B)/(A + B + 1600.0001)*10000).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["RDVI"]]  <- sprintf("gdal_calc.py -A %s --A_band=4 -B %s --B_band=3 --outfile=%s --calc='((A*A - 2*A*B + B*B)/(A + B + 0.0001)).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, out_file)
    cmd[["RDVI2"]] <- cmd[["RDVI"]]
    cmd[["PC1RGBNIR"]] <- sprintf("gdal_calc.py -A %s --A_band=1 -B %s --B_band=2 -C %s --C_band=3 -D %s --D_band=4 --outfile=%s --calc='(0.06379167*A + 0.14536839*B + 0.04085506*C + 0.98647327*D).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, vrt_file, vrt_file, out_file)
    cmd[["PC2RGBNIR"]] <- sprintf("gdal_calc.py -A %s --A_band=1 -B %s --B_band=2 -C %s --C_band=3 -D %s --D_band=4 --outfile=%s --calc='(-0.4401453*A + -0.5317655*B + -0.7105860*C +  0.1362536*D).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'", vrt_file, vrt_file, vrt_file, vrt_file, out_file)
    #
    if(index_name %in% names(cmd) == FALSE) {
        stop(sprintf("Unknown index: %s", index_name))
    }
    res <- system(cmd[[index_name]])
    invisible(out_file)
}


# Count the number of JP2 files recursively in the given directory.
#
# @param x A path to a directory.
# @return  A character.
count_jp2 <- function(x){
    x %>%
        list.files(pattern = ".jp2$", recursive = TRUE) %>%
        length() %>%
        return()
}


# Helper function for copying images from a tibble of satellite images.
#
# @param .data   A tibble.
# @param col     A variable in the given tibble.
# @param out_dir A lenght-one character. Desitination directory.
# @return out_dir (invisible).
cp_tb_files <- function(.data, col, out_dir){
    col <- rlang::enquo(col)
    .data %>%
        dplyr::pull(!!col) %>%
        file.copy(to = out_dir, recursive = TRUE, overwrite = FALSE)
    invisible(out_dir)
}


# Helper function to ensure distinct images.
#
# @param x A tibble with at least the fields landsat_date and tier..
# @return  A tibble.
distinct_landsat <- function(x){
    x %>%
        dplyr::distinct(landsat_date, tier, .keep_all = TRUE) %>%
        return()
}


# Helper function to filter a tibble of sentinel images by a band and then pile those images.
#
# @param band_name A length-one character. Name of a Sentinel2 band.
# @param img_tb    A tibble of Sentinel-2 images.
# @param out_pattern A char
# @return
filter_and_pile <- function(band_name, img_tb, out_pattern){
    band_tb <- img_tb %>%
        dplyr::filter(band == band_name) %>%
        dplyr::arrange(acquisition)
    out_file <- paste(out_pattern,
                      stringr::str_sub(band_tb$acquisition[[1]], 1, 8),
                      band_name, paste0(unique(band_tb$resolution), ".tif"),
                      sep = '_')
    band_tb %>%
        dplyr::pull(file_path) %>%
        pile_files(out_fn = out_file)
    invisible(out_file)
}


# Get the dimension of an image using gdal_info.
#
# @param in_file A character.
# @return        A integer or a list.
get_img_dimensions <- function(in_file) {
    stopifnot(is.atomic(in_file))
    if (is.na(in_file) || length(in_file) < 1)
        return(NA)
    if (length(in_file) == 1) {
        system2("gdalinfo", in_file, stdout = TRUE) %>%
        stringr::str_subset("Size is ") %>%
        stringr::str_split(" ") %>%
        unlist() %>%
        stringr::str_remove_all(pattern = ",") %>%
        utils::tail(2) %>%
        ensurer::ensure_that(length(.) > 1) %>%
        as.integer() %>%
        magrittr::set_names(c("pixels", "lines")) %>%
        return()
    } else {
        return(vapply(in_file, get_img_dimensions, integer(2)))
    }
}


# Get metadata of the Sentinel-2 bricks.
#
# @param in_dir          A length-one character. Path to a directory.
# @param n_expected_band A length-one integer. The number of bands and vegetation indixes in the brick.
# @return                A tibble.
get_brick <- function(in_dir, n_expected_bands = 12){
    in_dir %>%
        get_brick_md() %>%
        dplyr::arrange(tile, img_date, band) %>%
        ensurer::ensure_that(nrow(.) == n_expected_bands,
                             err_desc = sprintf("Unknown bands or indexes: %s",
                                                 in_dir)) %>%
        ensurer::ensure_that(length(unique(.$img_date)) == 1,
                             err_desc = sprintf("More than one date found: %s",
                                                in_dir)) %>%
        ensurer::ensure_that(length(unique(.$tile)) == 1,
                             err_desc = sprintf("More than one tile found: %s",
                                                in_dir)) %>%
        ensurer::ensure_that(!"" %in% .$band) %>%
        return()
}


# Get the metadata of the Sentinel-2 bricks in a directory.
#
# @param in_dir A length-one character. Path to a directory.
# @return       A tibble.
get_brick_md <- function(in_dir){
    file_band_names <- tibble::tribble(
        ~file_band, ~band,
        "B02",        "blue",
        "B03",        "green",
        "B04",        "red",
        "B08",        "bnir",
        "evi2",       "evi2",
        "gemi",       "gemi",
        "mtvi",       "mtvi",
        "ndvi",       "ndvi",
        "ndwi",       "ndwi",
        "osavi",      "osavi",
        "rdvi",       "rdvi",
        "savi",       "savi"
    )
    in_dir %>%
        list.files(pattern = ".[.]tif$", full.names = TRUE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
        tidyr::separate(col = file_name, into = c("mission", "level", "orbit",
                                                  "tile", "img_date", "file_band",
                                                  "band_complement",
                                                  "resolution"),
                        sep = '_', extra = "drop", fill = "right") %>%
        dplyr::mutate(resolution = ifelse(band_complement %in% c("10m", "20m", "60m"),
                                          band_complement, resolution),
                      band_complement = ifelse(band_complement %in% c("10m", "20m", "60m"),
                                               "", band_complement),
                      img_date = lubridate::as_date(stringr::str_sub(img_date,
                                                                     1, 8))) %>%
        dplyr::select(-band_complement) %>%
        dplyr::left_join(file_band_names, by = "file_band") %>%
        return()
}


# Mask a sentinel image.
#
# @param file_path  A length-one character. Path to a file.
# @param fmask_path A length-one character. Path to a mask file.
# @param out_dir    A leghth-one character. Path to a directory.
# @return           A character. Path to a file in out_dir.
mask_sentinel <- function(file_path, fmask_path, out_dir) {
    out_file <- file_path %>%
        basename() %>%
        #tools::file_path_sans_ext() %>%
        #stringr::str_c("_masked.tif") %>%
        (function(.x){ return(file.path(out_dir, .x))})
    vrt_file <- c(file_path, fmask_path) %>%
        gdalcmdline::gdal_build_vrt(out_filename = tempfile(pattern = "gdalbuildvrt_",
                                                          fileext = ".vrt"),
                                    resolution = "highest", separate = TRUE,
                                    vrtnodata = -9999)
    cmd <- sprintf("gdal_calc.py -A %s --A_band=1 -B %s --B_band=2 --outfile=%s --calc='(numpy.where(B != 4, A, -9999)).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'",
                   vrt_file, vrt_file, out_file)
    res <- system(cmd)
    return(out_file)
}


# Pile the given files usind gdal_merge.
#
# @param file_paths   A character. Path to files.
# @param out_fn       A length-one character. Path to the output file.
# @param gdal_format  A length-one character.
# @param no_data      A numeric.
# @param gdal_options A character.
# @return  A tibble.
pile_files <- function (file_paths, out_fn, gdal_format = "GTiff",
                        no_data = -9999, data_type = "Int16",
                        gdal_options = c("TILED=YES", "COMPRESS=LZW", "BIGTIFF=YES")){
    gdalcmdline::gdal_merge(input_files = file_paths, out_filename = out_fn,
                            ot = data_type, separate = TRUE, of = gdal_format,
                            creation_option = gdal_options, init = no_data,
                            a_nodata = no_data) %>%
    invisible()
}


# Helper function for piling Sentinel2 images.
#
# @param mission     A length-one character.
# @param level       A length-one character.
# @param orbit       A length-one character.
# @param tile        A length-one character
# @param pyear       A lenght-one integer. The PRODES year.
# @param sentinel_tb A tibble of Sentinel-2 images.
# @param out_dir     A length-one character. A path to a directory.
# @return  A tibble.
pile_sentinel_images <- function(mission, level, orbit, tile, pyear, sentinel_tb,
                        field_name,
                        out_dir){
    #field_name <- rlang::enquo(field_name)
    img_tb <- sentinel_tb %>%
        dplyr::filter(mission == mission, level == level, tile == tile,
                      pyear == pyear) %>%
        dplyr::select(!!field_name) %>%
        #dplyr::select(files_10m) %>%
        tidyr::unnest(!!field_name)
        # tidyr::unnest(files_10m)
    band_names <- img_tb %>%
        dplyr::pull(band) %>%
        unique() %>%
        sort()
    brick_path <- purrr::map_chr(band_names, filter_and_pile, img_tb = img_tb,
                                 out_pattern = file.path(out_dir,
                                                         stringr::str_c(mission, level, orbit, tile, sep = '_')))
    invisible(brick_path)
}


# Compute the PRODES year of the given date.
#
# @param x A lubridate's date object.
# @return  A integer.
prodes_year <- function(x){
    if(!lubridate::is.Date(x))
        return(NA)
    if(length(x) == 1){
        m <- lubridate::month(x)
        if(m < 8)
            return(as.integer(lubridate::year(x)))
        return(as.integer(lubridate::year(x) + 1))
    }else if(length(x) > 1){
        vapply(x, prodes_year, integer(1))
    }
}


# Split an image into chunks.
#
# @param in_file A character. Path to a file.
# @param x_size  An integer. Size of chunks in the x-direction.
# @param y_size  An integer. Size of chunks in the y-direction.
# @param out_dir A character. Path to a directory.
# @return        A tibble.
split_image <- function(in_file, xsize = 256, ysize = 256, out_dir = tempdir()){
    .Deprecated()
    # Chunk size = 64  bits * 36 images * 64 * 64 = 1.125 Mebibytes
    # Chunk size = 128 bits * 36 images * 64 * 64 = 2.25  Mebibytes
    # Chunk size = 256 bits * 36 images * 64 * 64 = 4.5   Mebibytes
    stopifnot(length(in_file) == 1)
    stopifnot(file.exists(in_file))
    stopifnot(dir.exists(out_dir))
    img_dim <- in_file %>%
        get_img_dimensions()
    pixels <- seq(from = 0, to = img_dim["pixels"], by = xsize)
    lines  <- seq(from = 0, to = img_dim["lines"],  by = ysize)
    chunk_tb <- expand.grid(pixels, lines) %>%
        tibble::as_tibble() %>%
        dplyr::rename(pixel_from = Var1, line_from = Var2) %>%
        dplyr::mutate(x_win = ifelse(img_dim["pixels"] < pixel_from + xsize, img_dim["pixels"] - pixel_from, xsize),
                      y_win = ifelse(img_dim["lines"]  < line_from  + ysize, img_dim["lines"]  - line_from,  ysize),
                      out_file = stringr::str_c(tools::file_path_sans_ext(basename(in_file)),
                                                pixel_from, line_from, sep = '_'),
                      out_file = paste0(file.path(out_dir, out_file), ".tif")) %>%
        dplyr::mutate(command = stringr::str_c("gdal_translate -q -of GTiff -srcwin",
                                               pixel_from, line_from,
                                               x_win, y_win,
                                               in_file, out_file,
                                               sep = ' ')) %>%
        dplyr::mutate(command_res = purrr::map_int(command, system)) %>%
        ensurer::ensure_that(all(dplyr::pull(., command_res) == 0),
                             err_desc = sprintf("Image chunking failed for file %s",
                                                in_file)) %>%
        dplyr::select(-command, -command_res) %>%
        return()
}


#------------------------------------------------------------------------------
# Helper functions.


# Helper for masking in parallel.
#
# @param id_row      A length-one integer. A row number in sentinel_tb.
# @param sentinel_tb A tibble describing Sentienl-2 images.
# @return            A character. Path to the masked image.
helper_mask <- function(id_row, sentinel_tb, out_dir = tempdir()){
    stopifnot(id_row %in% 1:nrow(sentinel_tb))
    in_files <- sentinel_tb %>%
        dplyr::slice(id_row) %>%
        dplyr::select(file_path, fmask_path) %>%
        unlist()
    in_files["file_path"] %>%
             mask_sentinel(fmask_path = in_files["fmask_path"],
                           out_dir = out_dir) %>%
             return()
}

helper_mask2 <- function(id_row, img_tb, var, out_dir = tempdir()){
    stopifnot(id_row %in% 1:nrow(sentinel_tb))
    var <- dplyr::enquo(var)
    in_files <- img_tb %>%
        dplyr::slice(id_row) %>%
        dplyr::select(!!var, fmask_path) %>%
        unlist()
    in_files[1] %>%
             mask_sentinel(fmask_path = in_files["fmask_path"],
                           out_dir = out_dir) %>%
             return()
}






# DEPRECATED
helper_pile <- function(x, out_dir){
    .Deprecated()
    out_file <- x %>%
        (function(.data){
            .data %>%
                dplyr::select(mission, level, baseline, orbit, pyear, tile, band, resolution, pixel_from, line_from) %>%
                lapply(., unique) %>%
                sapply(., length) %>%
                ensurer::ensure_that(all(. == 1),
                                     err_desc = "Invalid image tibble.")
            return(.data)
        }) %>%
        dplyr::arrange(img_date) %>%
        dplyr::slice(1) %>%
        dplyr::pull(safe_path) %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        paste(unique(x$band), unique(x$pixel_from), paste0(unique(x$line_from),
                                                           ".tif"), sep = '_')
    out_file <- file.path(out_dir, out_file)
    x %>%
        dplyr::pull(file_path) %>%
        pile_files(out_fn = out_file, gdal_format = "GTiff", no_data = -9999,
                   gdal_options = c("TILED=YES", "COPY_SRC_OVERVIEWS=YES",
                                         "COMPRESS=LZW", "BIGTIFF=YES"))
    return(out_file)
}


# Helper function for piling up Sentinel-2 images.
#
# @param .x      A tibble of 36 rows and at least the columns img_date (date), mission, level, orbit, tile, acquisition, band, resolution, file_path.
# @param out_dir A length-one character. Path to a directory.
# @return        A tibble.
helper_pile2 <- function(.x, out_dir){
    file_tb <- .x %>%
        ensurer::ensure_that(nrow(.) == 36, err_desc = "Missing images!") %>%
        dplyr::arrange(img_date)
    out_file <- file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(mission, level, orbit, tile, acquisition, band, resolution) %>%
        unlist() %>%
        paste(collapse = '_') %>%
        paste0(".tif")
    out_file <- file.path(out_dir, out_file)
    file_tb %>%
        dplyr::pull(file_path) %>%
        ensurer::ensure_that(length(.) > 0,
                             err_desc = "Missing path to images!") %>%
        pile_files(out_fn = out_file)
    file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(-safe_path, -file_path, -acquisition) %>% #, -processing
        dplyr::mutate(brick_file = out_file) %>%
        return()
}


# Helper function for piling up Sentinel-2 images.
#
# @param .x      A tibble of 36 rows and at least the columns img_date (date), mission, level, orbit, tile, acquisition, band, resolution, file_path.
# @param out_dir A length-one character. Path to a directory.
# @return        A tibble.
helper_pile_masked <- function(.x, out_dir){
    file_tb <- .x %>%
        ensurer::ensure_that(nrow(.) == 36, err_desc = "Missing images!") %>%
        dplyr::arrange(img_date)
    out_file <- file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(mission, level, orbit, tile, acquisition, band, resolution) %>%
        unlist() %>%
        paste(collapse = '_') %>%
        paste0(".tif")
    out_file <- file.path(out_dir, out_file)
    file_tb %>%
        dplyr::pull(img_masked) %>%
        pile_files(out_fn = out_file)
    file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(-safe_path, -file_path, -acquisition, -processing) %>%
        dplyr::mutate(brick_file = out_file) %>%
        return()
}


# Helper function for piling up Sentinel-2 images.
#
# @param .x      A tibble of 36 rows and at least the columns img_date (date), mission, level, orbit, tile, acquisition, band, resolution, file_path.
# @param out_dir A length-one character. Path to a directory.
# @return        A tibble.
helper_pile_raw <- function(.x, out_dir){
    .Deprecated("helper_pile2")
    file_tb <- .x %>%
        ensurer::ensure_that(nrow(.) == 36, err_desc = "Missing images!") %>%
        dplyr::arrange(img_date)
    out_file <- file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(mission, level, orbit, tile, acquisition, band, resolution) %>%
        unlist() %>%
        paste(collapse = '_') %>%
        paste0(".tif")
    out_file <- file.path(out_dir, out_file)
    file_tb %>%
        dplyr::pull(file_path) %>%
        pile_files(out_fn = out_file)
    file_tb %>%
        dplyr::slice(1) %>%
        dplyr::select(-safe_path, -file_path, -acquisition, -processing) %>%
        dplyr::mutate(brick_file = out_file) %>%
        return()
}


# Helper for computing vegetation indexes.
#
# @param vrt_file A lenght-one character. Path to a VRT file of a Sentinel-2 image.
# @return         A tibble.
helper_vi <- function(vrt_file){
    vi_names <- c("evi2", "gemi", "mtvi", "ndvi", "ndwi", "osavi", "rdvi", "savi", "pc1rgbnir", "pc2rgbnir")
    out_files <- vi_names %>%
        paste0('_') %>%
        lapply(tempfile, fileext = ".tif") %>%
        magrittr::set_names(vi_names)
    for(vi in vi_names) {
        compute_vi_sentinel(vrt_file, out_file = out_files[[vi]],
                            index_name = vi)
    }
    out_files %>%
        tibble::as_tibble() %>%
        return()
}


# Helper for bulding a VRT for ease vegetation index computation of Sentienl-2 images.
#
# @param B02 A lenght-one character. Path to a file of Sentinel-2 band
# @param B03 A lenght-one character. Path to a file of Sentinel-2 band.
# @param B04 A lenght-one character. Path to a file of Sentinel-2 band.
# @param B08 A lenght-one character. Path to a file of Sentinel-2 band.
# @return    A character. Path to a VRT file.
helper_vrt_vi <- function(B02, B03, B04, B08){
    c(B02, B03, B04, B08) %>%
        gdalcmdline::gdal_build_vrt(out_filename = tempfile(pattern = "sentinel_b2348_",
                                                            fileext = ".vrt"),
                                    resolution = "highest", separate = TRUE,
                                    vrtnodata = -9999) %>%
        return()
}

