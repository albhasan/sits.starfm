#' @title Build a table from the given images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table from the given images.
#'
#' @param img_path A charater. Path to Landsat 8 images.
#' @param pattern  A length-one character. Pattern used to filter the files.
#' @param from     A length-one character. The start date.
#' @param to       A length-one character. The end date.
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
build_landsat_tibble <- function(img_path, pattern = NULL, from = NULL,
                                 to = NULL, prodes_year_start = "-08-01") {
    l8_files <- img_path %>% list.files(pattern = pattern, full.names = TRUE,
                                        recursive = TRUE) %>%
        ensurer::ensure_that(length(.) > 0,
                             err_desc = sprintf("No files found in %s", img_path))
    l8_img <- l8_files %>% stringr::str_subset(pattern = ".*[.]tif$") %>%
        tibble::enframe(name = NULL) %>% dplyr::rename(file_path = value) %>%
        dplyr::mutate(sat_image = substr(basename(file_path), 1, 40)) %>%
        dplyr::select(sat_image, file_path) %>%
        tidyr::nest(file_path, .key = "files") %>%
        dplyr::mutate(tile = substr(sat_image, 11, 16),
                      img_date  = lubridate::ymd(substr(sat_image, 18, 25)),
                      proc_date = lubridate::ymd(substr(sat_image, 27, 34)),
                      prodes_year = match_prodes_year(
                          img_date, prodes_start = prodes_year_start)) %>%
        dplyr::distinct(tile, img_date, .keep_all = TRUE) %>%
        dplyr::arrange(tile, img_date)
    if (!all(is.null(from), is.null(to))) {
        l8_img <- l8_img %>% dplyr::filter(img_date >= lubridate::as_date(from),
                                           img_date <= lubridate::as_date(to))
    }
    l8_img$neigh <- purrr::map(l8_img$tile, get_tile_neighbors)
    l8_img$neigh <- purrr::map2(l8_img$neigh, l8_img$img_date, match_tile_date,
                                img_tb = l8_img, untie = 0.001)
    # get the cloud coverage
    l8_cloud_cov <- l8_files %>%
        stringr::str_subset(pattern = ".*_MTL\\.txt$") %>%
        dplyr::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(sat_image = substr(basename(file_path), 1, 40))
    l8_cloud_cov$cloud_cov <-
        purrr::map_dbl(l8_cloud_cov$file_path, function(x) {
            x %>% readLines() %>% stringr::str_subset("CLOUD_COVER_LAND") %>%
                stringr::str_split(" = ") %>% unlist() %>% dplyr::last() %>%
                as.numeric() * 1/100
        })
    l8_cloud_cov <- l8_cloud_cov %>% dplyr::select(sat_image, cloud_cov)
    l8_img <- l8_img %>% dplyr::left_join(l8_cloud_cov, by = "sat_image")
    return(l8_img)
}


#' @title Build a table from the given images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table from the given images.
#'
#' @param img_path A charater. Paths to MOD13Q1 images.
#' @param pattern  A length-one character. Pattern used to filter the files.
#' @param from     A length-one character. The start date.
#' @param to       A length-one character. The end date.
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
build_modis_tibble <- function(img_path, pattern, from = NULL, to = NULL,
                               prodes_year_start = "-08-01") {
    mod_img <- img_path %>%
        list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
        dplyr::as_tibble() %>% dplyr::rename(file_path = value) %>%
        dplyr::mutate(sat_image = basename(file_path)) %>%
        dplyr::select(sat_image, file_path) %>%
        dplyr::mutate(
            tile = substr(sat_image, 18, 23),
            img_date = as.Date(lubridate::parse_date_time(
                substr(sat_image, 10, 16), orders = "%Y%j")),
            prodes_year = match_prodes_year(
                img_date,
                prodes_start = prodes_year_start)
        ) %>%
        dplyr::distinct(tile, img_date, .keep_all = TRUE) %>%
        dplyr::arrange(tile, img_date)

    if (!all(is.null(from), is.null(to))) {
        mod_img <- mod_img %>% dplyr::filter(img_date >= lubridate::as_date(from),
                                             img_date <= lubridate::as_date(to))
    }

    # get the cloud coverage. TODO: Too slow. Find a faster way!
    mod_img$cloud_cov <- parallel::mclapply(mod_img$file_path, function(x) {
        sysres <- suppressWarnings(system(paste("gdalinfo", x), intern = TRUE))
        if (length(sysres) == 0)
            if (attr(sysres, "status") == 1) {
                warning(sprintf("Invalid file %s", x))
                return(NA)
            }
        sysres %>% stringr::str_subset("QAPERCENTCLOUDCOVER") %>%
            dplyr::first() %>% stringr::str_split("=") %>% unlist() %>%
            dplyr::last() %>% as.numeric() * 1/100 %>% return()
    }, mc.cores = parallel::detectCores()) %>% unlist()
    return(mod_img)
}


#' @title Get the Landsat band name from the file path
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the band name from a landsat file
#'
#' @param path      A character. Path to a Landsat file.
#' @param band_name A character. The type of name to retrieve. c("band_designation", "short_name")
#' @return A character.
#' @export
get_landsat_band <- function(path, band_name = "band_designation") {
    stopifnot(band_name %in% c("band_designation", "short_name"))
    if (length(path) == 1) {
        if (is.na(path)) return(NA)
        bnames <- SPECS_L8_SR %>% dplyr::pull(!!band_name)
        res <- path %>% basename() %>%
            stringr::str_extract(stringr::fixed(bnames, ignore_case = TRUE))
        if (all(is.na(res)))
            return(NA)
        res %>% .[!is.na(.)] %>% dplyr::first() %>% return()
    } else {
        res <- sapply(path, get_landsat_band, band_name = band_name)
        names(res) <- NULL
        return(res)
    }
}


#' @title Get the metadata required to call gdal's utilitaries.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the metadata required to call gdal's utilitaries.
#'
#' @param img_path     A length-one character. Path to a file.
#' @return             A list of named elements.
get_landsat_metadata <- function(img_path) {
    img_raster <- img_path %>% raster::raster()
    img_extent <- img_raster %>% raster::extent()
    #
    param_band <- img_path %>% get_landsat_band()
    param_crs <- paste0("'", raster::projection(img_raster), "'")
    param_extent_output <- c(img_extent@xmin, img_extent@ymin, img_extent@xmax,
                             img_extent@ymax)
    param_ncol <- ncol(img_raster)
    param_nrow <- nrow(img_raster)
    param_img_size <- c(param_ncol, param_nrow)
    param_pixel_size_x <- raster::xres(img_raster)
    param_pixel_size_y <- raster::yres(img_raster)
    param_srcnodata_l8 <- SPECS_L8_SR %>%
        dplyr::filter(band_designation == param_band) %>%
        dplyr::pull(fill_value)
    param_srcnodata_mod <- SPECS_MOD13Q1 %>%
        dplyr::filter(l8_sr_designation == param_band) %>%
        dplyr::pull(fill_value)
    param_data_range_l8 <- SPECS_L8_SR %>%
        dplyr::filter(band_designation == param_band) %>%
        dplyr::pull(valid_range) %>% unlist()
    param_data_range_mod <- SPECS_MOD13Q1 %>%
        dplyr::filter(l8_sr_designation == param_band) %>%
        dplyr::pull(valid_range) %>% unlist()
    param_scale_factor <- SPECS_L8_SR %>%
        dplyr::filter(band_designation == param_band) %>%
        dplyr::pull(scale_factor) %>% unlist() %>% solve() %>% as.vector()

    return(list(band = param_band, crs = param_crs,
                extent_output = param_extent_output,
                ncol = param_ncol, nrow = param_nrow, img_size = param_img_size,
                pixel_size_x = param_pixel_size_x,
                pixel_size_y = param_pixel_size_y,
                srcnodata_l8 = param_srcnodata_l8,
                srcnodata_mod = param_srcnodata_mod,
                data_range_l8 = param_data_range_l8,
                data_range_mod = param_data_range_mod,
                scale_factor = param_scale_factor))
}


#' @title Find the best-next image for a fusion model.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Find the next best image from the given reference row in the
#' brikcs' images. In case of no match, the next image with the fewer clouds is
#' returned.
#'
#' @param brick_imgs     A tibble of the images in a brick, including the
#' variables img_date, sat_image, and cloud_cov.
#' @param ref_row_number  A length-one numeric. The index of the reference row
#' in brick_imgs.
#' @param cloud_threshold A length-one numeric. The threshold for matching such
#' that the product of the cloud coverages of the reference and the match are
#' less than this threshold.
#' @return A tibble. The row in brick_imgs that best match the ref_row
#' @export
get_next_image <- function(brick_imgs, ref_row_number, cloud_threshold = 0.1) {
    # get the image with the fewer clouds AFTER ref_row_number
    few_clouds <- NA
    if (ref_row_number < nrow(brick_imgs))
        few_clouds <- brick_imgs %>% dplyr::slice(ref_row_number:nrow(.)) %>%
            dplyr::filter(cloud_cov == min(cloud_cov, na.rm = TRUE)) %>%
            dplyr::filter(dplyr::row_number() == 1)
    # get the reference data
    ref_row <- brick_imgs %>% dplyr::slice(ref_row_number)
    if (is.na(ref_row$cloud_cov)) {
        warning(sprintf("Missing cloud coverage for image %s",
                        ref_row$sat_image))
        ref_row$cloud_cov <- 1e+06
    }
    # find the next-best image regarding clouds coverage
    for (j in (ref_row_number + 1):nrow(brick_imgs)) {
        if (j >= nrow(brick_imgs))
            next
        test_row <- brick_imgs %>% dplyr::slice(j)
        if (is.na(test_row$sat_image) || is.na(test_row$cloud_cov))
            next
        if (ref_row$cloud_cov * test_row$cloud_cov <= cloud_threshold)
            return(test_row)
    }
    msg <- sprintf("No image satisfies the cloud threshold (%s)!",
                   cloud_threshold)
    if (length(few_clouds) > 1 && !is.na(few_clouds))
        msg <- msg %>%
            stringr::str_c(" Found instead the image with the fewest clouds.")
    warning(msg)
    return(few_clouds)
}


#' @title Get the 8 neighbors of a Landsat or MODIS tile.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the 8 neightbors of a Landsat or MODIS tile.
#'
#' @param tile  A character. The tile.
#' @return A character or a character matrix.
#' @export
get_tile_neighbors <- function(tile) {
    stopifnot(is.character(tile))
    tile <- tile[!is.na(tile)]
    stopifnot(nchar(tile) == 6)
    window_one <- function(x) {
        x %>% as.integer() %>% (function(x) {
            x + -1:1
        })
    }
    if (length(tile) == 1) {
        if (stringr::str_detect(tile, "^(h|H)[0-9]{2}(v|V)[0-9]{2}$")) {
            # modis
            i_path <- tile %>% stringr::str_sub(2, 3) %>% window_one() %>%
                stringr::str_pad(width = 2, pad = "0")
            i_row <- tile %>% stringr::str_sub(5, 6) %>% window_one() %>%
                stringr::str_pad(width = 2, pad = "0")
            i_path <- paste0("h", i_path)
            i_row <- paste0("v", i_row)
        } else if (stringr::str_detect(tile, "^[0-9]{6}")) {
            # landsat
            i_path <- tile %>% stringr::str_sub(1, 3) %>% window_one() %>%
                stringr::str_pad(width = 3, pad = "0")
            i_row <- tile %>% stringr::str_sub(4, 6) %>% window_one() %>%
                stringr::str_pad(width = 3, pad = "0")
        } else {
            return(NA)
        }
        expand.grid(i_path, i_row) %>%
            apply(MARGIN = 1, FUN = paste0, collapse = "") %>%
            setdiff(tile) %>%
            return()
    } else {
        return(c(sapply(tile, get_tile_neighbors)))
    }
}


#' @title Match spacially MODIS tiles to Landsat Scenes.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Match spacially MODIS tiles to Landsat Scenes.
#'
#' @param scene_path A length-one character. Path to a shapefile of Landsat
#' scenes (Worldwide Reference System).
#' @param tile_path  A length-one character. Path to a shapefile of MODIS tiles.
#' @param scenes     A character. A subset of scenes to be matched.
#' @param tiles      A character. A subset of tiles to match.
#' @return           A tibble.
match_tiles2scenes <- function(scene_path, tile_path, scenes = NULL,
                               tiles = NULL) {
    l8_shp <- scene_path %>% sf::st_read(quiet = TRUE) %>%
        dplyr::mutate(
            scene = stringr::str_c(stringr::str_pad(PATH, 3, pad = "0"),
                                   stringr::str_pad(ROW, 3, pad = "0")))
    mod_shp <- tile_path %>% sf::st_read(quiet = TRUE) %>%
        dplyr::mutate(
            tile = stringr::str_c("h", stringr::str_pad(h, 2, pad = "0"),
                                  "v", stringr::str_pad(v, 2, pad = "0"))) %>%
        sf::st_transform(crs = sf::st_crs(l8_shp)$proj4string)
    if (!is.null(scenes))
        l8_shp <- l8_shp %>% dplyr::filter(scene %in% scenes)
    if (!is.null(tiles))
        mod_shp <- mod_shp %>% dplyr::filter(tile %in% tiles)
    sf::st_intersection(l8_shp, mod_shp) %>% sf::st_set_geometry(NULL) %>%
        dplyr::as_tibble() %>% dplyr::select(scene, tile) %>%
        tidyr::nest(tile, .key = "tile") %>%
        return()
}


#' @title Get metadata from images' file names.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get metadata from images' file names.
#'
#' @param file_path A character. Paths to image files.
#' @return          A list of character.
#' @export
parse_img_name <- function(file_path){
    if(is.na(file_path) || !is.atomic(file_path) || length(file_path) < 1)
        return(NA)
    if(length(file_path) == 1){
        file_name <- basename(file_path)
        if(stringr::str_detect(file_name, pattern = "^L[CTM]0[4-9]_L[0-3][A-Z]{2}_[0-9]{6}_[0-9]{8}_[0-9]{8}_[0-9]{2}_[A-Z]([0-9]|[A-Z])")){
            # Landsat collection 1
            res <- unlist(stringr::str_split(file_name, "_"))
            names(res) <- c("header", "level", "path_row", "acquisition", "processing", "collection", "category")
            return(res)
        }else{
            return(NA)
        }
    }else{
        res <- lapply(file_path, parse_img_name)
        names(res) <- basename(file_path)
        return(res)
    }
}


