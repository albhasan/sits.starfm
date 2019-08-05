#' @title Add missing dates to a year of observations (a time-series tibble)
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Add the missing dates to a time-series tibble. This function
#' computes and joins the dates of a time series from the minimum to the maximum
#' dates found in the input tibble.
#'
#' @param x            A tibble with a the fields 'img_date' and 'sat_image'.
#' That is the date when the image was taken and its matching image.
#' @param step         A length-one numeric. The number of days between
#' observations.
#' @param prodes_start A length-one character. The first day of the PRODES year
#' (e.g. '-08-01').
#' @return             A tibble of the matching PRODES dates and NA for the
#' missing 'sat_image'.
add_missing_dates <- function(x, step, prodes_start) {
    date_vec <- x %>% dplyr::pull(img_date)
    img_type <- x %>% dplyr::pull(sat_image) %>% dplyr::first() %>%
        stringr::str_sub(1, 2)
    d_min <- min(date_vec)
    obs_per_year <- round(365/step, digits = 0)
    start_month <- as.numeric(substr(prodes_start, 2, 3))
    # First day of PRODES for x's year
    if (lubridate::month(d_min) >= start_month) {
        start_year <- lubridate::year(d_min)
    } else {
        start_year <- lubridate::year(d_min) - 1
    }
    prodes_date <- start_year %>% as.character() %>% paste0(prodes_start) %>%
        lubridate::ymd()
    # build the dates of a theoretical time series
    y1 <- start_year %>% as.character() %>% paste0("-01-01") %>%
        lubridate::ymd() %>% seq(by = step, length.out = obs_per_year)
    y2 <- (start_year + 1) %>% as.character() %>% paste0("-01-01") %>%
        lubridate::ymd() %>% seq(by = step, length.out = obs_per_year)
    ts_exp <- c(y1, y2)
    if (toupper(img_type) == "LC") {
        ts_exp <- c(seq(d_min, by = step, length.out = obs_per_year),
                    seq(d_min, by = -step, length.out = obs_per_year)) %>%
            unique() %>% sort()
    }
    # Image's dates for the PRODES year, matching the dates in x
    ts_exp <- ts_exp %>% subset(. >= prodes_date) %>%
        subset(1:length(.) <= obs_per_year)
    # Join PRODES dates to x. Missing sat_image become NAs
    return(ts_exp %>% dplyr::tibble(img_date = .) %>%
               dplyr::full_join(x, by = "img_date"))
}


#' @title Call the operating system.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Call the operating system.
#'
#' @param command A length-one character. The command to call.
#' @param args    A character. The command's parameters.
#' @param stdout  A length-one character.
#' @param stderr  A length-one character.
#' @param dry_run A length-one logical. The default is FALSE
#' @return A character
call_os <- function(command, args, stdout = "", stderr = "", dry_run = FALSE) {
    if (dry_run) {
        print(paste(c(command, args), collapse = " "))
        return(0)
    }
    tryCatch({
        system2(command = command, args = args, stdout = stdout, stderr = stderr)
    }, warning = function(msg) {
        #print("WARNING: R is a complete piece of shit")
        paste(command, collapse = " ")
        paste(args, collapse = " ")
        paste(msg, collapse = " ")
    }, error = function(msg) {
        #print("ERROR: R sucks and is the shitties languge ever")
        paste(command, collapse = " ")
        paste(args, collapse = " ")
        paste(msg, collapse = " ")
    #}, finally={
    #    print("FINALLY: R is a complete piece of shit")
    })
}


#' @title Get a mask for the borders of a Lansat image.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get a raster with 0 where the borders of the given Landsat image occur and 1 everywhere else.
#'
#' @param in_file  A character. Path to a file.
#' @param out_file A character. Path to a file.
#' @param no_data  A numeric. The value for no data. The default is 0.
#' @return         A character (identical to out_file).
get_landsat_corner_mask <- function(in_file, out_file, no_data = 0){
    stopifnot(length(in_file) > 0)
    stopifnot(length(in_file) == length(out_file))
    stopifnot(all(vapply(c(in_file, out_file), is.character, logical(1))))
    stopifnot(all(vapply(in_file, file.exists, logical(1))))

    if (length(in_file) == 1) {
        fill_value <- SPECS_L8_SR %>%
            dplyr::filter(band_designation == get_landsat_band(in_file)) %>%
            ensurer::ensure_that(nrow(.) == 1, err_desc = "Unknown Landsat image!") %>%
            dplyr::pull(fill_value)
        in_file %>% gdal_calc(
            out_filename = out_file,
            expression = paste0("(numpy.where(A == ", fill_value, ", 0, 1)).astype(int16)"),
            dstnodata = no_data,
            out_format = "GTiff") %>%
            return()
    }else{
        vapply(seq_along(in_file), function(i){
            get_landsat_corner_mask(in_file[i], out_file[i])
        }, character(1)) %>%
            return()
    }
}


#' @title Get the PRODES year of the given date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the PRODES year of the given date. The PRODES year starts
#' each year on August the 20th and it is identified by the largest year on its
#' interval.
#'
#' @param adate        A length-one date. The date of reference
#' @param prodes_start A length-one character. The first day of the PRODES year
#' (e.g. '-08-01')
#' @return             A numeric. The year
match_prodes_year <- function(adate, prodes_start) {
    year_vec <- seq(2000, lubridate::year(lubridate::today()))
    prodes_year <- as.Date(paste0(year_vec, prodes_start))
    return(lubridate::year(prodes_year[findInterval(adate, prodes_year) + 1]))
}


#' @title Find the tiled images closest to a given date.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Find the tiled images closest to a given date.
#'
#' @param img_tb    A tibble of images with a character column 'tile'.
#' @param ref_tiles A character. The tiles of reference.
#' @param ref_date  A length-one date. The date of reference.
#' @param untie     A length-one numeric. Small fraction of time used for
#' breaking ties.
#' @return          A subset of the rows of img_tb or NA
match_tile_date <- function(img_tb, ref_tiles, ref_date, untie = NULL) {
    if (nrow(img_tb) == 0)
        return(NA)  # return(img_tb)
    if (all(is.na(ref_tiles)))
        return(NA)
    if (length(ref_tiles) == 0)
        return(NA)
    if (is.na(ref_date))
        return(NA)
    if (!is.null(untie)) {
        ref_date <- ref_date + untie
    }
    subtiles <- img_tb %>% dplyr::filter(tile %in% ref_tiles) %>%
        dplyr::mutate(dist = abs(difftime(ref_date + untie, img_date,
                                          units = "days"))) %>%
        dplyr::group_by(tile) %>% dplyr::slice(which.min(dist)) %>% dplyr::ungroup()
    if (nrow(subtiles) == 0)
        return(NA)
    return(subtiles)
}


#' @title Return the size of the largest hole in time series.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the size of the largest hole in time series.
#'
#' @param x A tibble with a the fields 'img_date' and 'sat_image'. That is the date when the image was taken and its matching image.
#' @return  A length-one inetger
max_hole <- function(x) {
    max_hole_length <- 0
    img_vec <- x %>% dplyr::pull(sat_image)
    stats <- rle(is.na(img_vec))
    attr(stats, "class") <- NULL
    if (any(stats$values)) {
        max_hole_length <- stats %>% dplyr::as_tibble() %>%
            dplyr::filter(values == TRUE) %>% dplyr::pull(lengths) %>% max()
    }
    return(as.integer(max_hole_length))
}


