# STACK IMAGES IN TOP OF EACH OTHER.


#' @title Pile image files into a sigle file.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Pile images into a single file.
#'
#' @param file_paths   A character. Path to image files.
#' @param out_fn       A length-one character. Path to the output file.
#' @param gdal_format  A length-one character. Gdal format of the output file.
#' @param no_data      A length-one numeric. No data value.
#' @param gdal_options A character. Options passed to gdal for raster creation.
#' @export
pile_files <- function(file_paths, out_fn,
                       gdal_format = "GTiff",
                       no_data = -9999,
                       gdal_options = c("TILED=YES",
                                        "COPY_SRC_OVERVIEWS=YES",
                                        "COMPRESS=LZW")){
    gdalcmdline::gdal_merge(input_files = file_paths, out_filename = out_fn,
                            separate = TRUE, of = gdal_format,
                            creation_option = gdal_options, init = no_data,
                            a_nodata = no_data) %>%
        invisible()
}


#' @title Pile images into files..
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Pile images into files.
#'
#' @param brick_imgs   A tibble of images.
#' @param file_col     Name of the column in brick_imgs with the paths to the images to pile up.
#' @param brick_bands  A character. Names of the bands.
#' @param brick_prefix A lengh-one character. Prefix to append to produced file names.
#' @param brick_scene  A length-one character. The scene or tile of the brick.
#' @param out_dir      A character. Path to a directory for storing results.
#' @param no_data      A length-one numeric. The value for no data.
#' @param gdal_options A character. Options passed to gdal for raster creation.
#' @return             A tibble of the matching PRODES dates and NA for the
#' @export
pile_up <- function(brick_imgs, file_col, brick_bands, brick_prefix,
                    brick_scene, out_dir, no_data, gdal_options){

    # helper function to pile up the files of a single band
    helper_pile_band <- function(i_tb){
        band <- i_tb %>%
            dplyr::pull(band) %>%
            unique() %>%
            ensurer::ensure_that(length(.) == 1, err_desc = "More than 1 band found.")

        band_short_name <- SPECS_L8_SR %>%
            dplyr::filter(band_designation == band) %>%
            dplyr::pull(short_name) %>%
            dplyr::first() %>%
            stringr::str_replace(" ", "_") %>%
            tolower()

        out_fn <- file.path(out_dir,
                            paste0(paste(brick_prefix, brick_scene,
                                         first_date, band_short_name,
                                         "STACK_BRICK", sep = "_"), ".tif"))

        i_tb %>% dplyr::pull(.data[[file_col]]) %>%
            unlist() %>%
            pile_files(out_fn = out_fn, gdal_format = "GTiff",
                       no_data = no_data, gdal_options = gdal_options) %>%
            tibble::enframe(name = NULL) %>%
            return()
    }

    first_date <- brick_imgs %>%
        dplyr::pull(img_date) %>%
        unlist() %>%
        min()

    img_tb <- brick_imgs %>%
        tidyr::unnest(.data[[file_col]]) %>%
        dplyr::mutate(band = get_landsat_band(.data[[file_col]])) %>%
        dplyr::filter(band %in% brick_bands)

    img_tb %>%
        dplyr::group_by(band) %>%
        dplyr::do(helper_pile_band(.data)) %>%
        dplyr::ungroup() %>%
        return()
}


