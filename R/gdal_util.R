#' @title Get the full MOD13Q1 name from a file path.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the full MOD13Q1 name from a landsat file
#'
#' @param path_modis A character. Path to a file path.
#' @param band       A length-one character. Name of a Landsat 8 band.
#' @return A character. The full gdal name of a MOD13Q1 band.
gdal_match_name <- function(path_modis, band) {
    if (length(path_modis) == 1) {
        pre_suf <- SPECS_MOD13Q1 %>%
            dplyr::inner_join(SPECS_L8_SR,
                              by = c(common_name = "mod13q1_name")) %>%
            dplyr::filter(band_designation == band) %>%
            dplyr::select(gdal_prefix, gdal_suffix) %>%
            unlist()
        paste0(pre_suf[1], path_modis, pre_suf[2]) %>% return()
    } else if (length(path_modis) > 1) {
        sapply(path_modis, gdal_match_name, band = band) %>% return()
    }
}


#' @title Get the an image's dataset names.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the GDAL dataset names in an image file.
#'
#' @param file_path A character. Path to a file.
#' @return          A tibble.
#' @export
get_dataset_names <- function(file_path){
    stopifnot(is.atomic(file_path))
    if (is.na(file_path) || length(file_path) < 1) return(file_path)
    if (length(file_path) == 1) {
        system2("gdalinfo", file_path, stdout = TRUE) %>%
            stringr::str_subset(pattern = "SUBDATASET_[0-9]+_NAME=") %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            tidyr::separate(col = 1, into = c("key", "value"), sep = "=") %>%
            dplyr::pull(value) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            tidyr::separate(col = 1, into = c(NA, NA, NA, NA, "band"),
                            sep = ":", remove = FALSE) %>%
            tibble::as_tibble() %>%
            dplyr::rename(gdal_name = 1, band = 2) %>%
            return()
    } else {
        return(vapply(file_path, get_dataset_names, integer(1)))
    }
}


#' @title Get the number of bands in a file.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the number of bands in a file.
#'
#' @param filepath A character. Path to a file.
#' @return         A numeric.
#' @export
get_number_of_bands <- function(filepath) {
    stopifnot(is.atomic(filepath))
    if (is.na(filepath) || length(filepath) < 1) return(filepath)
    if (length(filepath) == 1) {
        system2("gdalinfo", filepath, stdout = TRUE) %>%
            stringr::str_subset("Band") %>% dplyr::last() %>%
            stringr::str_split(" ") %>% unlist() %>% dplyr::nth(2) %>%
            as.integer() %>%
            return()
    } else {
        return(vapply(filepath, get_number_of_bands, integer(1)))
    }
}

