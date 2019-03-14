sits.env <- new.env()
utils::globalVariables(c(
    ".",
    "%>%",
    "b4",
    "b5",
    "band",
    "band_designation",
    "brick_ratio",
    "cloud_cov",
    "data",
    "dist",
    "file_path",
    "files",
    "filled",
    "fill_value",
    "gdal_prefix",
    "gdal_suffix",
    "group_by",
    "h",
    "img_date",
    "l8_sr_designation",
    "logger",
    "mask",
    "n_expected",
    "n_img",
    "neigh",
    "next_best",
    "PATH",
    "prodes_year",
    "pyear",
    "ROW",
    "row_number",
    "sat_image",
    "scale_factor",
    "scene",
    "short_name",
    "starfm",
    "t0_fine",
    "tile",
    "ts",
    "v",
    "valid_range",
    "value",
    "values"
))


#' Pipe
#'
#' Magrittr compound assignment pipe-operator.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs,rhs A visualisation and a function to apply to it.
#' @export
NULL



#' .data
#'
#'
#'
#' @importFrom dplyr .data
#' @name .data
#' @rdname .data
#' @export
NULL


.onAttach <- function(libname, pkgname) {
    if (!exists("logger") || is.null(logger) || is.na(logger)) {
        logger                 <- log4r::create.logger()
        log4r::logfile(logger) <- file.path(path.expand("~"), paste0("sits.starfm_", system('uname -n', intern = TRUE),".log"))
        log4r::level(logger)   <- "DEBUG"
    }
    packageStartupMessage(sprintf("sits.starfm. Logger file: %s", logger$logfile))
}

