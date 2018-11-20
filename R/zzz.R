sits.env <- new.env()
utils::globalVariables(c(
    ".",
    "%>%",
    "band_designation",
    "brick_ratio",
    "cloud_cov",
    "data",
    "dist",
    "file_path",
    "files",
    "fill_value",
    "gdal_prefix",
    "gdal_suffix",
    "group_by",
    "h",
    "img_date",
    "l8_sr_designation",
    "mask",
    "n_expected",
    "n_img",
    "neigh",
    "PATH",
    "prodes_year",
    "ROW",
    "row_number",
    "sat_image",
    "scale_factor",
    "scene",
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
