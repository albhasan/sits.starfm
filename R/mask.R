#' @title Replace the clouds with a value.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Replace the clouds with a value.
#'
#' @param img               A one-row tibble with a the fields 'sat_image' and 'files'.
#' @param bands             A character. The names of the bands to mask.
#' @param replacement_value A numeric. The pixel value to fill in the clouds.
#' @param out_dir           A length-one character. Path to store results.
#' @param param             A list. A list of GDAL tranformation parameters.
#' @param tmp_dir           A length-one character. Path to store temporal files.
#' @return                  A tibble.
#' @export
mask_clouds <- function(img, bands, replacement_value, out_dir, param, tmp_dir = tempdir()){
    pixel_qa <- img %>% dplyr::pull(files) %>% unlist() %>%
        stringr::str_subset(pattern = "pixel_qa")
    param <- param %>% append(get_landsat_metadata(pixel_qa[1]))

    # build the cloud mask
    img_mask <- pixel_qa %>%
        gdalcmdline::gdal_calc(out_filename = file.path(tmp_dir,
                                                        paste0(paste("cloud_mask",
                                                                     img$sat_image,
                                                                     sep = "_"),
                                                        param[["fileext"]])),
            expression = "((numpy.bitwise_and(A, 40) != 0) * 1).astype(int16)",
            dstnodata = param[["dstnodata"]],
            out_format = param[["out_format"]],
            creation_option = param[["creation_option"]])
    img %>% dplyr::pull(files) %>% dplyr::bind_rows() %>%
        dplyr::mutate(band = get_landsat_band(file_path)) %>%
        dplyr::mutate(masked = purrr::map_chr(1:nrow(.),
                                              function(x, file_tb, img_mask, bands){
                                                  row_x <- file_tb %>% dplyr::slice(x) %>%
                                                      dplyr::filter(band %in% bands)
                                                  if(nrow(row_x) != 1) return(NA_character_)
                                                  row_x %>% dplyr::select(file_path) %>% unlist() %>%
                                                      c(img_mask) %>%
                                                      gdalcmdline::gdal_calc(
                                                          out_filename = file.path(out_dir,
                                                                                   paste0(paste(img$sat_image, row_x$band,
                                                                                                sep = "_"), "_maskcloud",
                                                                                          param[["fileext"]])),
                                                          dstnodata = param$dstnodata,
                                                          out_format = param$out_format,
                                                          creation_option = param$creation_option,
                                                          expression = paste0("(numpy.where(B,",  replacement_value, ", A)).astype(int16)")) %>%
                                                      return()
                                              }, file_tb = ., img_mask = img_mask, bands = bands)) %>%
        return()
}


#' @title Replace negatives values.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Replace the negatives values.
#'
#' @param img               A one-row tibble with a the fields 'sat_image' and 'files'.
#' @param bands             A character. The names of the bands to mask.
#' @param replacement_value A numeric. The pixel value to fill in the clouds.
#' @param out_dir           A length-one character. Path to store results.
#' @param param             A list. A list of GDAL tranformation parameters.
#' @param tmp_dir           A length-one character. Path to store temporal files.
#' @return                  A tibble.
#' @export
mask_negatives <- function(img, bands, replacement_value, out_dir, param, tmp_dir = tempdir()){
    img %>% dplyr::pull(files) %>% dplyr::bind_rows() %>%
        dplyr::mutate(band = get_landsat_band(file_path)) %>%
        dplyr::mutate(masked = purrr::map_chr(1:nrow(.),
                                              function(x, file_tb, bands){
                                                  row_x <- file_tb %>% dplyr::slice(x) %>%
                                                      dplyr::filter(band %in% bands)
                                                  if(nrow(row_x) != 1) return(NA_character_)
                                                  row_x %>% dplyr::select(file_path) %>% unlist() %>%
                                                      gdalcmdline::gdal_calc(
                                                          out_filename = file.path(out_dir,
                                                                                   paste0(paste(img$sat_image, row_x$band,
                                                                                                sep = "_"), "_maskneg", param[["fileext"]])),
                                                          dstnodata = param$dstnodata,
                                                          out_format = param$out_format,
                                                          creation_option = param$creation_option,
                                                          expression = paste0("(numpy.where(A < 0,",  replacement_value, ", A)).astype(int16)")) %>%
                                                      return()
                                              }, file_tb = ., bands = bands)) %>%
        return()
}

