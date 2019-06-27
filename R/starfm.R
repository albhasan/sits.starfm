#' @title  Fill in the clouds of img with the pixels from StarFM.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Fill in the clouds of img with the pixels from starfm.
#'
#' @param img     A one-row tibble including the variables img_date, sat_image,
#' files, neigh, tile, scene, and  starfm.
#' @return        A character. Path to files.
#' @export
fill_clouds <- function(img) {

    if (length(unlist(img$starfm)) == 1 && is.na(unlist(img$starfm))) {
        return(NA)
    }

    # No image to fill. Return StarFM.
    if (is.na(img$sat_image))
        img %>% dplyr::pull(starfm) %>%
            dplyr::bind_rows() %>%
            dplyr::pull(starfm) %>%
            return()

    # Get QA files.
    pixel_qa <- img %>% dplyr::pull(files) %>%
        unlist() %>%
        stringr::str_subset(pattern = "pixel_qa")

    # Get gdal parameters.
    param <- list(fileext = ".bin",
                  out_format = "ENVI",
                  creation_option = "SUFFIX=ADD",
                  resampling = "near",
                  dstnodata = -9999) %>%
        append(get_landsat_metadata(pixel_qa[1]))

    # Build the cloud mask.
    qa_mosaic <- pixel_qa %>%
        gdal_warp(
            out_filename = tempfile(pattern = paste("qa_mosaic", 
                                                   img$sat_image,
                                                   sep = "_"),
                                   fileext = param[["fileext"]]),
            out_format = param[["out_format"]],
            creation_option = param[["creation_option"]],
            extent_output = param[["extent_output"]],
            target_srs = param[["crs"]],
            size_ouput = param[["img_size"]],
            resampling = param[["resampling"]],
            srcnodata = param[["srcnodata_l8"]],
            dstnodata = param[["dstnodata"]])

    img_mask <- qa_mosaic %>%
        gdal_calc(
            out_filename = tempfile(pattern = paste("cloud_mask", 
                                                   img$sat_image,
                                                   sep = "_"),
                                   fileext = param[["fileext"]]),
            expression = "((numpy.bitwise_and(A, 40) != 0) * 1).astype(int16)",
            dstnodata = param[["dstnodata"]],
            out_format = param[["out_format"]],
            creation_option = param[["creation_option"]])

    # Helper function to do the filling.
    .fill_mask <- function(band, t0_fine, starfm, mask){
        if (is.na(t0_fine))
            return(starfm)
        c(t0_fine, mask, starfm) %>%
            gdal_calc(
                out_filename = tempfile(pattern = paste("filled", img$sat_image,
                                                        band, sep = "_"),
                                        fileext = param[["fileext"]]),
                expression = "(numpy.where(B, C, A)).astype(int16)") %>%
            return()
    }

    # Fill in the clouds using StarFM images.
    img %>% dplyr::pull(starfm) %>%
        dplyr::bind_rows() %>%
        dplyr::select(band, t0_fine, starfm) %>%
        dplyr::mutate(mask = img_mask) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "No StarFM image found.") %>%
        purrr::pmap(.fill_mask) %>%
        unlist() %>%
        return()
}

#' @title Run the StarFM fusion model.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Run the StarFM fusion model.
#'
#' @param img_t0   A one-row tibble. Metadata of the image at time 0.
#' @param img_t1   A one-row tibble. Metadata of the image at time 1.
#' @param band     A length-one character. The Landsat band to process.
#' @param out_file A length-one character. The path to the results.
#' @return         A length-four character. The paths to the StarFM result and
#' its inputs, t1_fine, t1_coarse, t0_coarse, and StarFM configuration file.
#' @export
run_starFM <- function(img_t0, img_t1, band, out_file = NULL) {

    # NOTE: StarFM doesn't work when masks are used; it throws an exception.:
    # gdal_calc.py --type=Byte -A LC08_L1TP_226064_20150924_20170403_01_T1_pixel_qa.tif --outfile=result.tif --calc="((numpy.bitwise_and(A, 40) == 0) * 1).astype(bool_)"

    .call_gadal_warp <- function(input_files, out_filename, param, img_type, band) {
        stopifnot(img_type %in% c("landsat8", "MOD13Q1"))
        if (img_type == "landsat8"){
            fill_value <- SPECS_L8_SR %>%
                dplyr::filter(band_designation == band) %>%
                dplyr::pull(fill_value)
        }else if (img_type == "MOD13Q1") {
            fill_value <- SPECS_MOD13Q1 %>%
                dplyr::filter(l8_sr_designation == band) %>%
                dplyr::pull(fill_value)
        }
        gdal_warp(input_files = input_files,
                  out_filename = out_filename,
                  out_format = param[["out_format"]],
                  creation_option = param[["creation_option"]],
                  extent_output = param[["extent_output"]],
                  target_srs = param[["crs"]],
                  size_ouput = param[["img_size"]],
                  resampling = param[["resampling"]],
                  srcnodata = fill_value,
                  dstnodata = param[["dstnodata"]]) %>%
            return()
    }

    # get image's files
    t0_coarse <- img_t0 %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>% dplyr::pull(file_path) %>% ensurer::ensure_that(length(.) > 0, err_desc = "Unable to find band")
    t1_coarse <- img_t1 %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>% dplyr::pull(file_path) %>% ensurer::ensure_that(length(.) > 0, err_desc = "Unable to find band")
    t0_fine <- img_t0 %>% dplyr::pull(files) %>% unlist() %>% stringr::str_subset(band) %>% ensurer::ensure_that(length(.) == 1, err_desc = "Unable to find band")
    t1_fine <- img_t1 %>% dplyr::pull(files) %>% unlist() %>% stringr::str_subset(band) %>% ensurer::ensure_that(length(.) == 1, err_desc = "Unable to find band")

    param <- list(fileext = ".bin",
                  out_format = "ENVI",
                  creation_option = "SUFFIX=ADD",
                  resampling = "near",
                  dstnodata = -9999,
                  nomd = TRUE) %>%
        append(get_landsat_metadata(t0_fine))

    prediction_date <- img_t0 %>%
        dplyr::pull(tile) %>%
        dplyr::bind_rows() %>%
        dplyr::pull(img_date) %>%
        unique() %>%
        ensurer::ensure_that(length(.) == 1,
                             err_desc = "MODIS images' dates mismatch.")

    # NOTE: Old Landsat 8 images use 0 as NO_DATA.

    # Uncertainty values. Taken from Gao:2017
    uncertainty_landsat <- 50
    uncertainty_modis <- 50
    if (band %in% c("sr_band2", "sr_band3", "sr_band4")) {
        uncertainty_landsat <- 20
        uncertainty_modis <- 20
    }

    # Mosaic, project and cut landsat & modis images.
    t1_fine <- t1_fine %>%
        .call_gadal_warp(
            out_filename = tempfile(pattern = paste("t1_fine", img_t1$scene,
                                                    band, img_t1$img_date,
                                                    sep = "_"),
                                    fileext = param[["fileext"]]),
            param = param,
	    img_type = "landsat8",
            band = band)
    t1_coarse <- t1_coarse %>%
        gdal_match_name(band = band) %>%
        .call_gadal_warp(
            out_filename = tempfile(pattern = paste("t1_coarse", img_t1$scene,
                                                    band, img_t1$img_date,
                                                    sep = "_"),
                                    fileext = param[["fileext"]]),
            param = param,
            img_type = "MOD13Q1",
            band = band)
    if (!is.na(t0_fine))
        t0_fine <- t0_fine %>% 
            .call_gadal_warp(
                out_filename = tempfile(pattern = paste("t0_fine", img_t0$scene,
                                                        band, img_t1$img_date,
                                                        sep = "_"),
                                        fileext = param[["fileext"]]),
                param = param,
                img_type = "landsat8",
                band = band)
    t0_coarse <- t0_coarse %>%
        gdal_match_name(band = band) %>%
        .call_gadal_warp(
            out_filename = tempfile(pattern = paste("t0_coarse", img_t0$scene,
                                                    band, prediction_date,
                                                    sep = "_"),
                                    fileext = param[["fileext"]]),
            param = param,
            img_type = "MOD13Q1",
            band = band)

    # StarFM files.
    starfm_conf <- tempfile(pattern = paste("starfm", img_t0$scene, band,
                                             prediction_date, sep = "_"),
                            fileext = ".txt")
    if (is.null(out_file) || is.na(out_file))
        out_file <- starfm_conf %>% tools::file_path_sans_ext() %>%
            paste0(".bin")
    file_con <- file(starfm_conf)
    writeLines(c("STARFM_PARAMETER_START", "NUM_IN_PAIRS = 1",
                 paste0("IN_PAIR_MODIS_FNAME = ", t1_coarse),
                 paste0("IN_PAIR_LANDSAT_FNAME = ", t1_fine),
                 paste0("IN_PDAY_MODIS_FNAME = ", t0_coarse),
                 paste0("OUT_PDAY_LANDSAT_FNAME = ", out_file),
                 paste0("NROWS = ", param[["nrow"]]),
                 paste0("NCOLS = ", param[["ncol"]]),
                 paste0("RESOLUTION = ", param[["pixel_size_x"]]),
                 paste0("SCALE_FACTOR = ", param[["scale_factor"]]),
                 paste0("LANDSAT_FILLV = ", param[["dstnodata"]]),
                 paste0("LANDSAT_DATA_RANGE = ", paste(param[["data_range_l8"]],
                                                       collapse = ", ")),
                 paste0("LANDSAT_UNCERTAINTY = ", uncertainty_landsat),
                 paste0("MODIS_FILLV = ", param[["dstnodata"]]),
                 paste0("MODIS_DATA_RANGE = ", paste(param[["data_range_mod"]],
                                                     collapse = ", ")),
                 paste0("MODIS_UNCERTAINTY = ", uncertainty_modis),
                 "USE_SPATIAL_FLAG = 1", "MAX_SEARCH_DISTANCE = 750",
                 "NUM_SLICE_PURE_TEST = 40", "STARFM_PARAMETER_END"), file_con)
    close(file_con)

    # call the fusion model
    call_os(command = "StarFM.exe", args = starfm_conf)

    # add the projection parameters to image
    hdr_md <- t1_fine %>% paste0(".hdr") %>% readLines()
    hdr_md %>% stringr::str_subset("map info") %>%
        write(file = paste0(out_file, ".hdr"), append = TRUE)
    hdr_md %>% stringr::str_subset("coordinate system string") %>%
        write(file = paste0(out_file, ".hdr"), append = TRUE)

    return(c(starfm = out_file, t1_fine = t1_fine, t1_coarse = t1_coarse,
             t0_fine = t0_fine, t0_coarse = t0_coarse, sfm_conf = starfm_conf))
}

