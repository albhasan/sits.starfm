#' @title  Fill in the clouds of img with the pixels from starfm.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Fill in the clouds of img with the pixels from starfm.
#'
#' @param img     A one-row tibble including the variables img_date, sat_image,
#' files, neigh, tile, scene, and  starfm.
#' @param tmp_dir A length-one character. A path to store intermediate files.
#' NULL by default.
#' @return        A character. Path to files.
#' @export
fill_clouds <- function(img, tmp_dir = NULL) {
    if (is.null(tmp_dir))
        tmp_dir <- tempdir()
    tmp_base_name <- basename(tempfile(pattern = "", tmpdir = ""))

    if (length(unlist(img$starfm)) == 1 && is.na(unlist(img$starfm))) {
        return(NA)
    }

    # No image to fill. Return StarFM
    if (is.na(img$sat_image)) {
        sfm_files <- img %>% dplyr::pull(starfm) %>% dplyr::bind_rows() %>%
            dplyr::pull(starfm)
        return(sfm_files)
    }

    # get QA files
    pixel_qa <- img %>% dplyr::pull(files) %>% unlist() %>%
        stringr::str_subset(pattern = "pixel_qa")
    neigh_qa <- ""
    if (!all(is.na(unlist(dplyr::pull(img, neigh)))))
        neigh_qa <- img %>% dplyr::pull(neigh) %>% dplyr::bind_rows() %>%
        dplyr::pull(files) %>% unlist() %>%
        stringr::str_subset(pattern = "pixel_qa")

    # get gdal parameters
    param <- list(fileext = ".bin", out_format = "ENVI",
                  creation_option = "SUFFIX=ADD", resampling = "near",
                  dstnodata = -9999) %>%
        append(get_landsat_metadata(pixel_qa[1]))

    # build the cloud mask
    qa_mosaic <- neigh_qa %>% c(pixel_qa) %>%
        gdal_warp(out_filename =
                      file.path(tmp_dir, paste0(paste("qa_mosaic",
                                                      img$sat_image,
                                                      tmp_base_name, sep = "_"),
                                                param[["fileext"]])),
                  out_format = param[["out_format"]],
                  creation_option = param[["creation_option"]],
                  extent_output = param[["extent_output"]],
                  target_srs = param[["crs"]], size_ouput = param[["img_size"]],
                  resampling = param[["resampling"]],
                  srcnodata = param[["srcnodata_l8"]],
                  dstnodata = param[["dstnodata"]])
    img_mask <- qa_mosaic %>%
        gdal_calc(
            out_filename =
                file.path(tmp_dir, paste0(paste("cloud_mask",
                                                img$sat_image,
                                                tmp_base_name, sep = "_"),
                                          param[["fileext"]])),
            expression = "((numpy.bitwise_and(A, 40) != 0) * 1).astype(int16)",
            dstnodata = param[["dstnodata"]],
            out_format = param[["out_format"]],
            creation_option = param[["creation_option"]])

    # fill in the clouds using StarFM
    starfm_tb <- img %>% dplyr::pull(starfm) %>% dplyr::bind_rows() %>%
        dplyr::select(t0_fine, starfm) %>%
        dplyr::mutate(mask = img_mask, band = get_landsat_band(starfm))

    # TODO: use ensurer::ensure
    if (nrow(starfm_tb) < 1) {
        warning("No files found!")
        return(NA)
    }

    img_filled <- lapply(1:(nrow(starfm_tb)), function(x, starfm_tb) {
        row_x <- starfm_tb %>% dplyr::slice(x)
        # there is no image, hence there are no clouds to fill in
        if (is.na(row_x$t0_fine))
            row_x %>% dplyr::pull(starfm) %>%
            return()
        # do the filling
        row_x %>% dplyr::select(t0_fine, mask, starfm) %>% unlist() %>%
            gdal_calc(out_filename =
                          file.path(tmp_dir,
                                    paste0(paste("filled", img$sat_image,
                                                 row_x$band, tmp_base_name,
                                                 sep = "_"),
                                           param[["fileext"]])),
                      expression = "(numpy.where(B, C, A)).astype(int16)") %>%
            return()
    }, starfm_tb = starfm_tb)
    img_filled %>% unlist() %>%
        return()
}

#' @title Run the StarFM fusion model.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Run the StarFM fusion model.
#'
#' @param img0_f       A one-row tibble.
#' @param img1_f       A one-row tibble.
#' @param band         A length-one character. The Landsat band to process.
#' @param out_filename A length-one character. The path to the results. NULL by
#' default.
#' @param tmp_dir      A length-one character. A path to store intermediate
#' files. NULL by default.
#' @return             A length-four character. The paths to the StarFM result
#' and its inputs, t1_fine, t1_coarse, and t0_coarse.
#' @export
run_starFM <- function(img0_f, img1_f, band, out_filename = NULL, tmp_dir = NULL) {
    if(!exists("logger")){
        logger                 <- log4r::create.logger()
    }

    log4r::debug(logger, sprintf("Starting starfm for %s",
                                 c(img0_f$sat_image, img1_f$sat_image, band,
                                   out_filename, tmp_dir)))

    # TODO create masks required by starFM ----
    # not working, StarFM throws exception!
    # gdal_calc.py --type=Byte -A LC08_L1TP_226064_20150924_20170403_01_T1_pixel_qa.tif --outfile=result.tif --calc="((numpy.bitwise_and(A, 40) == 0) * 1).astype(bool_)"

    # NOTE: random suffix, unrelated to tmp_dir
    tmp_base_name <- basename(tempfile(pattern = paste0(band, "_"),
                                       tmpdir = ""))

    # where to store temporal files. OS tempdir's life is very short
    if (is.null(tmp_dir))
        tmp_dir <- tempdir()
    if (!dir.exists(dirname(out_filename))) {
        warning("StarFM result directory not found. Creating it...")
        dir.create(dirname(out_filename))
    }

    # util functions
    .get_img_fine <- function(imgX_f) {
        imgX_f %>% dplyr::pull(files) %>% unlist() %>%
            stringr::str_subset(band) %>% dplyr::first() %>%
            return()
    }
    .get_neigh_fine <- function(imgX_f) {
        if (is.na(imgX_f$neigh))
            return(NA)
        row_neigh <- imgX_f %>% dplyr::pull(neigh) %>% dplyr::bind_rows()
        if (nrow(row_neigh) < 1)
            return(NA)
        row_neigh %>% dplyr::pull(files) %>% unlist() %>%
            stringr::str_subset(band) %>%
            return()
    }
    .get_img_coarse <- function(imgX_f) {
        imgX_f %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>%
            dplyr::pull(file_path) %>%
            return()
    }
    .call_gadal_warp <- function(input_files, out_filename, param) {
        gdal_warp(input_files = input_files, out_filename = out_filename,
                  out_format = param[["out_format"]],
                  creation_option = param[["creation_option"]],
                  extent_output = param[["extent_output"]],
                  target_srs = param[["crs"]], size_ouput = param[["img_size"]],
                  resampling = param[["resampling"]],
                  srcnodata = param[["srcnodata_l8"]],
                  dstnodata = param[["dstnodata"]]) %>%
            return()
    }

    # get image's files
    t1_fine <- img1_f %>% .get_img_fine()
    t0_fine <- img0_f %>% .get_img_fine()
    t1_fine_neigh <- img1_f %>% .get_neigh_fine()
    t0_fine_neigh <- img0_f %>% .get_neigh_fine()
    t1_coarse <- img1_f %>% .get_img_coarse()
    t0_coarse <- img0_f %>% .get_img_coarse()

    # get parameters
    param <- list(fileext = ".bin", out_format = "ENVI",
                  creation_option = "SUFFIX=ADD", resampling = "near",
                  dstnodata = -9999, nomd = TRUE)
    if (!is.na(img0_f$sat_image)) {
        param <- param %>% append(get_landsat_metadata(t0_fine))
    } else {
        param <- param %>% append(get_landsat_metadata(t1_fine))
    }

    prediction_date <- img0_f %>% dplyr::pull(tile) %>%
        unlist(recursive = FALSE) %>% dplyr::as_tibble() %>%
        dplyr::pull(img_date) %>% unique()
    if (length(prediction_date) != 1) {
        warning("Date conflict in the MODIS images")
        prediction_date <- prediction_date[1]
    }

    # Old Landsat 8 images use 0 as NO_DATA
    # TODO: wrap call to gdallocationinfo
    #pix00 <- call_os(command = "gdallocationinfo",
    #                 args = c(t1_fine, "0", "0", "-valonly"), stdout = TRUE)
    #if (as.numeric(pix00) != param[["srcnodata_l8"]])
    #    stop("Invalid no data value for Landsat 8 images!")

    # Uncertainty values. Taken from Gao:2017
    # TODO: Move to data?
    uncertainty_landsat <- 50
    uncertainty_modis <- 50
    if (band %in% c("sr_band2", "sr_band3", "sr_band4")) {
        uncertainty_landsat <- 20
        uncertainty_modis <- 20
    }

    if (length(t1_fine_neigh) != 8) {
        warning(
            sprintf("Not enough neighbor images to fill in the blanks of %s",
                    img0_f$sat_image))
    }

    # mosaic, project and cut landsat & modis images
    t1_fine <- t1_fine_neigh %>% c(t1_fine) %>% .[!is.na(.)] %>%
        ensurer::ensure_that(length(.) > 0, err_desc = "Invalid t1_fine images!") %>%
        .call_gadal_warp(
            out_filename =
                file.path(tmp_dir, paste0(paste("t1_fine", img1_f$scene,
                                                img1_f$img_date, tmp_base_name,
                                                sep = "_"),
                                          param[["fileext"]])), param)
    t1_coarse <- t1_coarse %>% gdal_match_name(band = band) %>%
        .call_gadal_warp(
            out_filename =
                file.path(tmp_dir,  paste0(paste("t1_coarse", img1_f$scene,
                                                 img1_f$img_date, tmp_base_name,
                                                 sep = "_"),
                                           param[["fileext"]])), param)
    if (!is.na(t0_fine))
        t0_fine <- t0_fine_neigh %>% c(t0_fine) %>% .[!is.na(.)] %>%
        ensurer::ensure_that(length(.) > 0, err_desc = "Invalid t0_fine images!") %>%
        .call_gadal_warp(
            out_filename =
                file.path(tmp_dir, paste0(paste("t0_fine", img1_f$scene,
                                                img1_f$img_date, tmp_base_name,
                                                sep = "_"),
                                          param[["fileext"]])), param)
    t0_coarse <- t0_coarse %>% gdal_match_name(band = band) %>%
        .call_gadal_warp(
            out_filename =
                file.path(tmp_dir, paste0(paste("t0_coarse", img0_f$scene,
                                                prediction_date, tmp_base_name,
                                                sep = "_"),
                                          param[["fileext"]])), param)

    # StarFM configuration file
    starfm_file <- file.path(tmp_dir,
                             paste0(paste("starfm", img0_f$scene,
                                          prediction_date, tmp_base_name,
                                          sep = "_"), ".txt"))
    if (is.null(out_filename) || is.na(out_filename))
        out_filename <- starfm_file %>% tools::file_path_sans_ext() %>%
        paste0(".bin")
    file_con <- file(starfm_file)
    writeLines(c("STARFM_PARAMETER_START", "NUM_IN_PAIRS = 1",
                 paste0("IN_PAIR_MODIS_FNAME = ", t1_coarse),
                 paste0("IN_PAIR_LANDSAT_FNAME = ", t1_fine),
                 paste0("IN_PDAY_MODIS_FNAME = ", t0_coarse),
                 paste0("OUT_PDAY_LANDSAT_FNAME = ", out_filename),
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
    call_os(command = "StarFM.exe", args = starfm_file)

    # add the projection parameters to image
    hdr_md <- t1_fine %>% paste0(".hdr") %>% readLines()
    hdr_md %>% stringr::str_subset("map info") %>%
        write(file = paste0(out_filename, ".hdr"), append = TRUE)
    hdr_md %>% stringr::str_subset("coordinate system string") %>%
        write(file = paste0(out_filename, ".hdr"), append = TRUE)

    log4r::debug(logger,
                 sprintf("Finishing starfm for %s",
                         c(img0_f$sat_image, img1_f$sat_image, band,
                           out_filename, tmp_dir)))
    return(c(starfm = out_filename, t1_fine = t1_fine, t1_coarse = t1_coarse,
             t0_fine = t0_fine, t0_coarse = t0_coarse, sfm_conf = starfm_file))
}

