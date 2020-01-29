# PILE IMAGES BY BAND INTO TIFF FILES.


#' @title Build a SITS brick using a mixture of HLS images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick using Harmonized Landsat8-Sentinel2 images either L30 or S30.
#'
#' @param in_dir        A length-one character. Path to a directory of HLS images.
#' @param brick_tile    A length-one character. The tile of the brick.
#' @param brick_from    A length-one character. The approximated first day of the brick.
#' @param brick_to      A length-one character. The approximated last day of the brick.
#' @param brick_bands   A character. The HLS bands.
#' @param out_dir       A length-one integer. The output directory.
#' @return              A tibble.
#' @export
#' @importFrom rlang .data
build_brick_hls_griffiths <- function(in_dir, brick_tile, brick_from, brick_to,
                                      brick_bands, out_dir){

    #---- TODO -----
    # remove
    in_dir       <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/harmonized_landsat_sentinel2/data/hls"
    brick_tile   <- "T19LGK"
    brick_from   <- "2016-08-08"
    brick_to     <- "2017-08-31"
    #out_dir      <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_mix"
    #fmask_dir    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/mask_s2_fmask4"
    #sen2agri_dir <- "/disks/d5/sen2agri/archive/dwn_def/s2/default/hls_19lgk"
    #brick_product <- "L30S30"
    #brick_prefix <- paste0("HLS", brick_product, "-MIX")
    #out_no_data  <- -9999
    #stopifnot(all(sapply(c(in_dir, out_dir, fmask_dir, sen2agri_dir), dir.exists)))
    #stopifnot(as.Date(brick_from) <= as.Date(brick_to))

    #---- Utilitary functions ----

    #---- Function body ----

    # Get images.
    #landsat8_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/landsat8"
    #sentinel2_dir <- ""
    #landsat_tb <- landsat8_dir %>%
    #    build_landsat_tibble()
    #sentinel_tb <- sentinel2_dir %>%

    # These are the files downloaded by the Brazil data cube project.
    # They were already processed by by Fmask4.
    # fmask_tb <- fmask_dir %>%
    #     list.files(pattern = "_Fmask4[.]tif$", full.names = TRUE) %>%
    #     tibble::enframe(name = NULL) %>%
    #     dplyr::rename(fmask_path = value) %>%
    #     dplyr::mutate(file_name = basename(fmask_path)) %>%
    #     tidyr::separate(file_name,
    #                     into = c("level", "tile", "stuff", "img_date", NA),
    #                     sep = '_') %>%
    #     # NOTE: Assume all the masks were made of Sentienel2 images of spatial resolution of 30 meters.
    #     dplyr::mutate(img_date = lubridate::ymd(stringr::str_sub(img_date, 1, 8)),
    #                   product = "S30")

    # Get HLS' metadata.
    img_hls_tb <- in_dir %>%
        sits.starfm::build_hls_tibble(pattern = "*hdf$") %>%
        ensurer::ensure_that(nrow(.) > 0,
                            err_desc = sprintf("No HLS images found at: %s",
                                 in_dir)) %>%
        dplyr::select(-.data$img_extent) %>%
        tidyr::drop_na() %>%
        dplyr::filter(img_date >= as.Date(brick_from, format = "%Y-%m-%d"),
                      img_date <= as.Date(brick_to,   format = "%Y-%m-%d"),
                      tile == brick_tile) %>%
        dplyr::mutate(period = as.integer(img_date - dplyr::lag(img_date)),
                      bands_tb = purrr::map(file_path, get_dataset_names)) %>%
        dplyr::arrange(product, tile, img_date) %>%
        dplyr::mutate(red_edges = ifelse(product == "S30",
                                         compute_oli_red_edges(bands_tb), NA)) %>%
        ensurer::ensure_that(nrow(.) >=  0, err_desc = "Not enough images!.")


compute_oli_red_edges <- function(band_tb){
    if (tibble::is_tibble(band_tb)) {
        expected_bands <- c("B04", "B05", "B06", "B07", "B08")
        band_tb <- band_tb %>%
            dplyr::filter(band %in% expected_bands) %>%
            dplyr::left_join(SPECS_HLS_NOMENCLATURE,
                             by = c("band" = "HLS_band_code_name_S2")) %>%
            ensurer::ensure_that(nrow(.) == length(expected_bands))
        rho_j_vec <- character()
        for(i in 1:(nrow(band_tb) - 2)){
            j <- i + 1
            k <- j + 1
            lambda_i <- band_tb$center_wavelength_msi[[i]] * 10^-9
            lambda_j <- band_tb$center_wavelength_msi[[j]] * 10^-9
            lambda_k <- band_tb$center_wavelength_msi[[k]] * 10^-9
            rho_i <- band_tb$gdal_name[[i]]
            rho_k <- band_tb$gdal_name[[k]]
            rho_j_vec[[i]] <- rho_j(lambda_i, lambda_j, lambda_k, rho_i, rho_k)
        }
        return(rho_j_vec)
    } else if (is.list(band_tb)) {
        return(lapply(band_tb, compute_oli_red_edges))
    }
}


# @title Compute missing Landsat8 bands.
# @author Alber Sanchez, \email{alber.ipia@@inpe.br}
# @description Compute the Landsat-8's missing red edge bands.
#
# @param lambda_i A length-one double. Center wavelenght for the neighbor Sentinel-2 spectral band i.
# @param lambda_j A length-one double. Center wavelenght for the neighbor Sentinel-2 spectral band j.
# @param lambda_k A length-one double. Center wavelenght for the neighbor Sentinel-2 spectral band k.
# @return         A length-one character. A path to a raster.
rho_j <- function(lambda_i, lambda_j, lambda_k, rho_i, rho_k, out_file = tempfile(pattern = "rho_j_", fileext = ".tif")){
    # GRIFFITHS, P.; NENDEL, C.; HOSTERT, P. Intra-annual reflectance composites
    # from Sentinel-2 and Landsat for national-scale crop and land cover mapping.
    # Remote Sensing of Environment, v. 220, n. October 2017, p. 135–151, jan. 2019.
    # Equation 1.
    stopifnot(all(vapply(list(lambda_i, lambda_j, lambda_k, rho_i, rho_k), is.atomic, logical(1))))
    stopifnot(length(c(lambda_i, lambda_j, lambda_k)) == 3)

    # A rho_i
    # B rho_k
    gdal_exp <- sprintf("(%2$1.10f - %1$1.10f) * (B - A) /(%3$1.10f - %2$1.10f) + A",
                        lambda_i, lambda_j, lambda_k)

    gdalcmdline::gdal_calc(input_files = c(rho_i, rho_k),
                           out_filename = out_file, expression = gdal_exp)
    return(out_file)
}




    #TODO: Fill in the 3 Landsat8-OLI red bands using eq 1.
    img_hls_tb %>%
        dplyr::filter(product == "L30") %>%
        dplyr::mutate(oli_redges = )





    #warning("There aren't fmasks for every HLS S30")
    #img_hls_tb %>%
    #    dplyr::filter(product == "S30") %>%
    #    (function(x){print(sprintf("Number of HLS S30 images: %s", nrow(x))); invisible(x)}) %>%
    #    dplyr::filter(!is.na(fmask_path)) %>%
    #    (function(x){print(sprintf("Number of HLS S30 images with Fmask: %s", nrow(x)))})

    #warning("Unmatched")
    #img_hls_tb %>%
    #    dplyr::filter(product == "S30", is.na(fmask_path)) %>%
    #    dplyr::mutate(fname = basename(file_path)) %>%
    #    dplyr::select(fname, tile, img_date)



    # These are the files downloaded by sen2agri.
    img_s2a_tb <- sen2agri_dir %>%
        list.dirs(recursive = FALSE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(sen2agri_path = value) %>%
        dplyr::mutate(dir_name = basename(sen2agri_path)) %>%
        tidyr::separate(dir_name,
                        into = c("mission", "level", "img_date",
                                 "baseline", "orbit", "tile", "discriminator"),
                        sep = '_') %>%
        dplyr::filter(mission %in% c("S2A", "S2B")) %>%
        dplyr::mutate(img_date = as.Date(lubridate::ymd_hms(img_date)))




    img_hls_tb %>%
        dplyr::filter(product == "S30", is.na(sen2agri_path)) %>%
        (function(.data){print(nrow(.data)); invisible(.data)}) %>%
        as.data.frame()
        ensurer::ensure_that(nrow(.) < 1, err_desc = "Some HLS S20 images miss Fmask.")


}


#' @title Build a SITS brick using HLS images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick using Harmonized Landsat8-Sentinel2 images either L30 or S30.
#'
#' @param in_dir        A length-one character. Path to a directory of HLS images.
#' @param brick_tile    A length-one character. The tile of the brick.
#' @param brick_product A length-one character. The HLS product (i.e. L30).
#' @param brick_from    A length-one character. The approximated first day of the brick.
#' @param brick_to      A length-one character. The approximated last day of the brick.
#' @param brick_bands   A character. The HLS bands.
#' @param brick_n_img   A length-one integer. The number of images in a brick.
#' @param out_dir       A length-one integer. The output directory.
#' @return              A tibble.
#' @export
#' @importFrom rlang .data
build_brick_hls_raw <- function(in_dir, brick_tile, brick_product, brick_from,
                                brick_to, brick_bands, brick_n_img, out_dir){

    brick_prefix <- paste0("HLS", brick_product, "-RAW")
    out_no_data  <- -9999

    #---- Utilitary functions ----

    # @title Build a VRT file.
    # @author Alber Sanchez, \email{alber.ipia@@inpe.br}
    # @description Build a GDAL virtual file from the inputs.
    #
    # @param x A tibble.
    # @return  The same tibble with an additional variable: The path to the GDAL VRT file.
    build_vrt <- function(x){
        out_file <- tempfile(pattern = "hls_brick_bands_", fileext = ".vrt")
        src_no_data <- x %>%
            dplyr::pull(.data$no_data) %>%
            unique() %>%
            ensurer::ensure_that(length(.) == 1,
                                 err_desc = "Missmatching no_data values.")
        x %>%
            dplyr::pull(.data$gdal_band) %>%
            gdalUtils::gdalbuildvrt(output.vrt = out_file, separate = TRUE,
                                    allow_projection_difference = FALSE,
                                    q = TRUE, srcnodata = src_no_data)
        x %>%
            dplyr::mutate(vrt_file = out_file,
                          brick_start_date = min(img_date)) %>%
            return()
    }

    # @title Compute the NA ratio
    # @author Alber Sanchez, \email{alber.ipia@@inpe.br}
    # @description Compute the percentage of NA in a raster.
    #
    # @param in_file A character. Path to a file.
    # @return        A numeric.
    compute_na_ratio <- function(g_path){
        if(!is.character(g_path)) {
            return(NA)
        } else if (length(g_path) == 1) {
            r <- g_path %>%
                raster::raster()
            sum(is.na(r[])) / length(r[]) %>%
                return()
        } else if (length(g_path) > 1) {
            vapply(g_path, compute_na_ratio, numeric(1)) %>%
               return()
        } else {
            warning("Unknown case.")
            return(NA)
        }
    }

    #---- Function body ----

    band_name <- cloud_mask <- common_name <- expected_date <- g_path <- na_percent <- no_data <- product <- qa_band <- QA_description <- SDS_name <- tile_file <- tile_path <- vrt_file <- NULL

    #
    stopifnot(brick_product %in% c("L30", "S30"))
    if (brick_product == "L30") {
        img_specs <- SPECS_HLS_L30 %>%
            dplyr::left_join(dplyr::select(SPECS_HLS_NOMENCLATURE, short_name,
                                           SDS_name = .data$HLS_band_code_name_L8),
                             by = "SDS_name")
    } else if (brick_product == "S30") {
        img_specs <- SPECS_HLS_S30 %>%
            dplyr::left_join(dplyr::select(SPECS_HLS_NOMENCLATURE, short_name,
                                           SDS_name = .data$HLS_band_code_name_S2),
                             by = "SDS_name")
    }
    img_specs <- img_specs %>%
        ensurer::ensure_that(all(brick_bands %in% .$SDS_name),
                             err_desc = "Unknown bands!") %>%
        dplyr::select(band = SDS_name, scale = .data$Scale, no_data = .data$Fill_value,
                      band_name = short_name)

    # Get image's metadata.
    img_tb <- in_dir %>%
        sits.starfm::build_hls_tibble(pattern = "*hdf$") %>%
        ensurer::ensure_that(nrow(.) > 0,
                            err_desc = sprintf("No HLS images found at: %s",
                                 in_dir)) %>%
        dplyr::select(-.data$img_extent) %>%
        tidyr::drop_na() %>%
        dplyr::filter(img_date >= as.Date(brick_from, format = "%Y-%m-%d"),
                      img_date <= as.Date(brick_to,   format = "%Y-%m-%d"),
                      tile == brick_tile,
                      product == brick_product) %>%
        dplyr::arrange(product, tile, img_date) %>%
        dplyr::mutate(period = as.integer(img_date - dplyr::lag(img_date))) %>%
        ensurer::ensure_that(nrow(.) >=  brick_n_img,
                             err_desc = "Not enough images!.")

    # Ensure we aren't building a brick of corner images.
    img_period <- 10
    if (brick_product == "L30")
        img_period <- 16
    time_tb <- img_tb %>%
        dplyr::pull(img_date) %>%
        min()
    if (abs(1 - brick_n_img/nrow(img_tb)) > 0.25) {
        warning("Skipping corner images...")
        time_tb <- img_tb %>%
            dplyr::slice(1:2) %>%
            dplyr::mutate(g_path = paste0('HDF4_EOS:EOS_GRID:"', file_path,
                                          '":Grid:', img_specs$band[[1]])) %>%
            dplyr::mutate(na_percent = purrr::map_dbl(g_path, compute_na_ratio)) %>%
            dplyr::filter(na_percent == min(na_percent)) %>%
            dplyr::pull(img_date)
    }

    time_tb <- time_tb %>%
        # seq(by = img_period, length.out = brick_n_img) %>%
        seq(by = img_period, length.out = brick_n_img * 2) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(img_date = "value") %>%
        dplyr::mutate(expected_date = img_date)
    holes_in_ts <- img_tb %>%
        dplyr::right_join(time_tb, by = "img_date") %>%
        dplyr::slice(1:brick_n_img) %>%
        dplyr::filter(is.na(file_path))
    if (nrow(holes_in_ts) > 0)
        warning(sprintf("There are %s missing images for brick %s: %s", nrow(holes_in_ts),
                paste(brick_prefix, brick_tile, brick_from, sep = '_'),
                paste(dplyr::pull(holes_in_ts, expected_date), collapse = ", ")))
    if (nrow(holes_in_ts) > 3)
        stop(sprintf("Too many missing images for brick %s.",
             paste(brick_prefix, brick_tile, brick_from, sep = '_')))
    img_tb <- img_tb %>%
        dplyr::left_join(time_tb, by = "img_date") %>%
        tidyr::drop_na(expected_date) %>%
        dplyr::slice(1:brick_n_img) %>%
        ensurer::ensure_that(length(unique(.$expected_date)) == brick_n_img,
                             err_desc = "Not enough images.")

    # HDF to VRT.
    brick_tb <- img_tb %>%
        dplyr::left_join(tidyr::expand(., file_path, band = brick_bands),
                         by = "file_path") %>%
        dplyr::left_join(img_specs, by = "band") %>%
        dplyr::mutate(gdal_band = paste0('HDF4_EOS:EOS_GRID:"', file_path,'":Grid:', band)) %>%
        dplyr::group_by(band) %>%
        dplyr::group_map(~ build_vrt(.x), keep = TRUE) %>%
        dplyr::bind_rows() %>%
        dplyr::select(product, tile, band, vrt_file, brick_start_date, no_data, band_name) %>%
        dplyr::distinct() %>%
    # VRT to TIF
        dplyr::mutate(tile_file = file.path(out_dir,
                                            paste0(brick_prefix, '_',
                                                   tile, '_', brick_start_date,
                                                   '_', band,
                                                   "_STACK_BRICK.tif"))) %>%
        dplyr::mutate(brick_created = purrr::pmap_lgl(
            dplyr::select(., vrt_file, tile_file, no_data),
            function(vrt_file, tile_file, no_data){
                gdal_warp(input_files = vrt_file,
                          out_filename = tile_file,
                          out_format = "GTiff",
                          creation_option = "BIGTIFF=YES",
                          srcnodata = no_data,
                          dstnodata = out_no_data)
                return(TRUE)
            }))

    bands_vi_tb <- brick_tb %>%
        dplyr::filter(band_name %in% c("blue", "red", "nirnarrow", "swir1",
                                       "swir2")) %>%
        dplyr::arrange(band_name) %>%
        dplyr::mutate(common_name = sort(c("blue", "nir", "red", "swir",
                                           "swir2"))) %>%
        dplyr::mutate(is_valid = purrr::map2_lgl(common_name, band_name, grepl)) %>%
        ensurer::ensure_that(all(.$is_valid),
                             err_desc = "Some bands weren't found.")
    vi_bands <- bands_vi_tb %>% dplyr::pull(tile_file)
    names(vi_bands) <- bands_vi_tb %>% dplyr::pull(common_name)

    # EVI computation is wrong!
    brick_vi_tb <- c("evi2", "msavi", "nbr", "nbr2", "ndmi", "ndvi", "savi") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(band = "value") %>%
        dplyr::mutate(tile_path = file.path(out_dir,
            stringr::str_replace(basename(brick_tb$tile_file[[1]]),
                                 brick_tb$band[[1]], band))) %>%
        dplyr::mutate(brick_created = purrr::pmap_chr(
            dplyr::select(., band, tile_path),
            function(band, tile_path, vi_bands){
                compute_brick_index(in_file = vi_bands,
                                    index = band,
                                    out_file = tile_path,
                                    in_no_data = out_no_data,
                                    out_no_data = out_no_data) %>%
                    return()
            }, vi_bands = vi_bands))

    # Cloud bricks.
    cloud_imgs <- img_tb %>%
        # HDF to VRT.
        dplyr::mutate(qa_band = paste0('HDF4_EOS:EOS_GRID:"', file_path,'":Grid:', 'QA')) %>%
        dplyr::mutate(qa_vrt = purrr::map_chr(qa_band, function(x){
                          out_file <- tempfile(pattern = "cloud_mask_",
                                               fileext = ".vrt")
                          qa_no_data <- img_specs %>%
                              dplyr::filter(band == "QA") %>%
                              dplyr::pull(no_data)
                          gdalUtils::gdalbuildvrt(gdalfile = x,
                              output.vrt = out_file, separate = TRUE,
                              allow_projection_difference = FALSE, q = TRUE,
                              src_no_data  = qa_no_data, vrtnodata = out_no_data
                          )
                          return(out_file)
                     })) %>%
        # Get mask out of QA.
        dplyr::mutate(cloud_mask = purrr::map_chr(.$qa_vrt, function(x){
            mask_value <- SPECS_HLS_QA %>%
                dplyr::filter(QA_description %in% c("Cloud")) %>%
                dplyr::pull(mask_value) %>%
                sum(na.rm = TRUE)
            x %>%
                gdalcmdline::gdal_calc(out_filename = tempfile(pattern = "cloud_mask_",
                                                               fileext = ".tif"),
                                       expression = paste0("((numpy.bitwise_and(A, ",
                                                           mask_value,
                                                           " ) != 0) * 1).astype(int16)"),
                                       dstnodata = out_no_data,
                                       out_format = "GTiff",
                                       creation_option = "BIGTIFF=YES") %>%
                return()
        }))

    # Pile up images.
    brick_start_date <- brick_tb %>%
        dplyr::pull(brick_start_date) %>%
        min()
    cloud_brick <- cloud_imgs %>%
        dplyr::pull(cloud_mask) %>%
        pile_files(out_fn = file.path(out_dir,
                                      paste(brick_prefix, brick_tile,
                                            brick_start_date,
                                            "cloud_STACK_BRICK.tif", sep = '_')),
            gdal_format = "GTiff",
            no_data = out_no_data,
            gdal_options = "BIGTIFF=YES")

    return(list(image_tb = img_tb, brick_tb = brick_tb, brick_index_tb = brick_vi_tb, brick_cloud = cloud_brick))
}


#' @title Build a SITS brick of images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick of images as they are.
#'
#' @param landsat_path    A length-one character. Path to a directory of Landsat images.
#' @param modis_path      A length-one character. Path to a directory of MODIS images.
#' @param scene_shp       A length-one character. Path to a polygon shapefile of the boundaries of Landsat's scenes.
#' @param tile_shp        A length-one character. Path to a polygon shapefile of the boundaries of MODIS's scenes.
#' @param brick_scene     A length-one character. A Landsat's scene id. i.e. "225063"
#' @param brick_from      A length-one character. The first day of the brick.
#' @param brick_to        A length-one character. The last day of the brick.
#' @param brick_bands     A character. The Landsat's bands to use for building the brick.
#' @param brick_prefix    A legth-one character. Prefix for naming the bricks.
#' @param brick_path      A length-one character. A path for storing the resulting bricks.
#' @param brick_n_img     A length-one integer. The number of images to include in a brick.
#' @param gdal_options    A character. Options passed to gdal for raster creation.
#' @param gdal_format     A length-one character. Gdal format of the output file.
#' @param no_data         A length-one numeric. The value for no data.
#' @param tmp_dir         A length-one character. A path to a folder to store temporal files.
#' @return                A list of bricks of bands (tibble), bricks of vegetation indexes (tibble), and bricks of mixture model (tibble)
#' @export
build_brick_raw <- function(landsat_path, modis_path, scene_shp, tile_shp,
                            brick_scene, brick_from, brick_to, brick_bands,
                            brick_prefix, brick_path, brick_n_img,
                            gdal_options = "BIGTIFF=YES",
                            gdal_format = "GTiff", no_data = -9999,
                            tmp_dir = tempdir()){

    # Get a tibble of the images.
    brick_imgs <- build_brick_landsat_modis(landsat_path = landsat_path,
                                            modis_path = modis_path,
                                            scene_shp = scene_shp,
                                            tile_shp = tile_shp,
                                            scenes = brick_scene,
                                            from = brick_from, to = brick_to,
                                            add_neighbors = FALSE) %>%
        dplyr::mutate(year = lubridate::year(img_date)) %>%
        dplyr::slice(1:brick_n_img) %>%
        dplyr::ungroup() %>%
        ensurer::ensure_that(nrow(.) == brick_n_img,
                             err_desc = sprintf("Not enough images: %s / %s for %s %s",
                                                nrow(.), brick_n_img,
                                                brick_scene, brick_from))

    # build bricks
    brick_files <- brick_imgs %>%
        pile_up(file_col = "files",
                brick_bands = brick_bands,
                brick_prefix = brick_prefix,
                brick_scene = brick_scene,
                out_dir = brick_path,
                no_data = no_data,
                gdal_options = gdal_options)

    # build vegetation indexes bricks
    vi_brick <- compute_vi(brick_path,
        brick_pattern = paste0("^", brick_prefix, "_.*[.]tif$"),
        vi_index = c("ndvi", "evi"))

    # Build bricks using the spectral mixture model.
    mix_brick <- brick_imgs %>% build_spectral_mixture_brick(field_name = "files",
                                                             brick_prefix = brick_prefix,
                                                             brick_path = brick_path,
                                                             brick_n_img = brick_n_img,
                                                             no_data = no_data,
                                                             gdal_format = gdal_format,
                                                             gdal_options = gdal_options,
                                                             tmp_dir = tmp_dir)

    # build the cloud mask brick
    cloud_bricks <- brick_imgs %>% build_cloud_brick(field_name = "files",
                                                     brick_prefix = brick_prefix,
                                                     brick_path = brick_path,
                                                     brick_scene = brick_scene,
                                                     no_data = no_data,
                                                     gdal_format = gdal_format,
                                                     gdal_options = gdal_options,
                                                     tmp_dir = tmp_dir)

    return(list(brick_bands = brick_files, brick_index = vi_brick, brick_mixture = mix_brick, brick_cloud = cloud_bricks))
}


#' @title Build a SITS brick of cloud-masked images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick of images. The clouds in the images are masked.
#'
#' @param landsat_path    A length-one character. Path to a directory of Landsat images.
#' @param modis_path      A length-one character. Path to a directory of MODIS images.
#' @param scene_shp       A length-one character. Path to a polygon shapefile of the boundaries of Landsat's scenes.
#' @param tile_shp        A length-one character. Path to a polygon shapefile of the boundaries of MODIS's scenes.
#' @param brick_scene     A length-one character. A Landsat's scene id. i.e. "225063"
#' @param brick_from      A length-one character. The first day of the brick.
#' @param brick_to        A length-one character. The last day of the brick.
#' @param brick_bands     A character. The Landsat's bands to use for building the brick.
#' @param brick_prefix    A legth-one character. Prefix for naming the bricks.
#' @param brick_path      A length-one character. A path for storing the resulting bricks.
#' @param brick_n_img     A length-one integer. The number of images to include in a brick.
#' @param gdal_options    A character. Options passed to gdal for raster creation.
#' @param gdal_format     A length-one character. Gdal format of the output file.
#' @param no_data         A length-one numeric. The value for no data.
#' @param tmp_dir         A length-one character. A path to a folder for storing temporal files.
#' @return                A list of bricks of bands (tibble), bricks of vegetation indexes (tibble), and bricks of mixture model (tibble)
#' @export
build_brick_simple <- function(landsat_path, modis_path, scene_shp, tile_shp,
                               brick_scene, brick_from, brick_to, brick_bands,
                               brick_prefix, brick_path, brick_n_img,
                               gdal_options = "BIGTIFF=YES",
                               gdal_format = "GTiff", no_data = -9999,
                               tmp_dir = tempdir()){
    dark <- mixture <- substrate <- vegetation <- year <- NULL
    # select best images per year
    brick_imgs <- build_brick_landsat_modis(landsat_path, modis_path, scene_shp, tile_shp,
                                            scenes = brick_scene, from = brick_from,
                                            to = brick_to, add_neighbors = FALSE) %>%
        dplyr::mutate(year = lubridate::year(img_date)) %>%
        dplyr::group_by(year) %>%
        dplyr::top_n(-as.integer(brick_n_img/2), cloud_cov) %>%
        dplyr::slice(1:(as.integer(brick_n_img/2))) %>%
        dplyr::ungroup() %>%
        ensurer::ensure_that(nrow(.) == brick_n_img, err_desc = "Not enough images!")

    # build bricks
    brick_files <- brick_imgs %>% pile_up(file_col = "files",
                                          brick_bands = brick_bands,
                                          brick_prefix = brick_prefix,
                                          brick_scene = brick_scene,
                                          out_dir = brick_path,
                                          no_data = no_data,
                                          gdal_options = gdal_options)

    # build vegetation indexes bricks
    vi_brick <- compute_vi(brick_path,
        brick_pattern = paste0("^", brick_prefix, "_.*[.]tif$"),
        vi_index = c("ndvi", "savi"))

    # Build bricks using the spectral mixture model.
    mix_brick <- brick_imgs %>% build_spectral_mixture_brick(field_name = "files",
                                                             brick_prefix = brick_prefix,
                                                             brick_path = brick_path,
                                                             brick_n_img = brick_n_img,
                                                             no_data = no_data,
                                                             gdal_format = gdal_format,
                                                             gdal_options = gdal_options,
                                                             tmp_dir = tmp_dir)

    # remove
    brick_imgs %>% dplyr::pull(mixture) %>% unlist() %>% file.remove()

    # build the cloud mask brick
    cloud_bricks <- brick_imgs %>% build_cloud_brick(field_name = "files")

    return(list(brick_bands = brick_files, brick_index = vi_brick, brick_mixture = mix_brick, brick_cloud = cloud_bricks))
}


#' @title Build a SITS brick using a fusion model (StarFM) to fill in the cloud gaps.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick using a fusion odel (StarFM) to finnin the
#' gaps. To build the StarFM model and for each image, this function finds the
#' next best image using the mean MODIS-Landsat cloud cover as parameter.
#'
#' @param landsat_path    A length-one character. Path to a directory of Landsat images.
#' @param modis_path      A length-one character. Path to a directory of MODIS images.
#' @param scene_shp       A length-one character. Path to a polygon shapefile of the boundaries of Landsat's scenes.
#' @param tile_shp        A length-one character. Path to a polygon shapefile of the boundaries of MODIS's scenes.
#' @param brick_scene     A length-one character. A Landsat's scene id. i.e. "225063"
#' @param brick_year      A length-one numeric. A PRODES year.
#' @param brick_bands     A character. The Landsat's bands to use for building the brick.
#' @param brick_path      A length-one character. A path for storing the resulting bricks.
#' @param cloud_threshold DEPRECATED. A length-one numeric. The approximated proportion of clouds in the brick.
#' @param img_per_year    A length-one numeric. The number of images in a brick-year. The default is 23.
#' @param n_best_img      A length-one numeric. Use only only this number of images. Best means less cloudy.
#' @param image_step      A length-one numeric. The number of days between images.
#' @param temp_dir        A length-one character. A path to a folder to store temporal files.
#' @param no_data         A length-one numeric. The value for no data.
#' @param gdal_options    A character. Options passed to gdal for raster creation.
#' @return                A tibble.
#' @export
build_brick_starfm <- function(landsat_path, modis_path, scene_shp, tile_shp,
                        brick_scene, brick_year, brick_bands,
                        brick_path, cloud_threshold, n_best_img = 23,
                        img_per_year = 23, image_step = 16, temp_dir = NULL,
                        no_data = -9999, gdal_options = c("TILED=YES",
                                                          "COPY_SRC_OVERVIEWS=YES",
                                                          "COMPRESS=LZW")) {
    mean_cloud_cov <- rid <- tile_cloud_cov <- NULL

    if (!missing("cloud_threshold"))
        warning("Argument deprecated: cloud_threshol: cloud_threshold.")
#TODO: ???
print("11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111")
    # Assemble bricks' data
    brick_tb <- build_brick_landsat_modis(landsat_path = landsat_path,
                                          modis_path = modis_path,
                                          scene_shp = scene_shp,
                                          tile_shp = tile_shp,
                                          scenes = brick_scene,
                                          from = paste(brick_year - 1, "08-01", sep = "-"),
                                          to   = paste(brick_year,     "09-30", sep ="-"),
                                          add_neighbors = FALSE) %>%
        ensurer::ensure_that(nrow(.) > n_best_img,
                             err_desc = "Not enough images.") %>%
        dplyr::mutate(rid = dplyr::row_number())
    brick_tb$tile_cloud_cov <- vapply(brick_tb$tile,
                                      function(x){mean(x$cloud_cov, na.rm = TRUE)},
                                      numeric(1))

    # Find the best posterior of each image.
    brick_tb$next_best <- lapply(brick_tb$rid, function(row_id, tb){
           tb <- tb %>%
               dplyr::filter(rid != row_id) %>%
               dplyr::mutate(mean_cloud_cov = rowMeans(dplyr::select(., cloud_cov, tile_cloud_cov), na.rm = TRUE))
           best <- tb %>%
               dplyr::slice(row_id:nrow(.)) %>%
               dplyr::filter(rank(mean_cloud_cov) == 1)
           if (nrow(best) == 0)
               best <- tb %>% dplyr::filter(rank(mean_cloud_cov) == 1)
           return(best)
        }, tb = brick_tb)
print("222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222")
    # Constrain the number of images.
    brick_tb <- brick_tb %>%
        dplyr::slice(1:img_per_year) %>%
        dplyr::top_n(-n_best_img, cloud_cov)
print("33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333")
    # Run StarFM.
    starfm <- list()
    for(row_id in brick_tb$rid) {
        img_t0 <- brick_tb %>% dplyr::slice(row_id)
        img_t1 <- brick_tb %>% dplyr::slice(row_id) %>%
            dplyr::pull(next_best) %>%
            .[[1]]
        sfm_files <- tibble::tibble()
        for(band in brick_bands){
            starfm_res <- run_starFM(img_t0 = img_t0,
                                     img_t1 = img_t1,
                                     band = band)
            sfm_files <- sfm_files %>%
                dplyr::bind_rows(c(band = band, starfm_res))
        }
        starfm[[row_id]] <- sfm_files
    }
    brick_tb$starfm <- starfm
print("44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444")
    # Fill in the clouds using StarFM.
    brick_tb$filled <- lapply(1:nrow(brick_tb), function(x, brick_tb){
            return(fill_clouds(img = dplyr::slice(brick_tb, x)))
        }, brick_tb = brick_tb)
print("55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555")
    # Stack cloud-filled images.
    brick_tb %>% pile_up(file_col = "filled", brick_bands = brick_bands,
                         brick_prefix = "LC8SR-MOD13Q1-STARFM",
                         brick_scene = brick_scene, out_dir = brick_path,
                         no_data = no_data, gdal_options =  gdal_options) %>%
        return()
}



#' @title Helper function for building cloud bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick masking the clouds in the images.
#'
#' @param brick_imgs   A tibble with metadata of satellite imagery (see build_brick_tibble2).
#' @param field_name   A length-one character. The name of a column in brick_imgs with paths to image files.
#' @param brick_prefix A lengh-one character. Prefix to append to produced file names.
#' @param brick_path   A length-one character. A path for storing the resulting bricks.
#' @param brick_scene  A length-one character. The scene or tile of the brick.
#' @param no_data      A length-one numeric. The value for no data.
#' @param gdal_format  A length-one character. Gdal format of the output file.
#' @param gdal_options A character. Options passed to gdal for raster creation.
#' @param tmp_dir      A length-one character. A path to a folder for storing temporal files.
#' @return             A lenth-one character. Path to a brick file.
#' @importFrom rlang .data
build_cloud_brick <- function(brick_imgs, field_name, brick_prefix, brick_path,
                              brick_scene, no_data = -9999,
                              gdal_format = "GTiff", gdal_options = c("TILED=YES",
                                  "COPY_SRC_OVERVIEWS=YES", "COMPRESS=LZW"),
                              tmp_dir = tempdir()){
    cloud_mask <- NULL
    brick_imgs <- brick_imgs %>%
        dplyr::mutate(pixel_qa = purrr::map_chr(.data[[field_name]], function(x){
            fname <- NULL
            x %>% colnames() %>%
                ensurer::ensure_that("file_path" %in% ., err_desc = "Column name file_path not found.")
            x %>% dplyr::mutate(fname = basename(file_path)) %>%
                dplyr::filter(stringr::str_detect(fname, "_pixel_qa.tif$")) %>%
                ensurer::ensure_that(nrow(.) == 1, err_desc = "Error while finding the QA band of image") %>%
                dplyr::pull(file_path) %>%
                return()
        })) %>%
        dplyr::mutate(cloud_mask = purrr::map_chr(.$pixel_qa, function(x){
            x %>%
                gdalcmdline::gdal_calc(out_filename <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(x)), '_'),
                                                                tmpdir = tmp_dir,
                                                                fileext = ".tif"),
                                       expression = "((numpy.bitwise_and(A, 40) != 0) * 1).astype(int16)",
                                       dstnodata = no_data,
                                       out_format = gdal_format,
                                       creation_option = gdal_options)
        }))
    #
    img_date <- brick_imgs %>% dplyr::pull(img_date) %>% sort() %>% dplyr::first()
    cloud_brick <- brick_imgs %>% dplyr::pull(cloud_mask) %>% pile_files(
        out_fn <- file.path(brick_path, paste(brick_prefix, brick_scene, img_date, "cloud", "STACK_BRICK.tif", sep = '_')),
        gdal_format = gdal_format,
        no_data = no_data,
        gdal_options = gdal_options) %>%
        return()
}


#' @title Helper function for building spectral mixture model bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick masking the clouds in the images.
#'
#' @param brick_imgs   A tibble with metadata of satellite imagery (see build_brick_tibble2).
#' @param field_name   A length-one character. The name of a column in brick_imgs with paths to image files.
#' @param brick_prefix A lengh-one character. Prefix to append to produced file names.
#' @param brick_path   A length-one character. A path for storing the resulting bricks.
#' @param brick_n_img     A length-one integer. The number of images to include in a brick.
#' @param no_data      A length-one numeric. The value for no data.
#' @param gdal_format  A length-one character. Gdal format of the output file.
#' @param gdal_options A character.
#' @param tmp_dir      A length-one character. A path to a folder for storing temporal files.
#' @return             A tibble of metadata about the created bricks.
build_spectral_mixture_brick <- function(brick_imgs, field_name, brick_prefix,
                                         brick_path, brick_n_img,
                                         no_data = -9999, gdal_format = "GTiff",
                                         gdal_options = c("TILED=YES",
                                             "COPY_SRC_OVERVIEWS=YES",
                                             "COMPRESS=LZW"),
                                         tmp_dir = tempdir()){
    dark <- mixture <- substrate <-vegetation <- NULL

    brick_imgs <- brick_imgs %>% dplyr::mutate(mixture = purrr::map(.[[field_name]],
        function(x){
            x %>% unlist() %>% compute_mixture_model(out_dir = tmp_dir, landsat_sat = 8, no_data = no_data) %>%
                tibble::enframe(name = "end_member", value = "file_path") %>%
                tidyr::spread(key = "end_member", value = "file_path") %>%
                return()
        }))
    mix_brick <- brick_imgs %>% tidyr::unnest(mixture) %>%
        dplyr::select(dark, substrate, vegetation) %>%
        lapply(function(x, out_dir, prefix){
                   img_md <- x[1] %>% parse_img_name()
                   img_date <- format(as.POSIXct(img_md["acquisition"],
                                      format = "%Y%m%d"), "%Y-%m-%d")
                   img_band <- unlist(strsplit(img_md[length(img_md)],
                                      split = "[.]"))[1]
                   out_fn <- file.path(out_dir, paste(prefix, img_md["path_row"],
                                       img_date, img_band, "STACK_BRICK.tif", sep = '_'))
                   x %>%
                       gdalcmdline::gdal_merge(out_filename = out_fn,
                                               separate = TRUE,
                                               of = gdal_format,
                                               creation_option = gdal_options,
                                               init = no_data,
                                               a_nodata = no_data) %>%
                       return()
               }, out_dir = brick_path,
               prefix = brick_prefix) %>%
        unlist() %>%
        tibble::enframe(name = NULL) %>%
        dplyr::mutate(n_img = get_number_of_bands(value)) %>%
        ensurer::ensure_that(all(.$n_img == brick_n_img)) %>%
        return()
}


#' @title Group images into time series
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description This function takes the input tibble of images and return a
#' tibble grouped by tile (scene) and year.
#'
#' @param img_tb  A tible of images. It must have the fields tile, img_date,
#' sat_image, and prodes_year
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @param image_step        A length-one numeric. Number of days between images
#' of the same place (temporal resolution). The default is 16.
#' @return A tibble
build_ts <- function(img_tb, prodes_year_start = "-08-01", image_step = 16) {
    res <- img_tb %>% dplyr::group_by(tile, prodes_year) %>% tidyr::nest() %>%
        dplyr::mutate(ts = purrr::map(data, add_missing_dates,
                                      step = image_step,
                                      prodes_start = prodes_year_start),
                      n_img = purrr::map_int(ts, function(x) {
                          sum(!is.na(x$sat_image))
                      }), n_expected = purrr::map_int(ts, nrow),
                      max_hole = purrr::map_int(ts, max_hole)) %>%
        dplyr::select(-data)
    return(res)
}


#' @title Do a calculation on bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Do coputations on brick files using gdal_calc.
#'
#' @param in_files      A character. Paths to brick files.
#' @param gdal_exp      A length-one character. A gdal_calc expression.
#' @param out_file      A length-one character. Path to the resulting file.
#' @param out_no_data   A length-one numeric. No data value for the output.
#' @param out_data_type A length-one character. Output data type.
#' @param gdal_options  A character. Options passed to gdal.
#' @return              out_file or NA.
#' @export
compute_brick <- function(in_files, gdal_exp, out_file,
                          out_no_data = -9999, out_data_type = "Int16",
                          gdal_options = c("TILED=YES",
                                           "COPY_SRC_OVERVIEWS=YES",
                                           "COMPRESS=LZW")){

    n_bands <- in_files %>%
        ensurer::ensure_that(all(file.exists(.)),
                             err_desc = sprintf("File not found: %s",
                                                paste(in_files, collapse = " "))) %>%
        get_number_of_bands() %>%
        ensurer::ensure_that(all(. > 0),
                             err_desc = "A file is missing bands.") %>%
        ensurer::ensure_that(length(unique(.)) == 1,
                             err_desc = "Mismatch in number of bands.") %>%
        unique()

    # export bands
    # NOTE: this is a workaround regarding gdal_calc inability to process bricks.
    tmp_files <- lapply(1:n_bands, function(b_number){
        fname <- tempfile(pattern = paste0("compute_brick_", b_number, "_"),
                          fileext = ".tif")
        gdalcmdline::gdal_calc(input_files = in_files, out_filename = fname,
                               expression = gdal_exp,
                               band_number = rep(b_number, length(in_files)),
                               data_type = out_data_type,
                               dstnodata = out_no_data,
                               creation_option = gdal_options, dry_run = FALSE)
    })

    # stack bands
    tmp_files %>%
        unlist() %>%
        gdalcmdline::gdal_merge(out_filename = out_file, separate = TRUE,
                                of = "GTiff", creation_option = gdal_options,
                                init = out_no_data, a_nodata = out_no_data,
                                dry_run = FALSE)
    tmp_files %>%
        unlist() %>%
        file.remove()

    if (!file.exists(out_file))
        return(NA)
    return(out_file)
}


#' @title Compute a vegetation-index brick.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute a vegetation-index brick from the inputs.
#'
#' @param in_file  A character. Path to a brick file.
#' @param index    A length-one character. An index name.
#' @param out_file A length-one character. Path to the resulting file.
#' @param in_no_data A length-one numeric. The no data value in the inputs.
#' @param out_no_data A length-one numeric. The no data value in the output.
#' @return         out_file.
compute_brick_index <- function(in_file, index, out_file, in_no_data, out_no_data){
    # print(c(in_file, index, out_file, in_no_data, out_no_data))
    #in_file <- c("blue"  = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_band02_STACK_BRICK.tif",
    #             "red"   = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_band04_STACK_BRICK.tif",
    #             "nir"   = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_band05_STACK_BRICK.tif",
    #             "swir"  = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_band06_STACK_BRICK.tif",
    #             "swir2" = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_band07_STACK_BRICK.tif")
    #in_no_data <- out_no_data <- -9999
    #out_file <- "/home/alber/shared/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_ndvi_STACK_BRICK.tif" # tempfile(pattern = "compute_brick_index_", fileext = ".tif")
    #index = "ndvi"

    name <- NULL

    veg_index <- c("evi", "evi2", "msavi", "nbr", "nbr2", "ndmi", "ndvi", "savi")
    gdal_expression <- list()
    gdal_expression[["evi"]] <- paste0(
        "numpy.where((A != ", in_no_data, ") * (B != ", in_no_data, ") * (C != ", in_no_data, "), ",
        # A = NIR  Band05
        # B = RED  Band04
        # C = BLUE Band02
        "numpy.divide(A.astype('float64')/10000.0 - B.astype('float64')/10000.0, A.astype('float64')/10000.0 + 6 * B.astype('float64')/10000.0 - 7.5 * C.astype('float64')/10000.0 + 1, ",
        "out = numpy.full_like(A.astype('float64'), ", in_no_data, "), where = A.astype('float64') + 6 * B.astype('float64')/10000.0 - 7.5 * C.astype('float64')/10000.0 + 1 != 0) * 2.5 * 10000, ", in_no_data, ")"
        )
    gdal_expression[["evi2"]] <- paste0(
        "numpy.where((A.astype('float64') != ", in_no_data, ") * (B != ", in_no_data, "), ",
        # A = NIR  Band05
        # B = RED  Band04
        "numpy.divide(A.astype('float64') - B.astype('float64'), A.astype('float64') + 2.4 * B.astype('float64') + 10000, ",
        "out = numpy.full_like(A.astype('float64'), ", in_no_data, "), where = A.astype('float64') + 2.4 * B.astype('float64') + 10000 != 0) * 2.5 * 10000, ", in_no_data, ")"
        )
    gdal_expression[["msavi"]] <- paste0(
        "numpy.where((A != ", in_no_data, ") * (B != ", in_no_data, "), ",
        # A = NIR  Band05
        # B = RED  Band04
        "numpy.divide(2.0 * A.astype('float64')/10000.0 + 1 - numpy.sqrt(numpy.power(2.0 * A.astype('float64')/10000 + 1, 2.0) - 8 * (A.astype('float64')/10000.0 - B.astype('float64')/10000.0)), 2, ",
        "out = numpy.full_like(A.astype('float64'), ", in_no_data, "), where = (numpy.power(2.0 * A.astype('float64')/10000 + 1, 2.0) - 8 * (A.astype('float64')/10000.0 - B.astype('float64')/10000.0)) > 0) * 10000, ", in_no_data, ")"
        )
    gdal_expression[["nbr"]] <- paste0(
        "numpy.where((A != ", in_no_data, ") * (B != ", in_no_data, "), ",
        # A = NIR   Band05
        # B = SWIR2 Band07
        "numpy.divide(A.astype('float64') - B.astype('float64'), A.astype('float64') + B.astype('float64') , ",
        "out = numpy.full_like(A.astype('float64'), ", in_no_data, "), where = A != -B) * 10000.0, ", in_no_data, ")"
        )
    gdal_expression[["nbr2"]] <- gdal_expression[["nbr"]]
        # A = SWIR1 Band06
        # B = SWIR2 Band07
    gdal_expression[["ndmi"]] <- gdal_expression[["nbr"]] # ndwi
        # A = NIR   Band05
        # B = SWIR1 Band06
    gdal_expression[["ndvi"]] <- gdal_expression[["nbr"]]
        # A = NIR Band05
        # B = RED Band04
    gdal_expression[["savi"]] <- paste0(
        "numpy.where((A != ", in_no_data, ") * (B != ", in_no_data, "), ",
        # A = NIR  Band05
        # B = RED  Band04
        "numpy.divide(A.astype('float64') - B.astype('float64'), A.astype('float64') + B.astype('float64') + 5000, ",
        "out = numpy.full_like(A.astype('float64'), ", in_no_data, ")) * 1.5 * 10000, ", in_no_data, ")"
        )
    required_bands <- list()
    required_bands[["evi"]]   <- c(A = "nir", B = "red", C =  "blue")
    required_bands[["evi2"]]  <- c(A = "nir", B = "red")
    required_bands[["msavi"]] <- required_bands[["evi2"]]
    required_bands[["nbr"]]   <- c(A = "nir", B = "swir2")
    required_bands[["nbr2"]]  <- c(A = "swir", B = "swir2")
    required_bands[["ndmi"]]  <- c(A = "nir", B = "swir")
    required_bands[["ndvi"]]  <- required_bands[["evi2"]]
    required_bands[["savi"]]  <- required_bands[["evi2"]]
    index_tb <- tibble::tibble(name = veg_index,
                               gdal_expression = unlist(gdal_expression),
                               required_bands = required_bands) %>%
        dplyr::filter(name == index) %>%
        ensurer::ensure_that(nrow(.) == 1,
                             err_desc = sprintf("Wrong number of indexes: %s", nrow(.))) %>%
        ensurer::ensure_that(all(.$required_bands[[1]] %in% names(in_file)),
                             err_desc = sprintf("Missing bands: %s requires %s but instead got %s. The available names are %s",
                                 index, paste(sort(.$required_bands[[1]]),
                                              collapse = ", "),
                                        paste(sort(names(in_file)),
                                              collapse = ", "),
                                        paste(sort(unique(unlist(required_bands, recursive = TRUE))),
                                              collapse = ", ")))

     compute_brick(in_files = in_file[sort(index_tb$required_bands[[1]])],
                   gdal_exp = index_tb$gdal_expression[[1]],
                   out_file = out_file,
                   out_no_data = out_no_data,
                   out_data_type = "Int16") %>%
        return()
}


#' @title Compute a vegetation-index brick.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute a vegetation-index brick from the inputs.
#'
#' @param brick_A         A length-one character. Path to a brick file.
#' @param brick_B         A length-one character. Path to a brick file.
#' @param vi_exp          A length-one character. A gdal_calc vaid expression.
#' @param vi_filename     A length-one character. Path to the resulting file.
#' @param gdal_options    A character. Options passed to gdal.
#' @param brick_data_type A length-one character. The data type of the resulting brick.
#' @param no_data         A length-one numeric. The no data value for the output.
#' @return            vi_filename.
compute_brick_vi <- function(brick_A, brick_B, vi_exp, vi_filename,
                             gdal_options = c("TILED=YES",
                                              "COPY_SRC_OVERVIEWS=YES",
                                              "COMPRESS=LZW"),
                             brick_data_type = "Int16", no_data = -9999){
   .Deprecated("compute_brick_index")

    # ndvi_exp <- paste0("numpy.where(A != ", no_data, ", numpy.divide(A.astype(float64) - B.astype(float64) , A.astype(float64) + B.astype(float64) + 0.0001, out = numpy.full_like(A.astype(float64), ", no_data, "), where = A != -B) * 10000, A)")
    # savi_exp <- paste0("numpy.where((A != ", no_data, ") * (B != ", no_data, "), ((A.astype(float64) - B.astype(float64)) / (A.astype(float64) + B.astype(float64) + 5000.0001)) * 1.5 * 10000, ", no_data, ")")
    n_bands <- get_number_of_bands(brick_A)
    if (n_bands != get_number_of_bands(brick_B)) {
        warning(sprintf("Missmatching number of bands between %s and %s", brick_A, brick_B))
        return(NA)
    }
    # export brick's bands
    # NOTE: this is a workaround regarding gdal_calc inability to process bricks.
    vi_tmp <- lapply(1:n_bands, function(b_number){
        fname <- tempfile(pattern = stringr::str_replace(basename(vi_filename),
                                                         ".tif",
                                                         paste0("_", b_number, "_")),
                          fileext = ".tif")
        gdalcmdline::gdal_calc(input_files = c(brick_A, brick_B),
                               out_filename = fname, expression = vi_exp,
                               band_number = rep(b_number, 2),
                               data_type = brick_data_type, dstnodata = no_data,
                               dry_run = FALSE)
    })
    # stack the bands
    vi_tmp %>%
        unlist() %>%
        gdal_merge(out_filename = vi_filename, separate = TRUE,
                   of = "GTiff", creation_option = gdal_options,
                   init = no_data, a_nodata = no_data, dry_run = FALSE)
                   #nodata_value = no_data

    # cleaning
    file.remove(unlist(vi_tmp))
    return(vi_filename)
}


#' @title Compute a mixture model.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute vegetation indexes from bricks.
#'
#' @param file_paths   A character. Paths to files of a Landsat image.
#' @param out_dir      A length-one character. Path to store the results.
#' @param landsat_sat  A length-one character. The landsat satelite denomination e.g. '8'.
#' @param no_data      A length-one numeric. The value for no data.
#' @return             A character. Paths to the End member files.
#' @export
compute_mixture_model <- function(file_paths, out_dir, landsat_sat = '8', no_data = -9999){
    # get misture model coefficients
    coef_tb <- END_MEMBERS_LANDSAT_8
    if (landsat_sat == '7') {
        coef_tb <- END_MEMBERS_LANDSAT_7
    }

    # build tibble of parameters
    model_tb <- file_paths %>% tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(band = get_landsat_band(file_path)) %>%
        dplyr::filter(band %in% coef_tb$band) %>%
        dplyr::right_join(coef_tb, by = "band") %>%
        dplyr::arrange(band) %>%
        ensurer::ensure_that(sum(is.na(file_paths)) == 0,
                             err_desc = "Not enough band files!") %>%
        ensurer::ensure_that(nrow(.) == nrow(coef_tb),
                             err_desc = "File-band missmatch!")

    # build file names for resulting files
    member_names <- names(coef_tb)[-(1:2)]
    dummy_name <- model_tb %>% dplyr::filter(band == "sr_band2") %>%
        dplyr::pull(file_path) %>% basename() %>% stringr::str_split('_') %>%
        unlist() %>% .[1:8]
    out_fnames <- file.path(out_dir, paste(paste(dummy_name, collapse = "_"),
                                           paste0(member_names, ".tif"), sep = '_'))
    names(out_fnames) <- member_names

    img_md <- get_landsat_metadata(model_tb$file_path[1])
    vapply(member_names, function(member){
        # util function
        exp_nodata <- function(letter){
            if(length(letter) == 1)
                return(paste("(", letter, "!=", no_data, ")", sep = " ",collapse = " * "))
            else(length(letter) > 1)
            return(paste(vapply(letter, exp_nodata, character(1)), collapse = " * "))
        }

        gcalc_exp <- paste0("numpy.where(", exp_nodata(LETTERS[1:nrow(coef_tb)]), ", ",
                            paste(paste(LETTERS[1:nrow(coef_tb)], "*", model_tb[[member]]), collapse = " + "),
                            ", -9999)")
        model_tb$file_path %>%
            gdalcmdline::gdal_calc(out_filename = out_fnames[member],
                                   expression = gcalc_exp,
                                   dstnodata = img_md$srcnodata_l8)
    }, character(1)) %>%
        return()
}


#' @title Compute vegetation indexes from bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute vegetation indexes from bricks.
#'
#' @param brick_path    A length-one character. Path to bricks.
#' @param brick_pattern A length-one character. Regular expression used to filter the files in brick_path.
#' @param vi_index      A character. The indexes to be computed.
#' @param no_data       A length-one numeric. The value for no data.
#' @param gdal_options  A character. Options passed to gdal for raster creation.
#' @return              A character. Path to the created vegetation index bricks.
#' @export
compute_vi <- function(brick_path, brick_pattern = "^brick_.*[.]tif$",
                       vi_index = c("ndvi", "savi"), no_data = -9999,
                       gdal_options = c("TILED=YES",
                                        "COPY_SRC_OVERVIEWS=YES",
                                        "COMPRESS=LZW")){
    .Deprecated("compute_brick_index")
    # Get the path to a file from a brick tibble
    # @param brick_tb A tibble of brick metadata. It must contain the fields c("files", "band", "file_path")
    # @param x        A length-one numeric. A row index in brick_tb
    # @param aband    A length-one character. The name of a band
    # @return         A character. A path to a file
    get_path <- function(brick_tb, x, aband){
        brick_tb %>%
            dplyr::slice(x) %>%
            dplyr::pull(files) %>%
            dplyr::bind_rows() %>%
            dplyr::filter(band == aband) %>%
            dplyr::pull(file_path) %>%
            dplyr::first() %>%
            return()
    }


    brick_tb <- brick_path %>%
        list.files(pattern = brick_pattern, full.names = TRUE) %>%
        ensurer::ensure_that(length(.) > 0, err_desc = "No brick files were found!") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(scene = stringr::str_extract(basename(file_path), "[0-9]{6}"),
                      first_date = as.Date(stringr::str_extract(basename(file_path), "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
                      band = get_landsat_band(file_path, band_name = "short_name")) %>%
        tidyr::nest(file_path, band, .key = "files")

    ndvi_exp <- paste0("numpy.where(A != ", no_data, ", numpy.divide(A.astype(float64) - B.astype(float64) , A.astype(float64) + B.astype(float64) + 0.0001, out = numpy.full_like(A.astype(float64), ", no_data, "), where = A != -B) * 10000, A)")
    savi_exp <- paste0("numpy.where((A != ", no_data, ") * (B != ", no_data, "), ((A.astype(float64) - B.astype(float64)) / (A.astype(float64) + B.astype(float64) + 5000.0001)) * 1.5 * 10000, ", no_data, ")")

    vi_paths <- lapply(1:nrow(brick_tb), function(b_index){
        b4 <- brick_tb %>% get_path(b_index, "red")
        b5 <- brick_tb %>% get_path(b_index, "nir")
        ndvi_filename <- NA
        savi_filename <- NA
        if (all(is.na(b5), is.na(b4))) {
            warning("No vegetation indexes was computed because of missing bricks.")
        }else{
            if ("ndvi" %in% vi_index) {
                vi_filename <- b4 %>% stringr::str_replace("_red_", "_ndvi_")
                ndvi_filename <- compute_brick_vi(b5, b4, ndvi_exp, vi_filename, gdal_options = gdal_options)
            }else if ("savi" %in% vi_index) {
                vi_filename <- b4 %>% stringr::str_replace("_red_", "_savi_")
                savi_filename <- compute_brick_vi(b5, b4, savi_exp, vi_filename, gdal_options = gdal_options)
            }
        }
        return(c(ndvi = ndvi_filename, savi = savi_filename))
    })
    return(vi_paths)
}


#' @title Get a time series from a brick.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Find the tiled images closest to a given date.
#'
#' @param brick_path A length-one character. Path to a brick file
#' @param pix_x      A length-one numeric. A pixel's column index
#' @param pix_y      A length-one numeric. A pixel's row index
#' @return           A numeric
#' @export
get_brick_ts <- function(brick_path, pix_x, pix_y){
    cmd <- paste("gdallocationinfo", brick_path, pix_x, pix_y)
    system(cmd, intern = TRUE) %>%
        .[gtools::even(seq_along(.))] %>% .[2:length(.)] %>% strsplit(" ") %>%
        lapply(dplyr::last) %>% unlist() %>% as.numeric() %>%
        return()
}


