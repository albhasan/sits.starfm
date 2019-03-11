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


#' @title Build a SITS brick using a fusion model (StarFM) to fill in the cloud gaps
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a SITS brick using a fusion odel (StarFM) to finnin the gaps.
#'
#' @param landsat_path A length-one character. Path to a direcotry of Landsat images.
#' @param modis_path   A length-one character. Path to a direcotry of MODIS images.
#' @param scene_shp    A length-one character. Path to a polygon shapefile of the boundaries of Landsat's scenes.
#' @param tile_shp     A length-one character. Path to a polygon shapefile of the boundaries of MODIS's scenes.
#' @param brick_scene  A length-one character. A Landsat's scene id. i.e. "225063"
#' @param brick_year   A length-one numeric. A PRODES year.
#' @param brick_bands  A character. The Landsat's bands to use for building the brick.
#' @param brick_path   A length-one character. A path to store the resulting bricks.
#' @param cloud_threshold A length-one numeric. The approximated proportion of clouds in the brick.
#' @param img_per_year A length-one numeric. The number of images in a brick-year. The default is 23.
#' @param n_best_img   A length-one numeric. Use only only this number of images. Best means less cloudy.
#' @param image_step   A length-one numeric. The number of days between images.
#' @param temp_dir     A length-one character. A path to a folder to store temporal files.
#' @param no_data      A length-one numeric. The value for no data.
#' @return             A tibble
#' @export
build_brick <- function(landsat_path, modis_path, scene_shp, tile_shp,
                        brick_scene, brick_year, brick_bands,
                        brick_path, cloud_threshold, n_best_img = 23,
                        img_per_year = 23, image_step = 16, temp_dir = NULL, 
                        no_data = -9999) {

    input_vec <- c(landsat_path = landsat_path,
                   modis_path = modis_path,
                   scene_shp = scene_shp,
                   tile_shp = tile_shp,
                   brick_scene = brick_scene,
                   brick_year = brick_year,
                   brick_bands = paste(brick_bands, collapse = ","),
                   brick_path = brick_path,
                   cloud_threshold = cloud_threshold,
                   n_best_img = n_best_img,
                   img_per_year = img_per_year,
                   image_step = image_step,
                   temp_dir = temp_dir)
    log4r::info(logger, c("Brick arguments...",
                          paste(names(input_vec), input_vec, sep = " = ")))

    log4r::info(logger, "Validating inputs...")
    if (is.null(temp_dir))
        temp_dir <- tempdir()
    tmp_dir <- temp_dir %>%
        file.path(paste("L8MOD", brick_scene, brick_year, sep = "_"))
    starfm_dir <- temp_dir %>%
        file.path(paste("starfm", brick_scene, brick_year, sep = "_"))
    if (!dir.exists(tmp_dir))
        tmp_dir %>% dir.create()
    if (!dir.exists(starfm_dir))
        starfm_dir %>% dir.create()

    log4r::info(logger, "Assembling bricks' data...")
    brick_tb <- suppressWarnings(
        build_brick_tibble(landsat_path = landsat_path,
                           modis_path = modis_path, scene_shp = scene_shp, tile_shp = tile_shp,
                           scenes = brick_scene, from = paste(brick_year - 1, "08-01", sep = "-"),
                           to = paste(brick_year, "09-30", sep = "-"), max_ts_hole = 1,
                           min_miss_ratio = 0.95)
    )

    log4r::info(logger, "Get brick's images...")
    brick_imgs <- brick_tb %>%
        dplyr::group_by(scene, prodes_year) %>%
        tidyr::nest(.key = "ts") %>%
        dplyr::filter(scene %in% brick_scene, prodes_year %in% brick_year) %>%
        dplyr::pull(ts) %>% dplyr::bind_rows() %>% dplyr::arrange(img_date) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc =
                                 sprintf("Unable to build a brick for %s - %s",
                                         brick_scene, brick_year)) %>%
        dplyr::mutate(scene = substr(sat_image, 11, 16))

    log4r::info(logger, "Add extra images...")
    br_img_last_date <- brick_imgs %>% dplyr::slice(nrow(.)) %>%
        dplyr::pull(img_date)
    brick_imgs <- brick_tb %>%
        dplyr::filter(scene %in% brick_scene,
                      img_date %in% (br_img_last_date + image_step)) %>%
        dplyr::select(img_date, sat_image) %>% dplyr::bind_rows(brick_imgs) %>%
        dplyr::arrange(img_date)
    if (nrow(brick_imgs) < img_per_year) {
        stop(sprintf("Not enough images for building a brick: %s / %s", nrow(brick_imgs), img_per_year))
    }

    log4r::info(logger, "Find the next best image to compute StarFM...")
    brick_imgs$next_best <- suppressWarnings(
        lapply(1:nrow(brick_imgs), function(x, brick_imgs, cloud_threshold) {
            brick_imgs %>% get_next_image(ref_row_number = x,
                                          cloud_threshold = cloud_threshold) %>%
                return()
        }, brick_imgs = brick_imgs, cloud_threshold = cloud_threshold))

    log4r::info(logger, "Remove extra images...")
    brick_imgs <- brick_imgs %>% dplyr::slice(1:img_per_year)

    log4r::info(logger, "Iterating over images...")
    sfm_list <- list()
    for (row_n in 1:nrow(brick_imgs)) {
        log4r::info(logger, sprintf("    Row %s - Getting images t0 and t1", row_n))

        # store the results
        sfm_files <- tibble::tibble(starfm = character(), t1_fine = character(),
                                    t1_coarse = character(),
                                    t0_fine = character(),
                                    t0_coarse = character(),
                                    sfm_conf = character())

        img0 <- brick_imgs %>% dplyr::slice(row_n)
        img1 <- brick_imgs %>% dplyr::slice(row_n) %>% dplyr::pull(next_best)

        # validation
        if (all(is.na(img1)) || is.na(dplyr::bind_rows(img1)$sat_image)) {
            log4r::warn(logger, sprintf("    Row %s - No suitable t1 image for
                                        scene %s on %s", row_n, brick_scene,
                                        img0$img_date))
            warning(sprintf("No suitable Landsat image was found for computing
                            the fusion model for scene %s on %s", brick_scene,
                            img0$img_date))
            sfm_list[[length(sfm_list) + 1]] <- sfm_files
            next()
        }
        img1 <- img1 %>% dplyr::bind_rows()

        # NOTE: Is the mean representative of the MODIS coverage of a Landsat image?
        mod_cloud_cover <- img0 %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>%
            dplyr::pull(cloud_cov) %>% mean()

        if (mod_cloud_cover > cloud_threshold) {
            log4r::warn(logger, sprintf("    Row %s - Cloudy MODIS images for
                                        scene %s on %s", row_n, brick_scene,
                                        img0$img_date))
            warning(sprintf("MODIS images are too cloudy for scene %s on %s",
                            brick_scene, img0$img_date))
            sfm_list[[length(sfm_list) + 1]] <- sfm_files
            next()
        }
        if (is.na(img0$sat_image)) {
            log4r::warn(logger, sprintf("    Row %s - Landsat image t0 is
                                        missing (scene %s, date %s)", row_n,
                                        brick_scene, img0$img_date))
            warning(sprintf("Landsat Image (scene %s and date %s) is missing!",
                            brick_scene, img0$img_date))
        }

        # run the fusion ----
        for (band in brick_bands) {
            log4r::info(logger, sprintf("    Row %s - Processing band %s",
                                        row_n, band))
            out_filename = file.path(starfm_dir,
                                     stringr::str_c(
                                         stringr::str_c("starfm", brick_scene,
                                                        img0$img_date, band,
                                                        sep = "_"), ".bin"))
            log4r::debug(logger, paste("StarFM file name: ", out_filename))
            log4r::info(logger, sprintf("    Row %s - Running StarFM...", row_n))

            starfm_res <- run_starFM(img0_f = img0, img1_f = img1, band = band,
                                     out_filename = out_filename,
                                     tmp_dir = tmp_dir)

            log4r::debug(logger, sprintf("    Row %s - Done running StarFM...",
                                         row_n))
            sfm_files <- sfm_files %>%
                dplyr::add_row(starfm = starfm_res["starfm"],
                               t1_fine = starfm_res["t1_fine"],
                               t1_coarse = starfm_res["t1_coarse"],
                               t0_fine = starfm_res["t0_fine"],
                               t0_coarse = starfm_res["t0_coarse"],
                               sfm_conf = starfm_res["sfm_conf"])
        }
        log4r::info(logger, sfm_files)
        sfm_list[[length(sfm_list) + 1]] <- sfm_files
    }
    if (length(sfm_list) != img_per_year) {
        log4r::error("Missing fusion images!")
        log4r::info("Logging sfm_list...")
        log4r::info(sfm_list)
        brick_imgs_fp <- file.path(starfm_dir,
                                   paste0(paste("brick_imgs", brick_scene,
                                                brick_year, sep = "_"),
                                          ".Rdata"))
        log4r::info(paste("Saving brick_imgs to", brick_imgs_fp))
        save(brick_imgs, file = brick_imgs_fp)
        stop("Missing fusion images!")
    }
    brick_imgs$starfm <- sfm_list
    log4r::info(logger, "Finished iterating")

    log4r::info(logger, "Filling in the clouds...")
    brick_imgs$filled <- lapply(1:nrow(brick_imgs), function(x, brick_imgs) {
        fill_clouds(img = dplyr::slice(brick_imgs, x), tmp_dir = tmp_dir)
    }, brick_imgs = brick_imgs)
    log4r::debug(logger, paste(c("Filled images...", unlist(brick_imgs$filled))))

    log4r::info(logger, "Stacking up filled images...")
    filled_tb <- brick_imgs %>% tidyr::unnest(filled) %>%
        dplyr::mutate(band = get_landsat_band(filled))

    first_img_date <- brick_imgs %>% dplyr::slice(1) %>%
        dplyr::pull(img_date) %>% dplyr::first()

    brick_path <- lapply(brick_bands, function(x, filled_tb, brick_path,
                                               first_img_date){

        band_short_name <- SPECS_L8_SR %>% dplyr::filter(band_designation == x) %>%
            dplyr::pull(short_name) %>% dplyr::first() %>%
            stringr::str_replace(" ", "_") %>% tolower()
        out_fn <- file.path(brick_path,
                            paste0(paste("LC8SR-MOD13Q1-STARFM", brick_scene,
                                         first_img_date, band_short_name,
                                         "STACK_BRICK", sep = "_"), ".tif" ))

        # NOTE: use filled_tb$filled because dplyr::pull drops the NAs and we don't know where to repeat images!!!!!!!!!!!!
        paths <- filled_tb %>% dplyr::filter(band %in% c(x, NA)) %>%
            .$filled %>% zoo::na.locf() %>%
            gdal_merge(out_filename = out_fn, separate = TRUE, of = "GTiff",
                       creation_option = "BIGTIFF=YES", init = no_data,
                       a_nodata = no_data)
        return(paths)
    }, filled_tb = filled_tb, brick_path = brick_path, first_img_date = first_img_date)
    #save(brick_path, file = file.path(tmp_dir, "brick_path.Rdata"))

    brick_imgs_fp <- file.path(starfm_dir,
                               paste0(paste("brick_imgs", brick_scene,
                                            brick_year, sep = "_"), ".Rdata"))
    save(brick_imgs, file = brick_imgs_fp)

    log4r::info(logger, sprintf("Finished scene %s year %s", brick_scene, brick_year))
    return(brick_imgs)
}


#' @title Build a tibble with the data required to create bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a tibble with the data required to create bricks.
#'
#' @param landsat_path A length-one character. Path to a directory of images.
#' @param modis_path   A length-one character. Path to a directory of images.
#' @param scene_shp    A length-one character. Path to a polygon shapefile of
#' Landsat scene borders.
#' @param tile_shp     A length-one character. Path to a polygon shapefile of
#' MODIS tile borders.
#' @param scenes       A character. Constrain to these scenes (e.g. 233067)
#' @param from         A character. Constrain to this starting date.
#' @param to           A character. Constrain to this ending date.
#' @param max_ts_hole  A length-one numeric. Maximum number of missing
#' consecutive images allowed in a time series
#' @param min_miss_ratio A length-one numeric. Minimum proportion of missing
#' images allowed in a time series.
#' @return             A tibble.
#' @export
build_brick_tibble <- function(landsat_path, modis_path, scene_shp, tile_shp,
                               scenes = NULL, from = NULL, to = NULL,
                               max_ts_hole = 1, min_miss_ratio = 0.95){

    if (!all(dir.exists(landsat_path), dir.exists(modis_path))) {
        stop("Directory not found!")
    }
    if (!all(file.exists(scene_shp), file.exists(tile_shp))) {
        stop("File not found!")
    }

    # Get files into a tibble & filter
    pattern_landsat <- NULL
    if (!is.null(scenes) && is.vector(scenes) && length(scenes) > 0) {
        scene_neigh <- scenes %>% get_tile_neighbors() %>% c(scenes) %>%
            unique() %>% .[!is.na(.)]
        if (length(scene_neigh) > 1) {
            scene_neigh <- paste0('(', paste(scene_neigh, collapse = '|'), ')')
        }
        pattern_landsat <- stringr::str_c("^LC08_L1[TG][PTS]_", scene_neigh,
                                          ".*(tif|_MTL\\.txt)$")
    }
    l8_img <- landsat_path %>% build_landsat_tibble(pattern = pattern_landsat,
                                                    from = from, to = to)

    # check brick completeness & spot missing images
    l8_img <- l8_img %>% build_ts() %>%
        dplyr::mutate(brick_ratio = n_img/n_expected) %>%
        dplyr::filter(brick_ratio > min_miss_ratio, max_hole <= max_ts_hole) %>%
        dplyr::select(tile, prodes_year, ts) %>% tidyr::unnest(ts) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = sprintf(
            "Not enough images to to build a brick for %s from %s to %s",
            scenes, from, to)) %>%
        dplyr::select(sat_image, files, tile, img_date, prodes_year, neigh,
                      cloud_cov)

    # match L8 scenes to MOD tiles (in space)
    l8mod_sp <- match_tiles2scenes(scene_path = scene_shp,
                                   tile_path = tile_shp,
                                   scenes = unique(dplyr::pull(l8_img, tile)))
    l8mod_sp$tile <- lapply(l8mod_sp$tile, function(x) {dplyr::pull(x, tile)})
    l8_img <- l8_img %>% dplyr::rename(scene = tile) %>%
        dplyr::inner_join(l8mod_sp, by = 'scene')

    # match L8 scenes to MOD tiles (in time)
    tiles <- l8_img %>% dplyr::pull(tile) %>% unlist() %>% unique()
    if (length(tiles) == 0) {
        return(NA)
    }else if (length(tiles) > 1) {
        tiles <- paste0('(', paste(tiles, collapse = '|'), ')')
    }
    pattern_mod <- stringr::str_c('^MOD13Q1\\.A201[2-8][0-9]{3}\\.', tiles,
                                  '.*hdf$')
    mod_img <- modis_path %>% build_modis_tibble(pattern = pattern_mod,
                                                 from = from, to = to)
    l8_img$tile <- purrr::map2(l8_img$tile, l8_img$img_date, match_tile_date,
                               img_tb = mod_img, untie = 0.001)
    #mod_imgs <- l8_img %>% dplyr::pull(tile) %>% dplyr::bind_rows() %>%
    #    dplyr::pull(sat_image)
    #if (length(mod_imgs) != length(unique(mod_imgs)))
    #    warning("Duplicated MODIS were related to LANDSAT images")
    return(l8_img)
}


#' @title Build a tibble with the data required to create bricks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a tibble with the data required to create bricks.
#'
#' @param landsat_path A length-one character. Path to a directory of images.
#' @param modis_path   A length-one character. Path to a directory of images.
#' @param scene_shp    A length-one character. Path to a polygon shapefile of
#' Landsat scene borders.
#' @param tile_shp     A length-one character. Path to a polygon shapefile of
#' MODIS tile borders.
#' @param scenes       A character. Constrain to these scenes (e.g. 233067)
#' @param from         A character. Constrain to this starting date.
#' @param to           A character. Constrain to this ending date.
#' @param add_neighbors A logical. Should neighbor images be considered?
#' @return             A tibble.
#' @export
build_brick_tibble2 <- function(landsat_path, modis_path, scene_shp, tile_shp,
                                scenes, from, to, add_neighbors){

    if (!all(vapply(c(landsat_path, modis_path), dir.exists, logical(1)))) {
        stop("Directory not found!")
    }
    if (!all(vapply(c(scene_shp, tile_shp), file.exists, logical(1)))) {
        stop("File not found!")
    }

    # Get files into a tibble 
    pattern_landsat <- NULL
    if (!is.null(scenes) && is.vector(scenes) && length(scenes) > 0) {
        scene_neigh <- scenes
        if (add_neighbors) {
            scene_neigh <- scenes %>% get_tile_neighbors() %>% c(scenes) %>%
                unique() %>% .[!is.na(.)]
        }
        if (length(scene_neigh) > 1) {
            scene_neigh <- paste0('(', paste(scene_neigh, collapse = '|'), ')')
        }
        pattern_landsat <- stringr::str_c("^LC08_L1[TG][PTS]_", scene_neigh,
                                          ".*(tif|_MTL\\.txt)$")
    }
    l8_img <- landsat_path %>% build_landsat_tibble(pattern = pattern_landsat,
                                                    from = from, to = to)

    # match L8 scenes to MOD tiles (in space)
    l8mod_sp <- match_tiles2scenes(scene_path = scene_shp,
                                   tile_path = tile_shp,
                                   scenes = unique(dplyr::pull(l8_img, tile))) %>%
                    dplyr::mutate(tile = purrr::map(tile, function(x) {dplyr::pull(x, tile)}))
    l8_img <- l8_img %>% dplyr::rename(scene = tile) %>%
        dplyr::inner_join(l8mod_sp, by = 'scene')

    # match L8 scenes to MOD tiles (in time)
    tiles <- l8_img %>% dplyr::pull(tile) %>% unlist() %>% unique()
    if (length(tiles) == 0) {
        return(NA)
    }else if (length(tiles) > 1) {
        tiles <- paste0('(', paste(tiles, collapse = '|'), ')')
    }
    pattern_mod <- stringr::str_c('^MOD13Q1\\.A201[2-8][0-9]{3}\\.', tiles,
                                  '.*hdf$')
    mod_img <- modis_path %>% build_modis_tibble(pattern = pattern_mod,
                                                 from = from, to = to)
    l8_img <- l8_img %>% dplyr::mutate(tile = purrr::map2(l8_img$tile, 
                                           l8_img$img_date, match_tile_date,
                                           img_tb = mod_img, untie = 0.001))

    return(l8_img)
}


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
        print(c(command, sapply(args, paste, collapse = " ")))
        return(0)
    }
    system2(command = command, args = args, stdout = stdout, stderr = stderr)
}


#' @title Compute a misture model. 
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
        model_tb$file_path %>% gdal_calc(out_filename = out_fnames[member], 
            expression = gcalc_exp, dstnodata = img_md$srcnodata_l8)
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
#' @return              A character. Path to the created vegetation index bricks.
#' @export
compute_vi <- function(brick_path, brick_pattern = "^brick_.*[.]tif$",
                       vi_index = c("ndvi", "savi"), no_data = -9999){

    # Get the path to a file from a brick tibble
    # @param brick_tb A tibble of brick metadata. It must contain the fields c("files", "band", "file_path")
    # @param x        A length-one numeric. A row index in brick_tb
    # @param aband    A length-one character. The name of a band
    # @return         A character. A path to a file
    get_path <- function(brick_tb, x, aband){
        brick_tb %>% dplyr::slice(x) %>% dplyr::pull(files) %>%
            dplyr::bind_rows() %>% dplyr::filter(band == aband) %>%
            dplyr::pull(file_path) %>% dplyr::first() %>% return()
    }


    # Compute a vegetation index from the input bricks
    # NOTE: this is a workaround regarding gdal_calc inability to process bricks
    # @param brick_A     A length-one character. Path to a brick file
    # @param brick_B     A length-one character. Path to a brick file
    # @param vi_exp      A length-one character. A gdal_calc vaid expression
    # @param vi_filename A length-one character. Path to the resulting file
    # @return vi_filename
    compute_brick_vi <- function(brick_A, brick_B, vi_exp, vi_filename){
        n_bands <- get_number_of_bands(brick_A)
        if (n_bands != get_number_of_bands(brick_B)) {
            warning(sprintf("Missmatching number of bands between %s and %s", brick_A, brick_B))
            return(NA)
        }
        # export brick's bands
        vi_tmp <- lapply(1:n_bands, function(b_number){
            fname <- vi_filename %>%
                stringr::str_replace(".tif", paste0("_tmp_", b_number, ".tif"))
            gdal_calc(input_files = c(brick_A, brick_B),
                      out_filename = fname,
                      expression = vi_exp,
                      band_number = rep(b_number, 2),
                      data_type = "Int16",
                      dry_run = FALSE)
        })
        # stack the bands
        vi_tmp %>% unlist() %>%
            gdal_merge(out_filename = vi_filename, separate = TRUE,
                       of = "GTiff", creation_option = "BIGTIFF=YES",
                       init = no_data, a_nodata = no_data)
        # cleaning
        file.remove(unlist(vi_tmp))
        return(vi_filename)
    }

    brick_tb <- brick_path %>% list.files(pattern = brick_pattern, full.names = TRUE) %>%
        ensurer::ensure_that(length(.) > 0, err_desc = "No brick files were found!") %>%
        dplyr::as_tibble() %>% dplyr::rename(file_path = value) %>%
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
                ndvi_filename <- compute_brick_vi(b5, b4, ndvi_exp, vi_filename)
            }else if ("savi" %in% vi_index) {
                vi_filename <- b4 %>% stringr::str_replace("_red_", "_savi_")
                savi_filename <- compute_brick_vi(b5, b4, savi_exp, vi_filename)
            }
        }
        return(c(ndvi = ndvi_filename, savi = savi_filename))
    })
    return(vi_paths)
}


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


#' @title Get the full MOD13Q1 name from a landsat file.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the full MOD13Q1 name from a landsat file
#'
#' @param path_modis A character. Path to a MOD13Q1 file.
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
            dplyr::filter(row_number() == 1)
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


#' @title Get the 8 neighbors of a Landsat or MODIS tile.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the 8 neightbors of a Landsat or MODIS tile.
#'
#' @param tile  A character. The tile.
#' @return A character or a character matrix.
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
mask_negatives <- function(img, bands, replacement_value, out_dir, param, tmp_dir = tempdir()){
        img %>% dplyr::pull(files) %>% dplyr::bind_rows() %>%
            dplyr::mutate(band = get_landsat_band(file_path)) %>%
            dplyr::mutate(masked = purrr::map_chr(1:nrow(.),
                function(x, file_tb, bands){
                    row_x <- file_tb %>% dplyr::slice(x) %>%
                        dplyr::filter(band %in% bands)
                    if(nrow(row_x) != 1) return(NA_character_)
                    row_x %>% dplyr::select(file_path) %>% unlist() %>%
                        gdal_calc(
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
mask_clouds <- function(img, bands, replacement_value, out_dir, param, tmp_dir = tempdir()){
    pixel_qa <- img %>% dplyr::pull(files) %>% unlist() %>%
        stringr::str_subset(pattern = "pixel_qa")
    param <- param %>% append(get_landsat_metadata(pixel_qa[1]))

    # build the cloud mask
    img_mask <- pixel_qa %>%
        gdal_calc(
            out_filename =
                file.path(tmp_dir, paste0(paste("cloud_mask", img$sat_image, 
                                       sep = "_"), param[["fileext"]])),
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
                        gdal_calc(
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


#' @title Pile images into files by band.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Pile images into files by band.
#'
#' @param brick_imgs   A tibble of images. 
#' @param file_col     Name of the column in brick_imgs with the paths to the images to pile up.
#' @param brick_bands  A character. Names of the bands.
#' @param brick_prefix A lengh-one character. Prefix to append to produced file names.
#' @param brick_scene  A length-one character. The scene or tile of the brick.
#' @param out_dir      A character. Path to a directory for storing results.
#' @param no_data      A length-one numeric. The value for no data.
#' @return             A tibble of the matching PRODES dates and NA for the
#' @export
pile_up <- function(brick_imgs, file_col, brick_bands, brick_prefix, brick_scene, out_dir, no_data = -9999){

    # helper function to pile up the files of a single band
    helper_pile_band <- function(i_tb){
        band_short_name <- SPECS_L8_SR %>% 
            dplyr::filter(band_designation == unique(i_tb$band)) %>%
            dplyr::pull(short_name) %>% dplyr::first() %>%
            stringr::str_replace(" ", "_") %>% tolower()
        out_fn <- file.path(out_dir,
                            paste0(paste(brick_prefix, brick_scene,
                                         first_date, band_short_name,
                                         "STACK_BRICK", sep = "_"), ".tif"))
        i_tb %>% dplyr::pull(file_path) %>% unlist() %>%
            gdal_merge(out_filename = out_fn, separate = TRUE, of = "GTiff",
                       creation_option = "BIGTIFF=YES", init = no_data,
                       a_nodata = no_data) %>%
            tibble::enframe(name = NULL) %>%
            return()
    }

    first_date <- brick_imgs %>% dplyr::pull(img_date) %>% unlist() %>% min() 
    img_tb <- brick_imgs %>% tidyr::unnest(.data[[file_col]]) %>%
        dplyr::mutate(band = get_landsat_band(.data[["file_path"]])) %>% 
        dplyr::filter(band %in% brick_bands)
    img_tb %>% dplyr::group_by(band) %>% 
        dplyr::do(helper_pile_band(.data)) %>% dplyr::ungroup() %>%
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
