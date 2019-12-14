# BUILD TIBBLE OF METADATA OF IMAGES.

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
        # Landsat collection 1
        if(stringr::str_detect(file_name, pattern = "^L[CTM]0[4-9]_L[0-3][A-Z]{2}_[0-9]{6}_[0-9]{8}_[0-9]{8}_[0-9]{2}_[A-Z]([0-9]|[A-Z])")){
            res <- unlist(stringr::str_split(file_name, "_"))
            names(res) <- c("header", "level", "path_row", "acquisition", "processing", "collection", "category")
            return(res)
        }else if(stringr::str_detect(file_name, pattern = "^M(O|Y)D[0-9]{2}[A-Z][0-9].A[0-9]{7}.h[0-9]{2}v[0-9]{2}.00[0-9].[0-9]{13}[.]hdf")){
            res <- unlist(stringr::str_split(file_name, "[.]"))
            names(res) <- c("product", "acquisition", "tile", "collection", "production", "format")
            return(res)
        }else{
            stop(sprintf("Unknow image format: ", basename(file_path)))
        }
    }else{
        res <- lapply(file_path, parse_img_name)
        names(res) <- basename(file_path)
        return(res)
    }
}


#' @title Build a tibble of Landsat-8 and Modis iamges.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a tibble of Landsat-8 and Modis iamges.
#'
#' @param landsat_path  A length-one character. Path to a directory of images.
#' @param modis_path    A length-one character. Path to a directory of images.
#' @param scene_shp     A length-one character. Path to a polygon shapefile of
#' Landsat scene borders.
#' @param tile_shp      A length-one character. Path to a polygon shapefile of
#' MODIS tile borders.
#' @param scenes        A character. Constrain to these scenes (e.g. 233067)
#' @param from          A character. Constrain to this starting date.
#' @param to            A character. Constrain to this ending date.
#' @param add_neighbors A logical. Should neighbor images be considered?
#' @return              A tibble.
#' @export
build_brick_landsat_modis <- function(landsat_path, modis_path, scene_shp,
                                      tile_shp, scenes, from, to,
                                      add_neighbors){

    if (!all(vapply(c(landsat_path, modis_path), dir.exists, logical(1))))
        stop("Directory not found!")
    if (!all(vapply(c(scene_shp, tile_shp), file.exists, logical(1))))
        stop("File not found!")

    pattern_landsat <- tile <- NULL

    if (!is.null(scenes) && is.vector(scenes) && length(scenes) > 0) {
        scene_neigh <- scenes
        if (add_neighbors) {
            scene_neigh <- scenes %>%
                get_tile_neighbors() %>%
                c(scenes) %>%
                unique() %>%
                .[!is.na(.)]
        }
        if (length(scene_neigh) > 1) {
            scene_neigh <- paste0('(', paste(scene_neigh, collapse = '|'), ')')
        }
        pattern_landsat <- stringr::str_c("^LC08_L1[TG][PTS]_", scene_neigh,
                                          ".*(tif|_MTL\\.txt)$")
    }

    # Get Landsat's image metadata.
    landsat_tb <- landsat_path %>%
        build_landsat_tibble(pattern = pattern_landsat, from = from, to = to)

    # Spatially match Landsat scenes to MODIS tiles.
    l8mod_sp <- match_tiles2scenes(scene_path = scene_shp, tile_path = tile_shp,
                                   scenes = unique(dplyr::pull(landsat_tb, tile))) %>%
        dplyr::mutate(tile = purrr::map(tile, function(x) {dplyr::pull(x, tile)}))

    landsat_tb <- landsat_tb %>%
        dplyr::rename(scene = tile) %>%
        dplyr::inner_join(l8mod_sp, by = 'scene')

    # match L8 scenes to MOD tiles (in time)
    tiles <- landsat_tb %>%
        dplyr::pull(tile) %>%
        unlist() %>%
        unique()
    if (length(tiles) == 0) {
        return(NA)
    }else if (length(tiles) > 1) {
        tiles <- paste0('(', paste(tiles, collapse = '|'), ')')
    }
    pattern_mod <- stringr::str_c('^MOD13Q1\\.A201[2-8][0-9]{3}\\.', tiles,
                                  '.*hdf$')
    mod_img <- modis_path %>%
        build_modis_tibble(pattern = pattern_mod, from = from, to = to)
    landsat_tb <- landsat_tb %>%
        dplyr::mutate(tile = purrr::map2(.$tile, .$img_date, match_tile_date,
                                         img_tb = mod_img, untie = 0.001))

    return(landsat_tb)
}


#' @title Build a metadata table of HLS images in a directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table the Harmonized Landsat8-Sentinel2 satellite
#' imagess in the input directory.
#'
#' @param in_dir            A charater. Path to a directory with images.
#' @param pattern           A length-one character. Pattern used to filter
#' files.
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
#' @importFrom rlang .data
build_hls_tibble <- function(in_dir, pattern, prodes_year_start = "-08-01") {

    product <- type <- v1 <- v2 <- ydoy <- NULL

    # Get HLS metadata from the file name.
    .get_md <- function(file_path){
        v1 <- v2 <- NULL
        res <- file_path %>%
            basename() %>%
            stringr::str_split(pattern = "[.]") %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            t()
        colnames(res) <- c("type", "product", "tile", "ydoy", "v1", "v2", "fileext")
        rownames(res) <- NULL
        res <- res %>%
            tibble::as_tibble() %>%
            dplyr::mutate(version = paste(v1, v2, sep = '.')) %>%
            dplyr::select("type", "product", "tile", "ydoy", "version", "fileext")
        res$file_path <- file_path
        return(res)
    }

    res <- in_dir %>%
        list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
        .get_md() %>%
        dplyr::select(-c(type, version)) %>%
        dplyr::mutate(img_date = ydoy %>%
                                    strptime(format = '%Y%j') %>%
                                    as.Date(),
                      pyear = dplyr::if_else(lubridate::month(img_date) < 8,
                                             lubridate::year(img_date),
                                             lubridate::year(img_date) + 1))

    # HLS files share the same extent, there is no need to read them all.
    img_extent <- res %>%
        dplyr::select(file_path, product, tile) %>%
        dplyr::group_by(product, tile) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(img_extent = purrr::map(file_path, function(file_path){
            if (stringr::str_detect(basename(file_path), "L30")) {
	        band = "band01"
	    } else if (stringr::str_detect(basename(file_path), "S30")) {
	        band = "B01"
	    }else{
	        warning("Unknown image type.")
	        return(NA)
	    }
	    ext <- file_path %>%
	        read_hdf(band = band) %>%
	        raster::projectExtent(crs = "+proj=longlat +datum=WGS84") %>%
	        raster::extent() %>%
	        attributes()
	    ext[["class"]] <- NULL
	    return(unlist(ext))
	})) %>%
        dplyr::select(-file_path)

    res %>%
        dplyr::left_join(img_extent, by = c("product","tile")) %>%
        dplyr::select(-ydoy) %>%
        return()
}


#' @title Build a table from the images in the given directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table from the given images.
#'
#' @param in_dir   A charater. Path to a directory with images.
#' @param pattern  A length-one character. Pattern used to filter the files.
#' @param from     A length-one character. The start date.
#' @param to       A length-one character. The end date.
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
build_landsat_tibble <- function(in_dir, pattern = NULL, from = NULL,
                                 to = NULL, prodes_year_start = "-08-01") {

    files <- in_dir %>%
        list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
        ensurer::ensure_that(length(.) > 0,
                             err_desc = sprintf("No files found in %s", in_dir))

    file_path <- img_date <- sat_image <- tile <- NULL
    image_tb <- files %>%
        stringr::str_subset(pattern = ".*[.]tif$") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = "value") %>%
        dplyr::mutate(sat_image = stringr::str_sub(basename(file_path), 1, 40)) %>%
        dplyr::select(sat_image, file_path) %>%
        #tidyr::nest(file_path, .key = "files") %>%
        tidyr::nest(files = file_path) %>%
        dplyr::mutate(tile = substr(sat_image, 11, 16),
                      tier = substr(sat_image, 39, 40),
                      img_date  = lubridate::ymd(substr(sat_image, 18, 25)),
                      proc_date = lubridate::ymd(substr(sat_image, 27, 34)),
                      prodes_year = match_prodes_year(img_date,
                                                      prodes_start = prodes_year_start)) %>%
        dplyr::distinct(tile, img_date, .keep_all = TRUE) %>%
        dplyr::arrange(tile, img_date)

    if (!all(is.null(from), is.null(to))) {
        image_tb <- image_tb %>%
            dplyr::filter(img_date >= lubridate::as_date(from),
                          img_date <= lubridate::as_date(to))
    }

    image_tb$neigh <- purrr::map(image_tb$tile, get_tile_neighbors)
    image_tb$neigh <- purrr::map2(image_tb$neigh, image_tb$img_date,
                                  match_tile_date, img_tb = image_tb,
                                  untie = 0.001)

    # get the cloud coverage
    image_cloud_cov <- files %>%
        stringr::str_subset(pattern = ".*_MTL\\.txt$") %>%
        dplyr::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(sat_image = substr(basename(file_path), 1, 40))

    image_cloud_cov$cloud_cov <- purrr::map_dbl(image_cloud_cov$file_path,
						     function(x) {
            x %>% readLines() %>%
                stringr::str_subset("CLOUD_COVER_LAND") %>%
                stringr::str_split(" = ") %>%
                unlist() %>%
                dplyr::last() %>%
                as.numeric() * 1/100
        })

    image_cloud_cov <- image_cloud_cov %>%
        dplyr::select(sat_image, cloud_cov)
    image_tb <- image_tb %>%
        dplyr::left_join(image_cloud_cov, by = "sat_image")

    return(image_tb)
}


#' @title Build a table from the images in the given directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table from the images in the given directory.
#'
#' @param in_dir   A character. Path to a directory with images.
#' @param pattern  A length-one character. Pattern used to filter the files.
#' @param from     A length-one character. The start date.
#' @param to       A length-one character. The end date.
#' @param prodes_year_start A length-one character. The start month and date of
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
build_modis_tibble <- function(in_dir, pattern, from = NULL, to = NULL,
                               prodes_year_start = "-08-01") {
    mod_img <- in_dir %>%
        list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(sat_image = basename(file_path)) %>%
        dplyr::select(sat_image, file_path) %>%
        dplyr::mutate(
            tile = substr(sat_image, 18, 23),
            img_date = as.Date(lubridate::parse_date_time(substr(sat_image, 10, 16),
                                                          orders = "%Y%j")),
            prodes_year = match_prodes_year(img_date,
                                            prodes_start = prodes_year_start)) %>%
        dplyr::distinct(tile, img_date, .keep_all = TRUE) %>%
        dplyr::arrange(tile, img_date)

    if (!all(is.null(from), is.null(to))) {
        mod_img <- mod_img %>%
            dplyr::filter(img_date >= lubridate::as_date(from),
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


#' @title Build a table from the images in the given directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build a table from the given images.
#'
#' @param in_dir   A charater. Path to a directory with images.
#' @param from     A length-one character. The start date.
#' @param to       A length-one character. The end date.
#' the PRODES year. The default is "-08-01"
#' @return A tibble
#' @export
build_sentinel_tibble <- function(in_dir, from = NULL, to = NULL) {

    # @title Build a table from the bands of the given image directory.
    # @author Alber Sanchez, \email{alber.ipia@@inpe.br}
    # @description Build a table from the bands of the given image directory.
    #
    # @param path    A length-one charater. Path to a directory with images.
    # @param pattern A length-one charater. A pattern to filter files.
    # @return     A tibble
    get_bands <- function(path, pattern){
        file_name <- NULL
        path %>%
            list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
            tibble::enframe(name = NULL) %>%
            dplyr::rename(file_path = "value") %>%
            dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
            tidyr::separate(col = file_name, into = c("tile", "acquisition",
                                                      "band"), sep = '_') %>%
            return()
    }
    dir_name <- img_date <- NULL
    image_tb <- in_dir %>%
        list.dirs() %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = "value") %>%
        dplyr::filter(endsWith(file_path, ".SAFE")) %>%
        dplyr::mutate(dir_name = tools::file_path_sans_ext(basename(file_path))) %>%
        tidyr::separate(col = dir_name,
                        into = c("mission", "level", "img_date", "baseline",
                                 "orbit", "tile", "processing"), sep = '_') %>%
        dplyr::mutate(files = purrr::map(file_path, get_bands,
                                         pattern = "([.]jp2$|[.]tif$)"),
                      img_date = lubridate::as_date(img_date),
                      processing = lubridate::as_date(processing)) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             err_desc = sprintf("No images found in %s",
                                                    in_dir))

    if (!all(is.null(from), is.null(to))) {
        image_tb <- image_tb %>%
            dplyr::filter(img_date >= lubridate::as_date(from),
                          img_date <= lubridate::as_date(to))
    }
    return(image_tb)
}

