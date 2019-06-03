# build bricks

library(dplyr)
library(sits.starfm)

base_path <- "/home/alber/Documents/data/experiments/l8mod-fusion"

dump_and_quit <- function() {
    dump_file <- base_path %>% file.path("Rpackage", "sits.starfm",
        paste0("last.dump_", stringr::str_replace(system('uname -n', intern = TRUE), "-", "_")))
    # Save debugging info to file last.dump.rda
    dump.frames(dumpto = dump_file, to.file = TRUE)
    # Quit R with error status
    q(status = 1)
}
options(error = dump_and_quit)

landsat_path <- "/home/alber/landsat8"
cloud_threshold <- 0.99

img_per_year <- 23
temp_dir = "/home/alber/shared/tmp"
prodes_year_start <- "-08-01"

brick_scene <- "226063"
brick_path  <- "/home/alber/shared/brick"
brick_bands = c("sr_band4", "sr_band5", "sr_band7")
brick_from  <- paste0("2016", prodes_year_start)
brick_to    <- as.character(lubridate::date(paste0("2017", prodes_year_start)) -1)
n_best <- 4

logger                 <- log4r::create.logger()
log4r::logfile(logger) <- file.path(paste0("build_bricks_", system('uname -n', intern = TRUE),".log"))
log4r::level(logger)   <- "DEBUG"


# Get a list of images required for building the brick.
build_landsat_brick_tibble <- function(landsat_path, brick_path, brick_scene,
                                brick_from, brick_to, cloud_threshold, n_best,
                                img_per_year, temp_dir, prodes_year_start){

    brick_year <- lubridate::year(lubridate::date(brick_to))
    if (is.null(temp_dir))
        temp_dir <- tempdir()
    tmp_dir <- temp_dir %>%
        file.path(paste("L8", brick_scene, brick_year, sep = "_"))

    if (!all(dir.exists(landsat_path), dir.exists(brick_path))) {
        stop("Directory not found!")
    }

    scene_neigh <- brick_scene %>% get_tile_neighbors() %>% c(brick_scene) %>%
            unique() %>% .[!is.na(.)]
    pattern_landsat <- stringr::str_c("^LC08_L1[TG][PTS]_", scene_neigh,
                                          ".*(tif|_MTL\\.txt)$")

    l8_img <- landsat_path %>% build_landsat_tibble(pattern = pattern_landsat,
                                                    from = brick_from, 
                                                    to = brick_to,
                                                    prodes_year_start = prodes_year_start) %>%
              dplyr::mutate(y = lubridate::year(img_date)) %>% 
              dplyr::group_by(tile, y) %>% 
              dplyr::top_n(-n_best/2, cloud_cov) %>% dplyr::ungroup() %>% 
              dplyr::slice(1:n_best) %>% ensurer::ensure_that(nrow(.) == 4)
    return(l8_img)
}
brick_imgs <- build_landsat_brick_tibble(landsat_path, brick_path, brick_scene,
                                brick_from, brick_to, cloud_threshold, n_best,
                                img_per_year, temp_dir, prodes_year_start)

# Match each image to the next one.
brick_imgs$next_best <- lapply(1:nrow(brick_imgs), function(x, brick_imgs){
    res <- NA
    if (nrow(brick_imgs) > x) {
        res <- brick_imgs %>% dplyr::slice(x + 1)
    }
    return(res)
}, brick_imgs = brick_imgs)

# Build a brick.
for (row_n in 1:nrow(brick_imgs)) {
    sfm_files <- tibble::tibble(starfm = character(), t1_fine = character(),
                                t1_coarse = character(),
                                t0_fine = character(),
                                t0_coarse = character(),
                                sfm_conf = character())

    img0 <- brick_imgs %>% dplyr::slice(row_n)
    img1 <- img0 %>% dplyr::pull(next_best) %>% dplyr::bind_rows()
    brick_year <- img0$prodes_year

    starfm_dir <- temp_dir %>%
            file.path(paste("starfm", brick_scene, brick_year, sep = "_"))

    for(band in brick_bands){
        out_filename = file.path(starfm_dir,
                                 stringr::str_c(
                                     stringr::str_c("starfm", brick_scene,
                                                    img0$img_date, band,
                                                    sep = "_"), ".bin"))

        if (!dir.exists(basename(out_filename))) {
            dir.create(basename(out_filename))
        }
 
        starfm_res <- run_starFM(img0_f = img0, img1_f = img1, band = band,
                                 out_filename = out_filename,
                                 tmp_dir = temp_dir)


    }
}



