#-------------------------------------------------------------------------------
# BUILD A BRICK OF RAW IMAGES OF SENTINEL-2 IMAGES.
#-------------------------------------------------------------------------------
stop("Use build_brick_sentinel.R")
library(dplyr)
library(sits.starfm)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

in_dir  <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"

# Sentinel images L2A processed using Sen2Cor.
sentinel_tb <- in_dir %>%
    sits.starfm::build_sentinel_tibble() %>%
    dplyr::rename(safe_path = file_path) %>%
    dplyr::mutate(pyear = prodes_year(img_date),
                  files_10m = purrr::map(files, dplyr::filter, resolution == "10m")) %>%
    dplyr::select(-files, -tile) %>%
    tidyr::unnest(files_10m) %>%
    dplyr::filter(band %in% c("B02", "B03", "B04", "B08")) %>%
    ensurer::ensure_that(nrow(.) > 0, err_des = "Images not found!")

# Pile the images into bricks.
brick_tb <- sentinel_tb %>%
    dplyr::group_by(mission, level, baseline, orbit, pyear, band, resolution) %>%
    dplyr::group_map(~ helper_pile_raw(.x, out_dir = out_dir), keep = TRUE) %>%
    dplyr::bind_rows()


#-------------------------------------------------------------------------------
# Compute vegetation index.
vegind_tb <- sentinel_tb %>%
    tidyr::pivot_wider(names_from = band, values_from = file_path) %>%
    dplyr::mutate(vrt_file = purrr::pmap_chr(dplyr::select(., B02, B03, B04, B08), helper_vrt_vi),
                  gemi_file  = tempfile(pattern = "gemi_", fileext = ".tif"),
                  mtvi_file  = tempfile(pattern = "mtvi_", fileext = ".tif"),
                  ndvi_file  = tempfile(pattern = "ndvi_", fileext = ".tif"),
                  ndwi_file  = tempfile(pattern = "ndwi_", fileext = ".tif"),
                  osavi_file = tempfile(pattern = "osavi_", fileext = ".tif"),
                  rdvi_file  = tempfile(pattern = "rdvi_", fileext = ".tif"),
                  savi_file  = tempfile(pattern = "savi_", fileext = ".tif"),
    )
#vegind_tb <- vegind_tb %>%
#    dplyr::mutate(gemi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = gemi_file,  index_name = "GEMI"),
#                  mtvi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = mtvi_file,  index_name = "MTVI"),
#                  ndvi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = ndvi_file,  index_name = "NDVI"),
#                  ndwi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = ndwi_file,  index_name = "NDWI"),
#                  osavi = purrr::map(vrt_file, compute_vi_sentinel, out_file = osavi_file, index_name = "OSAVI"),
#                  rdvi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = rdvi_file,  index_name = "RDVI"),
#                  savi  = purrr::map(vrt_file, compute_vi_sentinel, out_file = savi_file,  index_name = "SAVI"))




#TODO: pile bands and vegetation index as they were bands too.
# NOTE: It's not working, try something simpler!!!!
res = sentinel_tb %>%
    #dplyr::select(-gemi, -mtvi, -ndvi, -ndwi, -osavi, -rdvi, -savi) %>%
    #tidyr::pivot_longer(cols = tidyselect::one_of("safe_path", "mission", "level",
    #                                              "img_date", "baseline",
    #                                              "orbit", "processing",
    #                                              "pyear", "tile",
    #                                              "acquisition", "resolution"),
    #                    names_to = "band") %>%
    tidyr::pivot_longer(cols = tidyselect::one_of(c("gemi_file", "mtvi_file",
                                                  "ndvi_file", "ndwi_file",
                                                  "osavi_file", "rdvi_file",
                                                  "savi_file")),
                        names_to = "band") %>%


    #

# TODO: pile the vegetation indexes.
res = vegind_tb %>%
    tidyr::pivot_longer(cols) %>%
    dplyr::group_by(mission, level, baseline, orbit, pyear, resolution, )

    dplyr::mutate(brick_ndvi = stringr::str_c(mission, level, orbit, tile, acquisition, "NDVI", paste0(resolution, ".tif"), sep = '_'))


    dplyr::mutate(brick_ndvi = purrr::map_chr(ndvi, pile_files, out_fn))




# TODO: compute additional indexes .



