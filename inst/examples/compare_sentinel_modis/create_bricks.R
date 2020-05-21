# CREATE BRICKS SENTINEL AND LANDSAT.

# TODO:
# - Arrumar bricks Sentinel2. Vou que processar eles com sen2cor para obter reflectança de superfície e poder comparar com Landsat-8. Por agora vou trabalhar só um tile Sentinel-2 e um tile Landsat-8
# - Arrumar bricks Landsat-8 para garantir sobreposição com Sentinel-2 no eixo do tempo.
# - Rodar mascara de nuvem Fmask4 e arrumar bricks de mascada de nuvens. Assim o meu analisais acompanha o nosso artigo das nuvens.
# - Usar as mostras que peguei de Landsat-8 para treinar um modelo com os bricks Sentinel-2.
# - Comparar os resultados.

.Deprecated("Use bash scripts instead")
library(dplyr)
library(tidyr)
library(lubridate)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

# TODO: Run fmask 4 on Landsat images collection 1 level 1 once they're
# donwloaded. However, these images and surface reflectance were already
# processes using a former version of mask.

# NOTE: Fmask_4 masks must be present in Sentinel L1C images.
# TODO: Are landsat images needed? Tehy have been already processesd with F_mask 3 by USGS.

sentinel_l1c_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C"
sentinel_l2a_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A"
landsat_sr_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/landsat_sr/images"

# outputs
sentinel_raw_brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"

# Sentinel images L2A processed using Sen2Cor.
sentinel_tb <- sentinel_l2a_dir %>%
    sits.starfm::build_sentinel_tibble() %>%
    dplyr::rename(safe_path = file_path) %>%
    dplyr::mutate(pyear = prodes_year(img_date),
                  files_10m = purrr::map(files, dplyr::filter, resolution == "10m"),
                  files_20m = purrr::map(files, dplyr::filter, resolution == "20m"),
                  files_60m = purrr::map(files, dplyr::filter, resolution == "60m")) %>%
    dplyr::select(-files)

# Split images into chunks.
chunk_tb <- sentinel_tb %>%
    dplyr::select(-files_20m, -files_60m, -tile) %>%
    # TODO: Filter by band!
    # TODO: Bands depend on spatial resolution!
    dplyr::filter(band %in% c("B02", "B03", "B04", "B08")) %>%
    tidyr::unnest(files_10m)
chunk_tb[["chunks"]] <- parallel::mclapply(chunk_tb$file_path, split_image,
                                           xsize = 256, ysize = 256,
                                           # TODO: Quarter a million files for a single image. Use subdirectories!
                                           out_dir = "/disks/d3/tmp",
                                           mc.cores = 6)

fmasks_tb <- sentinel_l1c_dir %>%
    list.files(pattern = "[A-Z0-9]{3}_[A-Z0-9]{6}_A[0-9]{6}_[0-9]{8}T[0-9]{6}_Fmask4[.]tif",
               full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = basename(file_path),
                  safe_path = stringr::str_extract(file_path,
                                                   pattern = ".+?(?=SAFE)"),
                  safe_path = stringr::str_c(safe_path, "SAFE")) %>%
    tidyr::separate(col = file_name, into = c("level", "tile", NA,
                                              "fmask_date", NA), sep = '_')
fmasks_tb[["chunks"]] <- parallel::mclapply(fmasks_tb$file_path, split_image,
                                           xsize = 256, ysize = 256,
                                           out_dir = "/disks/d3/tmp/fmask",
                                           mc.cores = 6)
fmasks_tb <- fmasks_tb %>%
    tidyr::unnest(chunks)
    dplyr::rename(fmask_file = file_path,
                  fmask_chunk_file = out_file)

#-------------------------------------------------------------------------------
# NOTE: Save
saveRDS(sentinel_tb, file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/tmp/sentinel_tb.RDS")
saveRDS(chunk_tb, file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/tmp/chunk_tb.RDS")
saveRDS(fmasks_tb, file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/tmp/fmasks_tb.RDS")
#-------------------------------------------------------------------------------

# NOTE: Michelle quer um brick com imagens originais.
# TODO: Build a time series of the chunk, apply the Fmaks, fit a model, fill in the blanks

time_chunk_tb <- chunk_tb %>%
    tidyr::unnest(chunks) %>%
    tidyr::nest(time_chunk = c(safe_path, img_date, processing,
                               file_path, acquisition, out_file)) %>%






    # NOTE: VRT don't work
    dplyr::mutate(time_chunk_vrt = purrr::map_chr(



res = time_chunk_tb %>%
    dplyr::left_join(fmasks_tb, by = "safe_dir")


# NOTE: VRT don't work with raster package.
helper_build_vrt <- function(x, out_dir){
    out_f <- x %>%
        dplyr::arrange(img_date) %>%
        dplyr::pull(out_file) %>%
        dplyr::first() %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        paste0(".vrt")
    x %>%
        dplyr::arrange(img_date) %>%
        dplyr::pull(out_file) %>%
        gdalcmdline::gdal_build_vrt(out_filename = file.path(out_dir, out_f)) %>%
        return()
}





res = time_chunk_tb %>%
    dplyr::pull(time_series) %>%
    dplyr::first() %>%
    dplyr::arrange(img_date) %>%
    dplyr::pull(out_file)


# NOTE: disc0 still have some room!


time_chunk_tb_dir = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_sentinel2_raw"


# NOTE: Compute 10m NDVI after building the bricks.

