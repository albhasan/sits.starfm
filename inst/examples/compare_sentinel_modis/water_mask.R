library(dplyr)

results_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2" %>%
    list.files(pattern = "postprocessing_first.tif",
               recursive = TRUE, full.names = TRUE) %>%
               tibble::enframe(name = NULL) %>%
               dplyr::rename(file_path = value) %>%
               dplyr::mutate(water_mask = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/water_mask.tif") %>%
               dplyr::mutate(masked = purrr::map2_chr(file_path, water_mask,
                             function(file_path, water_mask){
                                    out_file <- file_path %>%
                                        dirname() %>%
                                        file.path(paste0(tools::file_path_sans_ext(basename(file_path)), "_masked.tif"))
                                    cmd <- sprintf("/usr/bin/gdal_calc.py -A %s -B %s --calc='A * (B == 0) + (4 * (B == 1))' --type=Int16 --co='COMPRESS=LZW' --co='PREDICTOR=2' --NoDataValue=-9999 --outfile=%s --overwrite",
                                                   file_path, water_mask, out_file)
                                    system(cmd)
                                    return(out_file)
                             }))
