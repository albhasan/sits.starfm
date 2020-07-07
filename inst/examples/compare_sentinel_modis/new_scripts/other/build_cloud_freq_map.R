# Build the map of frequency of clouds for the brick.
suppressMessages(library(dplyr))
suppressMessages(library(raster))

sentinel_l1c_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C";

mask_to_number <- function(x){
    stopifnot(file.exists(x))
    out_file <- tempfile(pattern = tools::file_path_sans_ext(basename(x)), fileext = ".tif")
    cmd <- sprintf("gdal_calc.py -A %s --A_band=1 --outfile=%s --calc='(numpy.where(A == 4, 1, 0)).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'", x, out_file)
    system(cmd)
    return(out_file)
}

# Get Fmask 4 masks.
fmask_tb <- sentinel_l1c_dir %>%
    list.files(pattern = "[A-Z0-9]{3}_[A-Z0-9]{6}_A[0-9]{6}_[0-9]{8}T[0-9]{6}_Fmask4[.]tif",
               full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(fmask_path = value) %>%
    dplyr::mutate(file_name = basename(fmask_path),
                  fmask_safe = stringr::str_extract(fmask_path,
                                                   pattern = ".+?(?=SAFE)"),
                  fmask_safe = stringr::str_c(fmask_safe, "SAFE")) %>%
    tidyr::separate(col = file_name, into = c("level", "tile", NA, "fmask_date",
                                              NA), sep = '_') %>%
    dplyr::mutate(img_date = stringr::str_sub(fmask_date, 1, 8),
                  img_date = lubridate::as_date(img_date)) %>%
    dplyr::select(-level) %>%
    dplyr::mutate(mask_as_num = purrr::map_chr(fmask_path, mask_to_number))

fmask_tb %>%
    dplyr::pull(mask_as_num) %>%
    gdalcmdline::gdal_build_vrt(out_filename = tempfile(pattern = "gdalbuildvrt_",
                                                        fileext = ".vrt"),
                                resolution = "highest", separate = TRUE,
                                vrtnodata = -9999)


first_12 <- fmask_tb %>%
        dplyr::slice(1:12) %>%
        dplyr::pull(mask_as_num)
second_12 <- fmask_tb %>%
        dplyr::slice(13:24) %>%
        dplyr::pull(mask_as_num)
third_12 <- fmask_tb %>%
        dplyr::slice(25:36) %>%
        dplyr::pull(mask_as_num)
first_12_file  <- tempfile(pattern = "fmask_first_12_",  fileext = ".tif")
second_12_file <- tempfile(pattern = "fmask_second_12_", fileext = ".tif")
third_12_file  <- tempfile(pattern = "fmask_third_12_",  fileext = ".tif")


letter_ids <- function(file_vec) {
    paste(paste(paste0('-', LETTERS[1:length(file_vec)]), file_vec, sep = ' '), collapse = ' ')
}

sprintf("gdal_calc.py %s --outfile=%s --calc='(A+B+C+D+E+F+G+H+I+J+K+L).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'",
        letter_ids(first_12), first_12_file) %>%
    system()
sprintf("gdal_calc.py %s --outfile=%s --calc='(A+B+C+D+E+F+G+H+I+J+K+L).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'",
        letter_ids(second_12), second_12_file) %>%
    system()
sprintf("gdal_calc.py %s --outfile=%s --calc='(A+B+C+D+E+F+G+H+I+J+K+L).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'",
        letter_ids(third_12), third_12_file) %>%
    system()

out_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/cloud_frequency.tif"
sprintf("gdal_calc.py %s --outfile=%s --calc='(A+B+C).astype(int16)' --type='Int16' --NoDataValue=-9999 --creation-option='COMPRESS=LZW'",
        letter_ids(c(first_12_file, second_12_file, third_12_file)), out_file) %>%
    system()

