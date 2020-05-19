#!/bin/bash
# MASK THE CLASSIFICATION USING THE CLASSIFICATION OF THE FIRST TWO IMAGES OF THE SAME YEAR.

library(dplyr)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

# Rules to apply during pros-processing
# Forest     + Deforestation = Deforestation
# Forest     + Forest        = Forest
# Forest     + Pasture       = Deforestation
# Non-Forest + Deforestation = Non-Forest
# Non-Forest + Forest        = Forest
# Non-Forest + Pasture       = Pasture
rules_reduced2_3 <- "1 * ((A == 101) & (B == 1)) + 2 * ((A == 101) & (B == 2)) + 1 * ((A == 101) & (B == 3)) + 102 * ((A == 102) & (B == 1)) + 2 * ((A == 102) & (B == 2)) + 3 * ((A == 102) & (B == 3))"

# Reclass the classification of the first images in the brick from 4 to 2 labels.
reclass_first <- function(partial_class){
    out_file = tempfile(pattern = "reduced_partial_class_", fileext = ".tif")
    # Forest (1)             = Forest     (101)
    # Natural_Non-Forest (2) = Non-Forest (102)
    # Non-Forest (3)         = Non-Forest (102)
    # Pasture (4)            = Non-Forest (102)
    cmd <- sprintf("/usr/bin/gdal_calc.py -A %s --outfile=%s --calc='(101 * (A == 1) + 102 * (A == 2) + 102 * (A == 3) + 102 * (A == 4)).astype(int16)' --NoDataValue=-9999 --type='Int16' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'",
                   partial_class, out_file )
    system(cmd)
    return(out_file)
}

# Classification maps of either the first or the last images in the brick.
first_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5a/approx_first_v2" %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::rename(partial_class = file_path)

# Classification maps of the whole brick.
class_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2" %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::filter(used_bands != "evi-ndvi-savi") %>%
    dplyr::rename(full_class = file_path) %>%
    dplyr::left_join(first_tb, by = c("used_method", "used_bands", "img_type")) %>%
    ensurer::ensure_that(!any(is.na(.$full_class)), err_desc = "Missing full classifications!") %>%
    ensurer::ensure_that(!any(is.na(.$partial_class)), err_desc = "Missing partial classifications!") %>%
    # Reduce the nubmer of classes of the first classification
    dplyr::mutate(partial_reduced = purrr::map_chr(partial_class, reclass_first)) %>%
    #NOTE:
    dplyr::mutate(out_dir = stringr::str_replace(dirname(full_class), pattern = "results5", replacement = "results5a")) %>%
    # apply the rules
    dplyr::mutate(post_class = purrr::pmap_chr(dplyr::select(., partial_reduced, full_class, out_dir),
                                               function(partial_reduced, full_class, out_dir){
                                                   postprocessing(partial_class = partial_reduced,
                                                                  full_class = full_class,
                                                                  rules = rules_reduced2_3,
                                                                  partial = "first",
                                                                  out_dir = out_dir) %>%
                                                   return()
                                               }))



