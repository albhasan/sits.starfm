#!/bin/bash
# MASK THE CLASSIFICATION USING THE CLASSIFICATION OF THE FIRST TWO IMAGES OF THE SAME YEAR.

library(dplyr)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

# Rules to apply during pros-processing
# The classification of 2 labels use 1-Forest, 2-Pasture.
# The classification of 3 labels use 1-Deforestation, 2-Forest, 3-Pasture.
rules_2_3 <- "1 * ((A == 1) & (B == 1)) + 2 * ((A == 1) & (B == 2)) + 1 * ((A == 1) & (B == 3)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2)) + 3 * ((A == 2) & (B == 3))"
rules_2_2 <- "2 * ((A == 1) & (B == 1)) + 1 * ((A == 1) & (B == 2)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2))"

# Classification of either the first or the last images in the brick.
first_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_first_v2" %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::rename(partial_class = file_path)

# Classification of the whole brick.
class_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2" %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::rename(full_class = file_path) %>%
    dplyr::left_join(first_tb, by = c("used_method", "used_bands", "img_type")) %>%
    ensurer::ensure_that(!any(is.na(.$full_class)), err_desc = "Missing full classifications!") %>%
    ensurer::ensure_that(!any(is.na(.$partial_class)), err_desc = "Missing partial classifications!") %>%
    # apply the rules
    dplyr::mutate(post_class = purrr::map2_chr(partial_class, full_class,
                                               postprocessing,
                                               rules = rules_2_3,
                                               partial = "first"
                                               #partial = "last"
                                               ))
