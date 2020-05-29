#!/usr/bin/env Rscript
# MASK THE CLASSIFICATION USING THE CLASSIFICATION OF THE FIRST TWO IMAGES OF THE SAME YEAR.

library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
      stop("This script takes one parameters: A path to the base directory with classification results.",
           call. = FALSE)
}
results_dir <-  args[[1]]
#results_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9"

stopifnot(dir.exists(results_dir)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

# Rules to apply during pros-processing
# The classification of 2 labels use 1-Forest, 2-Pasture.
# The classification of 3 labels use 1-Deforestation, 2-Forest, 3-Pasture.
#rules_2_3 <- "1 * ((A == 1) & (B == 1)) + 2 * ((A == 1) & (B == 2)) + 1 * ((A == 1) & (B == 3)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2)) + 3 * ((A == 2) & (B == 3))"
#rules_2_2 <- "2 * ((A == 1) & (B == 1)) + 1 * ((A == 1) & (B == 2)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2))"

#NOTE: Codes:
# Partial classification == A
# Full classification    == B
#
# Deforestation == 1
# Forest        == 2
# Non-Forest    == 3
rules_3_3 <- "1 * ((A == 1) & (B == 1)) +
              3 * ((A == 1) & (B == 2)) +
              3 * ((A == 1) & (B == 3)) +
              1 * ((A == 2) & (B == 1)) +
              2 * ((A == 2) & (B == 2)) +
              3 * ((A == 2) & (B == 3)) +
              3 * ((A == 3) & (B == 1)) +
              3 * ((A == 3) & (B == 2)) +
              3 * ((A == 3) & (B == 3))"


# Classification of either the first or the last images in the brick.
classification_tb <- results_dir %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    tidyr::pivot_wider(names_from = "class_type", values_from = "file_path") %>%
    ensurer::ensure_that(!any(is.na(.$full)), err_desc = "Missing full classifications!") %>%
    ensurer::ensure_that(!any(is.na(.$first_2)), err_desc = "Missing partial classifications!") %>%
    dplyr::mutate(post_class = purrr::map2_chr(first_2,
                                               full,
                                               postprocessing,
                                               rules = rules_3_3,
                                               partial = "first"))
