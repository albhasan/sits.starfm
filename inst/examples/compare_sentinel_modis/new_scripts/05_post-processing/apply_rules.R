#!/usr/bin/env Rscript
# MASK THE CLASSIFICATION USING THE CLASSIFICATION OF THE FIRST TWO IMAGES OF THE SAME YEAR.

suppressMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
      stop("This script takes parameters: A path to the base directory with classification results.",
           call. = FALSE)
}
results_dir <-  args[[1]]
used_rules  <-  args[[2]]

# Rules to apply during pros-processing
#NOTE: Classification Codes:
# Partial classification == A
# Full classification    == B
#
# 2 Labels:
# Forest        == 1
# Non-Forest    == 2
#
# 3 Labels:
# Deforestation == 1
# Forest        == 2
# Non-Forest    == 3

rule_set <- list()
rule_set[["rules_2_2"]] <- "2 * ((A == 1) & (B == 1)) + 1 * ((A == 1) & (B == 2)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2))"
rule_set[["rules_2_3"]] <- "1 * ((A == 1) & (B == 1)) + 2 * ((A == 1) & (B == 2)) + 1 * ((A == 1) & (B == 3)) + 3 * ((A == 2) & (B == 1)) + 3 * ((A == 2) & (B == 2)) + 3 * ((A == 2) & (B == 3))"
rule_set[["rules_3_3"]] <- "1 * ((A == 1) & (B == 1)) + 3 * ((A == 1) & (B == 2)) + 3 * ((A == 1) & (B == 3)) + 1 * ((A == 2) & (B == 1)) + 2 * ((A == 2) & (B == 2)) + 3 * ((A == 2) & (B == 3)) + 3 * ((A == 3) & (B == 1)) + 3 * ((A == 3) & (B == 2)) + 3 * ((A == 3) & (B == 3))"

stopifnot(dir.exists(results_dir))
stopifnot(used_rules %in% names(rule_set))

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

classification_tb <- results_dir %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::select(-used_labels, -used_samples) %>%
    tidyr::pivot_wider(names_from = "class_type", values_from = "file_path") %>%
    ensurer::ensure_that(!any(is.na(.$full)), err_desc = "Missing full classifications!") %>%
    ensurer::ensure_that(!any(is.na(.$first_2)), err_desc = "Missing partial classifications!") %>%
    dplyr::mutate(post_class = purrr::map2_chr(first_2,
                                               full,
                                               postprocessing,
                                               rules = rule_set[[used_rules]],
                                               partial = "first"))
