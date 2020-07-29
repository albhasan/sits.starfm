#!/usr/bin/Rscript

# Recode the labels in the samples files from 5 to 3.

suppressMessages(library(dplyr))

base_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation_other"

# Read samples
recode_to_3_labels <- function(rds_file){
    rds_file %>%
        readRDS() %>%
        dplyr::filter(label != "Water") %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "No rows!") %>%
        return()
}


base_dir %>%
    file.path("samples_B_approx.rds") %>%
    recode_to_3_labels() %>%
    saveRDS(file.path(base_dir, "samples_B_approx_5l.rds"))
