#!/usr/bin/Rscript

# Recode the labels in the samples files from 5 to 2.

suppressMessages(library(dplyr))

base_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation"

# Read samples
recode_to_2_labels <- function(rds_file){
    rds_file %>%
        readRDS() %>%
        dplyr::filter(label != "Water") %>%
        dplyr::filter(label != "Deforestatio") %>%
        dplyr::mutate(label = dplyr::recode(label,
                                            #"Forest"
                                            #"Deforestatio"
                                            #NonForest
                                            Pasture      = "NonForest",
                                            NatNonForest = "NonForest")) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "No rows!") %>%
        ensurer::ensure_that(length(unique(.$label)) == 2,
                             err_desc = "Label missmatch!") %>%
        return()
}

base_dir %>%
    file.path("samples_A_approx.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_A_approx_2l.rds"))

base_dir %>%
    file.path("samples_B_approx.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_B_approx_2l.rds"))

base_dir %>%
    file.path("samples_C_approx.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_C_approx_2l.rds"))

base_dir %>%
    file.path("samples_A_raw.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_A_raw_2l.rds"))

base_dir %>%
    file.path("samples_B_raw.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_B_raw_2l.rds"))

base_dir %>%
    file.path("samples_C_raw.rds") %>%
    recode_to_2_labels() %>%
    saveRDS(file.path(base_dir, "samples_C_raw_2l.rds"))
