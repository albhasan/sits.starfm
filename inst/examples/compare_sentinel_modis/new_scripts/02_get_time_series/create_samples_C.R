#!/usr/bin/Rscript

# Buld the sample set B by randomly selecting samples from samples_A and samples_B.

suppressMessages(library(dplyr))

set.seed(666)
base_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation"

sample_number <- tibble::tribble(~label,         ~n,
                                 "Deforestatio", 60,
                                 "Forest",       60,
                                 "NatNonForest", 20,
                                 "NonForest",    20,
                                 "Pasture",      20)

build_samples_C <- function(samples_A, samples_B, sample_number){
    samples_C <- samples_A %>%
        dplyr::bind_rows(samples_B) %>%
        dplyr::group_by(label) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::left_join(sample_number, by = "label") %>%
        ensurer::ensure_that(all(!is.na(.$n)),
                             err_desc = "Missing number of samples!") %>%
        dplyr::mutate(samp = purrr::map2(data, n, sample_n)) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(samp) %>%
        return()
}

# Read samples_A & samples_B
samples_A_approx <- base_dir %>%
    file.path("samples_A_approx.rds") %>%
    readRDS() %>%
    dplyr::filter(label != "Water") %>%
    ensurer::ensure_that(nrow(.) > 0)
samples_B_approx <- base_dir %>%
    file.path("samples_B_approx.rds") %>%
    readRDS() %>%
    dplyr::filter(label != "Water") %>%
    ensurer::ensure_that(nrow(.) > 0)
build_samples_C(samples_A_approx, samples_B_approx, sample_number) %>%
    saveRDS(file.path(base_dir, "samples_C_approx.rds"))

samples_A_raw <- base_dir %>%
    file.path("samples_A_raw.rds") %>%
    readRDS() %>%
    dplyr::filter(label != "Water") %>%
    ensurer::ensure_that(nrow(.) > 0)
samples_B_raw <- base_dir %>%
    file.path("samples_B_raw.rds") %>%
    readRDS() %>%
    dplyr::filter(label != "Water") %>%
    ensurer::ensure_that(nrow(.) > 0)
build_samples_C(samples_A_raw, samples_B_raw, sample_number) %>%
    saveRDS(file.path(base_dir, "samples_C_raw.rds"))
