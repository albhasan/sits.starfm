#devtools::install_github("vqv/ggbiplot")
library(plyr)
library(dplyr)
library(parallel)
library(sits)

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#selected_labels <-  c("Forest", "Pasture")
selected_labels <-  c("Forest", "Pasture", "Deforestation")

approx_samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/samples/approx_samples_tb.rds"

# Helper for subseting the bands in a sits tibble
select_bands <- function(x, bands){
    bands <- c("Index", bands)
    x$time_series <- lapply(x$time_series, function(y){y[, bands]})
    return(x)
}

# Compute the PCA given a time series tibble
# pca_matrix contrains the coefficintes of eac PC (columns) for each band (rows)
compute_pca <- function(.data, pca_matrix) {
    observation_mt <- .data %>%
        dplyr::select(tidyselect::starts_with(rownames(pca_matrix))) %>%
        as.matrix()
    if (ncol(observation_mt) != nrow(pca_matrix)) {
        warning(sprintf("non-conformable arguments: %s",
                        paste(rownames(pca_matrix), collapse = " ")))
        return(NA)
    }
    .data %>%
        dplyr::bind_cols(tibble::as_tibble(observation_mt %*% pca_matrix)) %>%
        return()
}

# Produce the combinatory the available bands, n_bands at the time
combine_bands <- function(n_bands, available_bands){
    available_bands %>%
        combn(m = n_bands) %>%
        split(f = col(.)) %>%
        return()
}

# Get the user or producer accuracies from an accuracy object for the given label.
get_acc <- function(x, label, acc_type = "pa") {
    if (acc_type == "pa")
        accuracy_label <- "Sensitivity"
    if (acc_type == "ua")
        accuracy_label <- "Pos Pred Value"
    index_mt <- x %>%
        magrittr::extract2("byClass")
    row_id <- match(label, stringr::str_match(rownames(index_mt), label))
    col_id <- match(accuracy_label, stringr::str_match(colnames(index_mt),
                                                       accuracy_label))
    if (any(is.na(c(row_id, col_id))))
        return(NA)
    return(index_mt[row_id, col_id])
}


# Get the rotations of the first two principal components.
f0 <- function(selected_bands, selected_labels, samples_tb){
    samples_tb %>%
        dplyr::filter(label %in% selected_labels) %>%
        select_bands(selected_bands) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
        dplyr::select(label, time_series) %>%
        tidyr::unnest(time_series) %>%
        dplyr::select(-label, -Index) %>%
        stats::prcomp(center = TRUE, scale = rep(10000, length(selected_bands))) %>%
        magrittr::extract2("rotation") %>%
        magrittr::extract(, 1:2) %>%
        return()
}


# Use the labels Forest and Pasture to do PCA, add PC1 & PC2 as bands on all the
# sample time series and finally compute a K-Folds.
f <- function(selected_bands, selected_labels, samples_tb,
              kfold_method = sits::sits_rfor(num_trees = 1000), plot = FALSE){
    filtered_samples_tb <- samples_tb %>%
        dplyr::filter(label %in% selected_labels) %>%
        select_bands(selected_bands) %>%
        ensurer::ensure_that(nrow(.) > 0, err_desc = "Samples are missing") %>%
        dplyr::select(label, time_series) %>%
        tidyr::unnest(time_series)
    samples_pca <- filtered_samples_tb %>%
        dplyr::select(-label, -Index) %>%
        stats::prcomp(center = TRUE, scale = rep(10000, length(selected_bands)))
    if (plot)
        print(ggbiplot::ggbiplot(samples_pca, ellipse = TRUE,
                           groups = filtered_samples_tb$label) +
                  ggplot2::ggtitle(paste(selected_bands, collapse = " ")))
    pca_mt <- samples_pca %>%
        magrittr::extract2("rotation") %>%
        magrittr::extract(, 1:2)
    samples_tb <- samples_tb %>%
        dplyr::mutate(time_series = purrr::map(time_series, compute_pca,
                                               pca_matrix = pca_mt)) %>%
        sits::sits_select_bands(PC1, PC2)
    samples_tb %>%
        dplyr::mutate(is_na = purrr::map_lgl(time_series,
                                             function(x){any(is.na(x))})) %>%
        dplyr::filter(!is_na) %>%
        sits::sits_kfold_validate(ml_method = kfold_method) %>%
        sits::sits_conf_matrix() %>%
        return()
}

# Get sample time series.
samples_tb <- approx_samples_file %>%
    readRDS() %>%
    clean_ts()
available_bands <- samples_tb %>%
    sits::sits_bands()

# Compute all f for all the combination of bands
pca_accuracy_tb <- tibble::tibble(n_bands = 2:(length(available_bands) - 1)) %>%
    dplyr::mutate(selected_bands = purrr::map(n_bands, combine_bands,
                                              available_bands = available_bands)) %>%
    tidyr::unnest(cols = selected_bands) %>%
    dplyr::mutate(used_bands = purrr::map_chr(selected_bands,
                                              stringr::str_c, collapse = ","))
pca_accuracy_tb[["pca_res"]] <- parallel::mclapply(pca_accuracy_tb$selected_bands,
                                                     f,
                                                     selected_labels = selected_labels,
                                                     samples_tb = samples_tb,
                                                     mc.cores = 16)

# Add the user and producer accuracies.
pca_accuracy_tb <- pca_accuracy_tb %>%
    dplyr::mutate(deforestation_pa = purrr::map_dbl(pca_res, get_acc,
                                                    label = "Deforestation",
                                                    acc_type = "pa"),
                  deforestation_ua = purrr::map_dbl(pca_res, get_acc,
                                                    label = "Deforestation",
                                                    acc_type = "ua"),
                  forest_pa = purrr::map_dbl(pca_res, get_acc,
                                             label = "Forest",
                                             acc_type = "pa"),
                  forest_ua = purrr::map_dbl(pca_res, get_acc,
                                             label = "Forest",
                                             acc_type = "ua"),
                  pasture_pa = purrr::map_dbl(pca_res, get_acc,
                                              label = "Pasture",
                                              acc_type = "pa"),
                  pasture_ua = purrr::map_dbl(pca_res, get_acc,
                                              label = "Pasture",
                                              acc_type = "ua"))

saveRDS(pca_accuracy_tb, file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/pca_accuracy_tb.rds")

# Save results to CSV and print accuracies as function of number of bands in PCA.
pca_accuracy_tb %>%
    dplyr::arrange(dplyr::desc(deforestation_pa), dplyr::desc(deforestation_ua),
                   dplyr::desc(forest_pa), dplyr::desc(forest_ua),
                   dplyr::desc(pasture_pa), dplyr::desc(pasture_ua)) %>%
    dplyr::select(-selected_bands, -pca_res) %>%
    (function(.data){
        View(.data)
        readr::write_csv(.data, path = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/pca_acc.csv")
        invisible(.data)
    }) %>%
    dplyr::mutate(n_bands = stringr::str_pad(as.character(n_bands),
                                             width = 2, pad = "0")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = deforestation_pa,
                                     y = deforestation_ua,
                                     color = n_bands)) +
    ggplot2::coord_fixed() +
    ggplot2::geom_abline()
ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/pca_acc.png")

# Process and print some specific combination of bands.
pca_accuracy_tb <- readRDS(file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/pca_accuracy_tb_Forest-Pasture.rds")
pca_accuracy_tb <- readRDS(file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/pca_accuracy_tb_Forest-Pasture-Deforestation.rds")
pca_accuracy_tb %>%
    dplyr::filter(used_bands %in% c("blue,bnir,green,red",
                                    "blue,bnir,green,ndvi,red",
                                    "blue,bnir,evi2,green,red",
                                    "bnir,ndvi",
                                    "bnir,evi2",
                                    "ndvi,red",
                                    "blue,green,ndvi",
                                    "blue,evi2,green")) %>%
    #dplyr::mutate(pca_res = purrr::map(selected_bands, f,
    #                                   selected_labels = selected_labels,
    #                                   samples_tb = samples_tb, plot = TRUE)) %>%
    dplyr::select(-selected_bands, -pca_res) %>%
    dplyr::arrange(dplyr::desc(deforestation_pa), dplyr::desc(deforestation_ua),
                   dplyr::desc(forest_pa), dplyr::desc(forest_ua),
                   dplyr::desc(pasture_pa), dplyr::desc(pasture_ua))

# Build brick PCA made of the following band combinations: using 3-label PCA coefficients
# - ndvi,red
# - blue,green,ndiv
# - blue,bnir,green,red
# - green,rdvi,red,savi
pca_accuracy_tb <- readRDS(file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/pca_accuracy_tb_Forest-Pasture.rds")
pca_accuracy_tb <- readRDS(file = "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/papers/deforestation/pca_accuracy_tb_Forest-Pasture-Deforestation.rds")
res <- pca_accuracy_tb %>%
    dplyr::filter(used_bands %in% c("ndvi,red",
                                    "blue,green,ndvi",
                                    "blue,bnir,green,red",
                                    "green,rdvi,red,savi")) %>%
    dplyr::mutate(pca_matrix = purrr::map(selected_bands,
                                          f0,
                                          selected_labels = selected_labels,
                                          samples_tb = samples_tb))

