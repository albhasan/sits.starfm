#!/usr/bin/Rscript
suppressMessages(library(dplyr))
suppressMessages(library(caret))
suppressMessages(library(sits))

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
    stop("This script takes two parameters: Two input files (RDS) of time series of samples, and an output directory.",  call. = FALSE)
}

samples_file_1 <- args[[1]]
samples_file_2 <- args[[2]]
out_dir     <- args[[3]]
#samples_file_1 <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds"
#samples_file_2 <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds"

samples_tb <- samples_file_1 %>%
    readRDS() %>%
    dplyr::bind_rows(readRDS(samples_file_2)) %>%
    dplyr::filter(label != "Water")

experiments <- list(all_bands = c("blue","bnir","green","nnir","red","swir1","swir2"),
                    indeces   = c("evi","ndmi","ndvi"))

source("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/util.R")

run <- function(bands, samples_tb){
    lapply(1:100, function(i){
        samples_tb %>%
            sits::sits_sample(n = 60) %>%
            select_bands(bands) %>%
            sits::sits_kfold_validate(folds = 10,
                                      ml_method = sits::sits_rfor(num_trees = 1000)) %>%
            sits::sits_conf_matrix() %>%
            return()
    })
}

kfold_ls <- lapply(experiments, run, samples_tb = samples_tb)

helper_acc <- function(x, label, acc_type){
    sapply(x,  get_up_accuracy, label = label, acc_type = acc_type)
}

exp_tb <- tibble::tibble(experiment = names(experiments)) %>%
    dplyr::mutate(kfold = kfold_ls) %>%
    dplyr::mutate(def_pa = purrr::map(kfold, helper_acc, label = "Deforestatio", acc_type = "pa"),
                  def_ua = purrr::map(kfold, helper_acc, label = "Deforestatio", acc_type = "ua"),
                  for_pa = purrr::map(kfold, helper_acc, label = "Forest",       acc_type = "pa"),
                  for_ua = purrr::map(kfold, helper_acc, label = "Forest",       acc_type = "ua"),
                  nnf_pa = purrr::map(kfold, helper_acc, label = "NatNonForest", acc_type = "pa"),
                  nnf_ua = purrr::map(kfold, helper_acc, label = "NatNonForest", acc_type = "ua"),
                  nof_pa = purrr::map(kfold, helper_acc, label = "NonForest",    acc_type = "pa"),
                  nof_ua = purrr::map(kfold, helper_acc, label = "NonForest",    acc_type = "ua"),
                  pas_pa = purrr::map(kfold, helper_acc, label = "Pasture",      acc_type = "pa"),
                  pas_ua = purrr::map(kfold, helper_acc, label = "Pasture",      acc_type = "ua"))

helper_plot <- function(x, label){
    x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        tidyr::pivot_longer(cols = !tidyselect::matches("index"),
                            names_to = "experiment") %>%
        dplyr::mutate(experiment = tools::toTitleCase(stringr::str_replace(experiment, '_', ' '))) %>%
        ggplot2::ggplot(ggplot2::aes(x = value, y = experiment)) +
        ggplot2::geom_boxplot() +
        ggplot2::xlab("Index") +
        ggplot2::ylab("Band Combination") +
        ggplot2::theme(text = ggplot2::element_text(size = 8)) +
        ggplot2::xlim(0.4, 1.0) +
        ggplot2::ggtitle(label)
}

fix_name <- function(x){
    x %>%
        stringr::str_replace('_', ' ') %>%
        stringr::str_replace(" ua", " User Accuracy") %>%
        stringr::str_replace(" pa", " Producer Accuracy") %>%
        stringr::str_replace("def ", "Deforestation ") %>%
        stringr::str_replace("for ", "Forest ") %>%
        stringr::str_replace("nnf ", "Natural Non-Forest ") %>%
        stringr::str_replace("nof ", "Non-Forest ") %>%
        stringr::str_replace("pas ", "Pasture ") %>%
        return()
    }

for (name in names(exp_tb)[3:ncol(exp_tb)]) {
    helper_plot(exp_tb[[name]], fix_name(name)) %>%
        ggplot2::ggsave(file = file.path(out_dir,
                                         paste0(tolower(fix_name(name)), ".png")),
                        width = 8, height = 3, units = "cm")
}
