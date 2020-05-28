#!/usr/bin/Rscript
.Deprecater("Use version 3a")
suppressMessages(library(dplyr))
suppressMessages(library(dplyr))
suppressMessages(library(parallel))
suppressMessages(library(sits))

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
    stop("This script takes a parameters: An input file (RDS) of time series of samples.",  call. = FALSE)
}

samples_file <- args[[1]]

source("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/util.R")

#---- setup ----

used_combinations <- c("blue,bnir,green,nnir,red,swir1,swir2",
                       "evi,ndmi,ndvi")

out_file_template <- samples_file %>%
    basename() %>%
    tools::file_path_sans_ext()

#---- script ----

# get valid sample time series.
# samples_tb <- samples_file %>%
#     readRDS() %>%
#     dplyr::mutate(label = dplyr::recode(label,
#                                         Deforestatio = "deforestation",
#                                         NatNonForest = "natural non-forest",
#                                         NonForest    = "non-forest")) %>%
#     dplyr::mutate(label = tools::toTitleCase(label))

samples_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds" %>%
    readRDS() %>%
    dplyr::bind_rows(readRDS("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds")) %>%
    dplyr::filter(label != "Water")



#---- Plot time series ----

samples_plot <- samples_tb %>%
    dplyr::mutate(id = 1:dplyr::n(),
                  time_series = purrr::map(time_series, function(.data){
                      .data %>%
                          dplyr::select(-Index) %>%
                          magrittr::set_colnames(paste0("band_", colnames(.))) %>%
                          dplyr::bind_cols(dplyr::select(.data, Index)) %>%
                          dplyr::select(Index, tidyselect::everything()) %>%
                          return()
                      })) %>%
    tidyr::unnest(time_series) %>%
    dplyr::select(Index, label, tidyselect::starts_with("band_")) %>%
    #---
    dplyr::select(Index, label, band_evi, band_ndvi, band_ndmi, band_savi) %>%
    #---
    tidyr::pivot_longer(cols = tidyselect::starts_with("band_"),
                        names_to = "key") %>%
    ggplot2::ggplot() +
    ggplot2::geom_smooth(ggplot2::aes(x = Index, y = value, color = key)) +
    ggplot2::facet_grid( ~ label)
file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot",
          paste0(out_file_template, ".png")) %>%
    ggplot2::ggsave(plot = samples_plot)

#---- Utilitary functions ----

# get a tibble with the combinatory of bands
helper_kfolds <- function(){
    available_bands <- samples_tb %>%
        sits::sits_bands()
    accuracy_tb <- tibble::tibble(n_bands = 1:(length(available_bands))) %>%
        dplyr::mutate(selected_bands = purrr::map(n_bands, combine_n_bands,
                                                  available_bands = available_bands)) %>%
        tidyr::unnest(cols = selected_bands) %>%
        dplyr::mutate(used_bands = purrr::map_chr(selected_bands,
                                                  stringr::str_c, collapse = ",")) %>%
        dplyr::filter(used_bands %in% c(used_combinations)) %>%
        ensurer::ensure(nrow(.) == length(used_combinations),
                        err_desc = "Band combination not found!") %>%
        dplyr::mutate(filtered_samples = purrr::map(selected_bands,
                                                    function(sb, .data){
                                                        .data %>%
                                                            select_bands(bands = sb) %>%
                                                            return()
                                                    }, .data = samples_tb),
                      k_folds = purrr::map(filtered_samples,
                                           sits::sits_kfold_validate,
                                           ml_method = sits::sits_rfor(num_trees = 1000),
                                           ),
                      con_mat = purrr::map(k_folds, sits::sits_conf_matrix),
                      deforestation_pa = purrr::map_dbl(con_mat, get_up_accuracy,
                                                        label = "deforestation",
                                                        acc_type = "pa"),
                      deforestation_ua = purrr::map_dbl(con_mat, get_up_accuracy,
                                                        label = "deforestation",
                                                        acc_type = "ua"),
                      forest_pa = purrr::map_dbl(con_mat, get_up_accuracy,
                                                 label = "forest",
                                                 acc_type = "pa"),
                      forest_ua = purrr::map_dbl(con_mat, get_up_accuracy,
                                                 label = "forest",
                                                 acc_type = "ua"),
                      pasture_pa = purrr::map_dbl(con_mat, get_up_accuracy,
                                                  label = "pasture",
                                                  acc_type = "pa"),
                      pasture_ua = purrr::map_dbl(con_mat, get_up_accuracy,
                                                  label = "pasture",
                                                  acc_type = "ua")) %>%
        dplyr::arrange(dplyr::desc(deforestation_pa),
                       dplyr::desc(deforestation_ua),
                       dplyr::desc(pasture_pa),
                       dplyr::desc(pasture_ua),
                       dplyr::desc(forest_pa),
                       dplyr::desc(forest_ua)) %>%
        dplyr::select(-n_bands, -selected_bands, -filtered_samples, k_folds, con_mat)
    return(accuracy_tb)
}

#---- Run K-Folds ----

# Run the K_Folds several times to diminish random variation in the results.
kfold_ls <- list()
#for (i in 1:33) {
for (i in 1:1) {
    kfold_ls[[i]] <- helper_kfolds()
}

kfold_file <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation",
          paste0(out_file_template, "_kfolds_ls.rds"))
kfold_ls %>%
    saveRDS(file = kfold_file)
# kfold_ls <- readRDS(kfold_file)

# Helper for box plotting.
plot_accuracies <- function(.data, var_name, my_xmin = 0.825){
    var_name <- rlang::enquo(var_name)
    x_label <- var_name %>%
        rlang::as_label() %>%
        tools::toTitleCase() %>%
        stringr::str_replace('_', ' ') %>%
        stringr::str_replace('ua', 'User Accuracy') %>%
        stringr::str_replace('pa', 'Producer Accuracy')
    .data %>%
        dplyr::select(used_bands, !!var_name) %>%
        ggplot2::ggplot(ggplot2::aes(y = used_bands, x = !!var_name)) +
        ggplot2::geom_boxplot() +
        ggplot2::xlab(x_label) +
        ggplot2::ylab("Band Combination") +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) +
        ggplot2::xlim(my_xmin, 1.0) %>%
        return()
}

# Box plot the k-folds of the same combination of bands.
helper_save_plot <- function(my_plot, file_prefix){
    "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot" %>%
        file.path(paste0(file_prefix, '_', out_file_template, ".png")) %>%
        ggplot2::ggsave(plot = my_plot, width = 800, 300)
    invisible(my_plot)
}
kfold_ls %>%
    dplyr::bind_rows() %>%
        dplyr::filter(used_bands %in% c("evi,ndmi,ndvi",
                                    "blue,bnir,green,nnir,red,swir1,swir2")) %>%
    (function(.data){
        my_xmin = 0.4
        .data %>% plot_accuracies(deforestation_pa, my_xmin) %>% helper_save_plot(file_prefix = "pa_def")  %>% print()
        .data %>% plot_accuracies(forest_pa, my_xmin)        %>% helper_save_plot(file_prefix = "pa_for")  %>% print()
        .data %>% plot_accuracies(pasture_pa, my_xmin)       %>% helper_save_plot(file_prefix = "pa_pas")  %>% print()
        .data %>% plot_accuracies(deforestation_ua, my_xmin) %>% helper_save_plot(file_prefix = "ua_def")  %>% print()
        .data %>% plot_accuracies(forest_ua, my_xmin)        %>% helper_save_plot(file_prefix = "ua_for")  %>% print()
        .data %>% plot_accuracies(pasture_ua, my_xmin)       %>% helper_save_plot(file_prefix = "ua_pas")  %>% print()
    })

# Test if the distribution of the K-FOLDS are normal.
# The data is normal if the p.value is above 0.05
help_print <- function(.data, var, msg){
    print(msg)
    var <- rlang::enquo(var)
    .data %>%
        dplyr::select(used_bands, !!var) %>%
        dplyr::filter(!!var > 0.05) %>%
        print(n = Inf)
    invisible(.data)
}
msg <- "KFOLDS OF COMBINATION OF BANDS WHICH ARE NORMALLY DISTRIBUTED."
kfold_ls %>%
    dplyr::bind_rows() %>%
    dplyr::select(-k_folds, -con_mat, -pasture_pa) %>%
    dplyr::group_by(used_bands) %>%
    dplyr::summarize_all(.funs = dplyr::funs(statistic = shapiro.test(.)$statistic,
                                             p.value = shapiro.test(.)$p.value)) %>%
    (function(x){
        write.csv(x, file = "shapiro_test.csv")
        invisible(x)
    }) %>%
    help_print(deforestation_pa_p.value, msg) %>%
    help_print(deforestation_ua_p.value, msg) %>%
    help_print(forest_pa_p.value, msg) %>%
    help_print(forest_ua_p.value, msg) %>%
    #help_print(pasture_pa_p.value, msg) %>%
    help_print(pasture_ua_p.value, msg)

# ANOVA test.
anova_test <- kfold_ls %>%
    dplyr::bind_rows() %>%
    dplyr::select(-k_folds, -con_mat) %>%
    (function(x){
         band_comb <- x %>%
             dplyr::pull(used_bands) %>%
             unique() %>%
             sort() %>%
             combn(m = 2) %>%
             t()
         differences_mt <- matrix(data = NA, nrow = nrow(band_comb),
                                  ncol = ncol(x))
         for(i in 1:nrow(band_comb)){
             for(j in 2:ncol(x)){
                 my_data <- x %>%
                     dplyr::filter(used_bands %in% band_comb[i, ]) %>%
                     dplyr::select(tidyselect::all_of(c(1, j))) %>%
                     magrittr::set_colnames(c("used_bands", "my_var"))
                 res <- aov(my_var ~ used_bands, data = my_data)
                 differences_mt[i, j] <- summary(res)[[1]]$'Pr(>F)'[[1]]
             }
         }
         differences_mt <- differences_mt %>%
             magrittr::set_colnames(stringr::str_c( stringr::str_sub(colnames(x), 1, 3),
                                                   stringr::str_sub(colnames(x), -3))) %>%
             tibble::as_tibble() %>%
             dplyr::select(-1)
         band_comb %>%
             magrittr::set_colnames(c("bands_1", "bands_2")) %>%
             tibble::as_tibble() %>%
             dplyr::bind_cols(differences_mt) %>%
             return()
    }) %>%
    (function(x){
        print("ANOVA")
        write.csv(x, file = "anova_test.csv")
        invisible(x)
    }) %>%
    print(n= Inf)

mean_differences <- kfold_ls %>%
    dplyr::bind_rows() %>%
    dplyr::select(-k_folds, -con_mat) %>%
    (function(x){
         band_comb <- x %>%
             dplyr::pull(used_bands) %>%
             unique() %>%
             sort() %>%
             combn(m = 2) %>%
             t()
         differences_mt <- matrix(data = NA, nrow = nrow(band_comb),
                                  ncol = ncol(x))
         for(i in 1:nrow(band_comb)){
             for(j in 2:ncol(x)){
                 my_data <- x %>%
                     dplyr::filter(used_bands %in% band_comb[i, ]) %>%
                     dplyr::select(tidyselect::all_of(c(1, j))) %>%
                     magrittr::set_colnames(c("used_bands", "my_var"))
                 res <- wilcox.test(my_var ~ used_bands, data = my_data)
                 differences_mt[i, j] <- res$p.value
             }
         }
         differences_mt <- differences_mt %>%
             magrittr::set_colnames(stringr::str_c( stringr::str_sub(colnames(x), 1, 3),
                                                   stringr::str_sub(colnames(x), -3))) %>%
             tibble::as_tibble() %>%
             dplyr::select(-1)
         band_comb %>%
             magrittr::set_colnames(c("bands_1", "bands_2")) %>%
             tibble::as_tibble() %>%
             dplyr::bind_cols(differences_mt) %>%
             return()
    }) %>%
    (function(x){
        print("VALUES LESS THAN 0.05 MEAN THERE ARE STATISTICAL SIGNIFICATIVE DIFFERENCES IN THE MEAN")
        write.csv(x, file = "mean_differences.csv")
        invisible(x)
    }) %>%
    print(n= Inf)

# NOTE: This assumes samples are taken from a normal distribution.
var_differences <- kfold_ls %>%
    dplyr::bind_rows() %>%
    dplyr::select(-k_folds, -con_mat) %>%
    (function(x){
         band_comb <- x %>%
             dplyr::pull(used_bands) %>%
             unique() %>%
             sort() %>%
             combn(m = 2) %>%
             t()
         differences_mt <- matrix(data = NA, nrow = nrow(band_comb),
                                  ncol = ncol(x))
         for(i in 1:nrow(band_comb)){
             for(j in 2:ncol(x)){
                 my_data <- x %>%
                     dplyr::filter(used_bands %in% band_comb[i, ]) %>%
                     dplyr::select(tidyselect::all_of(c(1, j))) %>%
                     magrittr::set_colnames(c("used_bands", "my_var"))
                 res <- var.test(my_var ~ used_bands, data = my_data)
                 differences_mt[i, j] <- res$p.value
             }
         }
         differences_mt <- differences_mt %>%
             magrittr::set_colnames(stringr::str_c( stringr::str_sub(colnames(x), 1, 3),
                                                   stringr::str_sub(colnames(x), -3))) %>%
             tibble::as_tibble() %>%
             dplyr::select(-1)
         band_comb %>%
             magrittr::set_colnames(c("bands_1", "bands_2")) %>%
             tibble::as_tibble() %>%
             dplyr::bind_cols(differences_mt) %>%
             return()
    }) %>%
    (function(x){
        print("VALUES LESS THAN 0.05 MEAN THERE ARE STATISTICAL SIGNIFICATIVE DIFFERENCES IN THE VARIANCE (assuming samples were taken from a normal distribution)")
        write.csv(x, file = "var_differences.csv")
        invisible(x)
    }) %>%
    print(n= Inf)

# Summarize the k-folds of the same bands using the mean and standard deviation.
kfold_ls %>%
    dplyr::bind_rows() %>%
    dplyr::select(-k_folds, -con_mat) %>%
    dplyr::group_by(used_bands) %>%
    dplyr::summarize(def_pa_mean = mean(deforestation_pa),
                     def_pa_sd   =   sd(deforestation_pa),
                     def_ua_mean = mean(deforestation_ua),
                     def_ua_sd   =   sd(deforestation_ua),
                     pas_pa_mean = mean(pasture_pa),
                     pas_pa_sd   =   sd(pasture_pa),
                     pas_ua_mean = mean(pasture_ua),
                     pas_ua_sd   =   sd(pasture_ua),
                     for_pa_mean = mean(forest_pa),
                     for_pa_sd   =   sd(forest_pa),
                     for_ua_mean = mean(forest_ua),
                     for_ua_sd   =   sd(forest_ua)) %>%
    dplyr::mutate(def_pa_min = def_pa_mean * (1 - 3 * def_pa_sd),
                  def_pa_max = def_pa_mean * (1 + 3 * def_pa_sd),
                  def_ua_min = def_ua_mean * (1 - 3 * def_ua_sd),
                  def_ua_max = def_ua_mean * (1 + 3 * def_ua_sd)) %>%
    dplyr::arrange(dplyr::desc(def_pa_mean),
                   dplyr::desc(def_ua_mean),
                   dplyr::desc(pas_pa_mean),
                   dplyr::desc(pas_ua_mean),
                   dplyr::desc(for_pa_mean),
                   dplyr::desc(for_ua_mean)) %>%
    (function(x){
        write.csv(x, file = "kfold.csv")
        invisible(x)
    }) %>%
    print(n = Inf)
