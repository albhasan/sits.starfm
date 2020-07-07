suppressMessages(library(dplyr))
suppressMessages(library(sits))

#---- Util ----

source("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/util.R")


get_up_accuracy2 <- function(x, label, acc_type = "pa") {
    cm <- as.matrix(x$table)
    if (acc_type == "pa") {
        acc <- diag(cm)/colSums(cm)
    }else if (acc_type == "ua") {
        acc     <- diag(cm)/rowSums(cm)
    }else{return(NA)}
    return(acc[label])
}

helper_acc <- function(x, label, acc_type){
    sapply(x, get_up_accuracy2, label = label, acc_type = acc_type)
}



run_kfolds <- function(bands, samples_tb){
    lapply(1:100, function(i){
        samples_tb %>%
            sits::sits_sample(n = 42) %>%
            select_bands(bands) %>%
            sits::sits_kfold_validate(folds = 10,
                                      ml_method = sits::sits_rfor(num_trees = 1000)) %>%
            sits::sits_conf_matrix() %>%
            return()
    })
}

run_bootstrap <- function(bands, samples_tb){
    f <- function(){
        lapply(1:10, function(i){
            samples2_tb <- samples_tb %>%
                sits::sits_sample(n = 42) %>%
                select_bands(bands)
            test_tb <- samples2_tb %>%
                sits::sits_sample(n = 4)
            training_tb <- samples2_tb %>%
                dplyr::anti_join(test_tb, by = c("longitude", "latitude", "label",
                                                 "start_date", "end_date", "cube"))
            s_model <- training_tb %>%
                sits::sits_rfor(num_trees = 1000)
            test_tb %>%
                sits::sits_classify(ml_model = s_model) %>%
                dplyr::mutate(reference = label,
                              predicted = purrr::map_chr(predicted, function(x){
                                  x %>%
                                      dplyr::pull(class) %>%
                                      return()
                              })) %>%
                dplyr::select(reference, predicted) %>%
                return()
        })
    }
    lapply(1:100, function(x){
        f() %>%
            dplyr::bind_rows() %>%
            sits::sits_conf_matrix()
    }) %>%
        return()
}

run_bootstrap2 <- function(bands, samples_tb){
    f <- function(){
        lapply(1:10, function(i){
            test_tb <- samples_tb %>%
                sits::sits_sample(frac = 0.1)
            training_tb <- samples_tb %>%
                dplyr::anti_join(test_tb, by = c("longitude", "latitude", "label",
                                                 "start_date", "end_date", "cube"))
            s_model <- training_tb %>%
                sits::sits_rfor(num_trees = 1000)
            test_tb %>%
                sits::sits_classify(ml_model = s_model) %>%
                dplyr::mutate(reference = label,
                              predicted = purrr::map_chr(predicted, function(x){
                                  x %>%
                                      dplyr::pull(class) %>%
                                      return()
                              })) %>%
                dplyr::select(reference, predicted) %>%
                return()
        })
    }
    lapply(1:100, function(x){
        f() %>%
            dplyr::bind_rows() %>%
            sits::sits_conf_matrix()
    })%>%
        return()
}

#---- Script ----


experiment_type <- "bootstrap"
stopifnot(experiment_type %in% c("kfolds","bootstrap","bootstrap2"))

if (experiment_type == "kfolds") {
    experiment_f =  run_kfolds
}else if (experiment_type == "bootstrap") {
    experiment_f =  run_bootstrap
}else if (experiment_type == "bootstrap2") {
    experiment_f =  run_bootstrap2
}

experiment_tb <- tibble::tribble(
    ~brick_type, ~samples_file,
    "Cloud-filled time series", "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_approx_3l.rds",
    #"Clouded time series",      "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_raw_3l.rds"
) %>%
    dplyr::mutate(samples_tb = purrr::map(samples_file, readRDS),
                  samples_tb = purrr::map(samples_tb, dplyr::filter, label != "Water"),
                  bands = purrr::map(nrow(.), function(x){return(c("blue","bnir","green","nnir","red","swir2","swir2"))}),
                  indices = purrr::map(nrow(.), function(x){return(c("evi","ndmi","ndvi"))}))

experiment_tb <- experiment_tb %>%
    dplyr::mutate(kb_bands   = purrr::map2(bands,   samples_tb, experiment_f),
                  kb_indices = purrr::map2(indices, samples_tb, experiment_f))

experiment_tb <- experiment_tb %>%
    dplyr::mutate(def_pa_bands   = purrr::map(kb_bands,   helper_acc, label = "Deforestatio", acc_type = "pa"),
                  def_ua_bands   = purrr::map(kb_bands,   helper_acc, label = "Deforestatio", acc_type = "ua"),
                  for_pa_bands   = purrr::map(kb_bands,   helper_acc, label = "Forest",       acc_type = "pa"),
                  for_ua_bands   = purrr::map(kb_bands,   helper_acc, label = "Forest",       acc_type = "ua"),
                  nof_pa_bands   = purrr::map(kb_bands,   helper_acc, label = "NonForest",    acc_type = "pa"),
                  nof_ua_bands   = purrr::map(kb_bands,   helper_acc, label = "NonForest",    acc_type = "ua"),
                  def_pa_indices = purrr::map(kb_indices, helper_acc, label = "Deforestatio", acc_type = "pa"),
                  def_ua_indices = purrr::map(kb_indices, helper_acc, label = "Deforestatio", acc_type = "ua"),
                  for_pa_indices = purrr::map(kb_indices, helper_acc, label = "Forest",       acc_type = "pa"),
                  for_ua_indices = purrr::map(kb_indices, helper_acc, label = "Forest",       acc_type = "ua"),
                  nof_pa_indices = purrr::map(kb_indices, helper_acc, label = "NonForest",    acc_type = "pa"),
                  nof_ua_indices = purrr::map(kb_indices, helper_acc, label = "NonForest",    acc_type = "ua"))

experiment_tb <- experiment_tb %>%
    dplyr::select(-samples_file, -samples_tb, -bands,
                  -indices, -kb_bands, -kb_indices) %>%
    tidyr::unnest(c(def_pa_bands, def_ua_bands, for_pa_bands, for_ua_bands,
                    nof_pa_bands, nof_ua_bands, def_pa_indices, def_ua_indices,
                    for_pa_indices, for_ua_indices, nof_pa_indices,
                    nof_ua_indices)) %>%
    tidyr::pivot_longer(def_pa_bands:nof_ua_indices) %>%
    dplyr::mutate(experiment = dplyr::case_when(stringr::str_detect(name, "bands")   ~ "Bands",
                                                stringr::str_detect(name, "indices") ~ "Indices"),
                  accuracy_type = dplyr::case_when(stringr::str_detect(name, "_pa_") ~ "Producer accuracy",
                                                   stringr::str_detect(name, "_ua_") ~ "User accuracy")) %>%
    dplyr::mutate(name = purrr::map_chr(name, function(x){
        paste(unlist(stringr::str_split(x, '_'))[1],
              collapse = '_')
    })) %>%
    dplyr::mutate(label = dplyr::recode(name,
                                       !!!c( "def" = "Deforestation",
                                             "for" = "Forest",
                                             "nof" = "Non-Forest"))) %>%
    dplyr::mutate(name = label)

experiment_tb %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = value, y = experiment)) +
    ggplot2::xlab('Accuracy') +  ggplot2::ylab(' ') +
    ggplot2::facet_grid(label ~ accuracy_type)

if (experiment_type == "kfolds") {
    ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/kfolds_samples.png")
}else if (experiment_type == "bootstrap") {
    ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/bootstrap_samples.png")
    saveRDS(experiment_tb, file = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/bootstrap_samples.rds")
    experiment_tb %>%
        dplyr::group_by(experiment, accuracy_type, label) %>%
        dplyr::summarise(median(value)) %>%
        readr::write_csv("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/bootstrap_samples_median.csv")
}else if (experiment_type == "bootstrap2") {
    ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/bootstrap2_samples.png")
}


