#!/usr/bin/Rscript

# Validate the results of a classification.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
      stop("This script takes 3 parameters: A raster file, a text file with labels,  and a shapefile.",
           call. = FALSE)
}

stop("alber")

suppressMessages(library(dplyr))
suppressMessages(library(raster))

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#raster_file <- args[[1]]
#label_file  <- args[[2]]
#shp_file    <- args[[3]]
#stopifnot(all(file.exists(raster_file, label_file, shp_file)))

#raster_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results7/approx-samples_A_approx/evi-ndmi-ndvi/random-forest_1000/sentinel-bricks_probs_class_bayesian_2018_8_2019_7_007.tif"
#label_file  <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results7/approx-samples_A_approx/evi-ndmi-ndvi/random-forest_1000/sits_labels.txt"
shp_file    <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/minibrick/samples_validation_minibrick/samples_validation_minibrick.shp"

get_ref_pred <- function(raster_file, validation_points, label_vec){
    if(length(label_vec) != length(unique(validation_points$label))){
        label_vec <- label_vec %>%
            dplyr::recode(#Deforestation
                          #Forest
                          NatNonForest = "Non-Forest",
                          #Non-Forest
                          Pasture = "Non-Forest") %>%
        unique() %>%
        magrittr::set_names(as.character(1:length(.)))
    }
    stopifnot(all(sort(label_vec) %in% sort(unique(validation_points$label))))
    raster_file %>%
        raster::raster() %>%
        raster::extract(y = sf::as_Spatial(validation_points), sp = TRUE) %>%
        sf::st_as_sf() %>%
        sf::st_set_geometry(NULL) %>%
        tibble::tibble() %>%
        magrittr::set_colnames(c("reference", "predicted")) %>%
        dplyr::mutate(predicted = as.character(predicted)) %>%
        dplyr::mutate(predicted = dplyr::recode(predicted, !!!label_vec)) %>%
        ensurer::ensure_that(sum(is.na(.$predicted)) == 0,
                             err_desc = "missing labels!") %>%
        return()
}

read_label <- function(x){
    x %>%
        scan(what = character(), quiet = TRUE) %>%
        dplyr::recode(Deforestatio = "Deforestation",
                      #Forest
                      NonForest = "Non-Forest") %>%
        magrittr::set_names(as.character(1:length(.))) %>%
        return()
}

read_validation_points <- function(x){
    x %>%
        sf::read_sf() %>%
        dplyr::select(label) %>%
        return()
}

results_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results7" %>%
    get_results() %>%
    dplyr::filter(img_type == "classification") %>%
    dplyr::mutate(label_file = file.path(dirname(file_path), "sits_labels.txt"),
                  shp_file = shp_file) %>%
    ensurer::ensure_that(all(file.exists(.$label_file)),
                         err_desc = "Missing label file") %>%
    dplyr::mutate(label_vec = purrr::map(label_file, read_label)) %>%
    dplyr::mutate(validation_points = purrr::map(shp_file,
                                                 read_validation_points)) %>%
    dplyr::rename(raster_file = file_path) %>%
    dplyr::mutate(ref_pred = purrr::pmap(dplyr::select(., raster_file,
                                                       validation_points,
                                                       label_vec),
                                         get_ref_pred)) %>%
    dplyr::mutate(con_mat = purrr::map(ref_pred, sits::sits_conf_matrix)) %>%
    dplyr::mutate(def_pa = purrr::map_dbl(con_mat, get_up_accuracy, label = "Deforestation", acc_type = "pa"),
                  def_ua = purrr::map_dbl(con_mat, get_up_accuracy, label = "Deforestation", acc_type = "ua"),
                  for_pa = purrr::map_dbl(con_mat, get_up_accuracy, label = "Forest", acc_type = "pa"),
                  for_ua = purrr::map_dbl(con_mat, get_up_accuracy, label = "Forest", acc_type = "ua"),
                  nfo_pa = purrr::map_dbl(con_mat, get_up_accuracy, label = "Non-Forest", acc_type = "pa"),
                  nfo_ua = purrr::map_dbl(con_mat, get_up_accuracy, label = "Non-Forest", acc_type = "ua"))

results_tb %>%
  dplyr::select(raster_file, used_method, used_bands, img_type, def_pa, def_ua, for_pa, for_ua, nfo_pa, nfo_ua) %>%
  write.csv("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results7/accuracies.csv") 

