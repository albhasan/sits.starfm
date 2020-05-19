# COMPUTE THE CONFUSION MATRIX FROM THE VALIDATION SAMPLES.

library(caret)
library(dplyr)
library(raster)
library(sf)

source("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/util.R")

shp_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/validation_tudo_final4.shp"
classification_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5a/approx_v2"

# Get the predicted values from a classified raster for the given SF points.
get_predicted <- function(raster_path, point_sf){
    raster_path %>%
        raster::raster() %>%
        raster::extract(y = sf::as_Spatial(point_sf), sp = TRUE) %>%
        sf::st_as_sf() %>%
        sf::st_set_geometry(NULL) %>%
        magrittr::set_colnames(c("id", "reference", "predicted")) %>%
        dplyr::mutate(predicted = dplyr::recode(predicted,
                                                `1`   = "Deforestatio",
                                                `2`   = "Forest",
                                                `3`   = "Pasture",
                                                `102` = "Non-Forest",
                                                .default = NA_character_)) %>%
        dplyr::select(-id) %>%
        return()
}



test <- function(raster_path, point_sf){
    raster_path %>%
        raster::raster() %>%
        raster::extract(y = sf::as_Spatial(point_sf), sp = TRUE) %>%
        sf::st_as_sf() %>%
        add_coords() %>%
        sf::st_set_geometry(NULL) %>%
        magrittr::set_colnames(c("id", "reference", "predicted", "lon", "lat")) %>%
        dplyr::mutate(predicted = dplyr::recode(predicted,
                                                `1`   = "Deforestatio",
                                                `2`   = "Forest",
                                                `3`   = "Pasture",
                                                `102` = "Non-Forest",
                                                .default = NA_character_)) %>%
        dplyr::select(-id) %>%
        return()
}
all_sf_coords <- test(raster_path = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5a/approx_v2/blue-bnir-green-nnir-red-swir1-swir2/random-forest_1000/postprocessing_first.tif",
     point_sf = all_sf) %>%
     dplyr::filter(predicted == "Deforestatio",
                   reference == "Forest") %>%
     write.csv(file = "point_for_verification_all.csv")
indices_sf_coords <- test(raster_path = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5a/approx_v2/evi-ndmi-ndvi/random-forest_1000/postprocessing_first.tif",
     point_sf = index_sf) %>%
     dplyr::filter(predicted == "Deforestatio",
                   reference == "Forest") %>%
     write.csv(file = "point_for_verification_indices.csv")




# Validation points
validation_point_sf <- shp_file %>%
    sf::read_sf() %>%
    (function(x){
         print(table(x$label))
         print(sum(table(x$label)))
         stopifnot(!all(is.na(x$label)))
         return(x)
    }) %>%
    dplyr::select(-path, -count) %>%
    dplyr::filter(label != "Water") %>%
    sf::st_as_sf()

# Validation points of the classification using the bands.
all_sf <- validation_point_sf %>%
    dplyr::filter(layer == "validation_all_bands_v1") %>%
    dplyr::select(id, label)

# Validation points of the classification using indexes EVI, NDMI, NDVI.
index_sf <- validation_point_sf %>%
    dplyr::filter(layer == "validation_evi_ndmi_ndvi_v1") %>%
    dplyr::select(id, label)

# Classification maps
classification_tb <- classification_dir %>%
    get_results() %>%
    dplyr::filter(img_type == "postprocessing_first") %>%
    dplyr::filter(used_bands %in% c("blue-bnir-green-nnir-red-swir1-swir2",
                                    "evi-ndmi-ndvi"))

# Confusion matrix of the validation point of the classification using all the bands.
all_conmat <- classification_tb %>%
    dplyr::mutate(predicted_label = purrr::map(file_path, get_predicted, all_sf)) %>%
    dplyr::mutate(predicted_recoded = purrr::map(predicted_label, function(x){
                                                     x %>%
                                                         dplyr::mutate(reference = dplyr::recode(reference,
                                                                                                 NatNonForest = "NonForest"
                                                                                                 )) %>%
                                                     return()
                                    })) %>%
    dplyr::mutate(conmat_obj = purrr::map(predicted_recoded, sits::sits_conf_matrix)) %>%
    dplyr::mutate(conmat = purrr::map(conmat_obj, function(x){x$table}))

# Confusion matrix of the validation point of the classification using the indeces.
index_conmat <- classification_tb %>%
    dplyr::mutate(predicted_label = purrr::map(file_path, get_predicted, index_sf)) %>%
    dplyr::mutate(conmat_obj = purrr::map(predicted_label, sits::sits_conf_matrix)) %>%
    dplyr::mutate(conmat = purrr::map(conmat_obj, function(x){x$table}))

