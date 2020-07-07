stop("Do NOT run. It will overwrite hand-labeled files.")
suppressMessages(library(dplyr))
suppressMessages(library(raster))
suppressMessages(library(sf))

set.seed(666)

# Sample 70 random points for all_bands classification inside the label Deforestation
# Sample 72 random points for indices   classification inside the label Deforestation

source("/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/util.R")

#classification_bands <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000/postprocessing_first.tif"
#classification_indices <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/postprocessing_first.tif"
classification_bands <-   "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000/sentinel-bricks_probs_class_bayesian_2018_8_2019_7_009.tif"
classification_indices <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/sentinel-bricks_probs_class_bayesian_2018_8_2019_7_009.tif"
samples_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_A_approx_3l.rds"
stopifnot(all(sapply(c(classification_bands, classification_indices, samples_file), file.exists)))

# Read a text file of labels used during a sits classification.
read_label <- function(x){
    x %>%
        scan(what = character(), quiet = TRUE) %>%
        #dplyr::recode(Deforestatio = "Deforestation",
        #              #Forest
        #              NonForest = "Non-Forest") %>%
        magrittr::set_names(as.character(1:length(.))) %>%
        return()
}

maps <- c(classification_bands, classification_indices) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(raster_file = value) %>%
    dplyr::mutate(label_file = file.path(dirname(raster_file),
                                         "sits_labels.txt"),
                  rds_file = samples_file) %>%
    ensurer::ensure_that(all(file.exists(.$label_file)),
                         err_desc = "Missing label file") %>%
    dplyr::mutate(raster_obj = purrr::map(raster_file, raster::raster),
                  label_vec  = purrr::map(label_file, read_label),
                  samples    = purrr::map(rds_file, readRDS)) %>%
    dplyr::mutate(ref_pred = purrr::pmap(dplyr::select(., samples, raster_obj, label_vec),
                                         get_ref_pred))

(n_samples <-  84 - table(maps$ref_pred[[1]]$predicted)["Deforestatio"])
maps$raster_obj[[1]] %>%
    raster::sampleStratified(size = 100, xy = TRUE, sp = TRUE, na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    magrittr::set_colnames(c("cell", "x", "y", "id_label", "geometry")) %>%
    dplyr::filter(id_label == 1) %>%
    ensurer::ensure_that(nrow(.) > n_samples) %>%
    ensurer::ensure_that(nrow(.) > 1) %>%
    dplyr::sample_n(size = n_samples) %>%
    #sf::write_sf("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/extra_samples_deforestation_bands.shp")

(n_samples <- 84 - table(maps$ref_pred[[2]]$predicted)["Deforestatio"])
maps$raster_obj[[2]] %>%
    raster::sampleStratified(size = 100, xy = TRUE, sp = TRUE, na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    magrittr::set_colnames(c("cell", "x", "y", "id_label", "geometry")) %>%
    dplyr::filter(id_label == 1) %>%
    ensurer::ensure_that(nrow(.) > n_samples) %>%
    ensurer::ensure_that(nrow(.) > 1) %>%
    dplyr::sample_n(size = n_samples) %>%
    #sf::write_sf("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/extra_samples_deforestation_indices.shp")

