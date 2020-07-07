library(dplyr)
library(sf)

# R sucks! 
# raster::clump returns the whole image belonging to the same label.
# Use gdal_polygonize.py instead.

data_tb <- tibble::tribble(
    ~Classification, ~file_path,
    "Bands",   "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/blue-bnir-green-nnir-red-swir1-swir2/Deforestatio-Forest-NonForest/random-forest_1000/bands_clumped.gpkg",
    "Indices", "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results9/approx/samples_B_approx_3l/evi-ndmi-ndvi/Deforestatio-Forest-NonForest/random-forest_1000/indices_clumped.gpkg"
) %>% 
    ensurer::ensure_that(all(file.exists(.$file_path)), err_desc = "Missing file!") %>% 
    mutate(sf = purrr::map(file_path, function(file_path){
        file_path %>% 
            sf::read_sf() %>% 
            dplyr::mutate(my_area = sf::st_area(.)) %>% 
            dplyr::mutate(label = dplyr::recode(.$DN, !!!c(`1` = "Deforestation",
                                                           `2` = "Forest",
                                                           `3` = "Non-Forest"))) %>% 
            sf::st_set_geometry(NULL) %>% 
            return()
        })) %>% 
    dplyr::select(-file_path) %>% 
    tidyr::unnest(cols = sf)

plot_data <- data_tb %>% 
    dplyr::mutate(area_ha = abs(as.numeric(my_area/(100*100))),
                  log_area = log10(area_ha),
                  bin = as.integer(floor(log_area))) %>% 
    dplyr::filter(area_ha != 0) %>% 
    dplyr::select(Classification, label, area_ha, bin) %>% 
    dplyr::group_by(Classification, label, bin) %>% 
    dplyr::summarise(Area = sum(area_ha)) %>% 
    dplyr::filter(label == "Deforestation")

# plot_data[["bin"]] <- factor(plot_data$bin, 
#                              levels = -2:5, 
#                              labels = paste("10", -2:5, sep = "^"),
#                              ordered = TRUE)

scientific_10 <- function(x) {
    parse(text = paste0("10^", x))
}

color_table <- tibble::tribble( ~Classification, ~Color,
                                "Bands",   "gray30",
                                "Indices", "gray60" ) 

plot_data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = bin, y = Area, fill = Classification), 
                      position = "dodge",
                      stat = "Identity",
                      color = "Grey") + 
    ggplot2::geom_text(data = dplyr::filter(plot_data, Classification == "Bands"), 
                       ggplot2::aes(x = bin, y = Area, label = round(Area, 1)), 
                       vjust = -0.5,
                       nudge_x = -0.25,
                       size = 5) +
    ggplot2::geom_text(data = dplyr::filter(plot_data, Classification == "Indices"), 
                       ggplot2::aes(x = bin, y = Area, label = round(Area, 1)), 
                       vjust = -0.5,
                       nudge_x = 0.25,
                       size = 5) +
    ggplot2::xlab("Deforestation patch size (ha)") + 
    ggplot2::ylab("Sum of area (ha)") +
    ggplot2::scale_x_continuous(label = scientific_10) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   text = element_text(size = 14)) + 
    ggplot2::scale_fill_manual(values = color_table$Color)

ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/deforestation_size_histogram.png")   
