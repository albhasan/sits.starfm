library(sits)

samples_B_approx_3l <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_B_approx_3l.rds"

samples <- samples_B_approx_3l %>% 
    readRDS() %>%
    (function(x){
        plot(x)
    })

table(samples$label) 

samples %>%
    tidyr::unnest(time_series) %>%
    dplyr::select(-longitude, -latitude, -start_date, -end_date, -cube) %>%
    dplyr::mutate(label = dplyr::recode(label,
                                        Deforestatio = "Deforestation",
                                        Forest = "Forest",
                                        NonForest = "Non-Forest")) %>%
    dplyr::rename(Label = "label") %>% 
    tidyr::pivot_longer(cols = blue:swir2, 
                        names_to = "Band", 
                        values_to = "Value") %>%
    dplyr::filter(Band %in% c("blue", "bnir", "green", "nnir", "red", "swir1", 
                              "swir2", "evi", "ndmi", "ndvi")) %>%
    ggplot2::ggplot() + 
    ggplot2::geom_point(ggplot2::aes(x = Index, y = Value), 
                        size = 0.1, color = "gray") + 
    ggplot2::geom_smooth(ggplot2::aes(x = Index, 
                                      y = Value, 
                                      color = Label)) + 
    ggplot2::facet_wrap(~ Band) + 
    ggplot2::theme(text = element_text(size = 14)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90),
                   legend.title = element_blank()) +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::xlab("") + ggplot2::ylab("") 
    ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/training_samples.png")   
