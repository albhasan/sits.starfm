libaray(dplyr)
libaray(raster)

r <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/cloud_frequency.tif"

data <- r %>% 
    raster::raster() %>% 
    .[] %>% 
    table()

data %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    tibble::as_tibble() %>% 
    magrittr::set_colnames(c("Clouded", "Frequency")) %>%  
    ggplot2::ggplot() +
    ggplot2::geom_bar(mapping = aes(x = Clouded, y = Frequency), 
                      stat = "identity") + 
    ggplot2::theme(text = element_text(size = 22)) +
    ggplot2::xlab(" ") + 
    ggplot2::ylab(" ")
ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/cloud_histogram.png")   
