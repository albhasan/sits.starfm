my_poly <- tibble::tribble(
    ~group, ~lon, ~lat, 
    1,    -65.75,  -10,
    1,    -65.75,  -11,
    1,    -64.75,  -11,
    1,    -64.75,  -10)

ggplot2::map_data("world") %>% 
    dplyr::filter(region %in% c("Argentina", "Brazil", "Bolivia", "Chile", 
                                "Colombia",  "Ecuador", "French Guiana", 
                                "Guyana", "Paraguay", "Peru", "Suriname", 
                                "Uruguay", "Venezuela")) %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), 
                          fill = "white", colour = "grey50") + 
    ggplot2::geom_polygon(data = my_poly, ggplot2::aes(x = lon, y = lat, group = group), 
                          fill = "red", colour = "red") + 
    ggplot2::coord_fixed(xlim = c(-73, -35),  ylim = c(-35, 5), ratio = 1) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")