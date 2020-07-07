# Convert coordinates of the patches from xy to lon lat

library(dplyr)
library(sf)

#a x 284048.244 y 8862407.214
#b x 247210.763 y 8887803.702
#c x 292346.107 y 8862030.038
#d x 284343     y 8857137

patches_tb <- tibble::tribble(~patch, ~x, ~y,
                              "a",  284048.244,  8862407.214,  
                              "b",  247210.763,  8887803.702,
                              "c",  292346.107,  8862030.038,
                              "d",  284343    ,  8857137)

patches <- patches_tb %>%  
    sf::st_as_sf(coords = c("x", "y"), crs = 32720) %>% 
    sf::st_transform(crs = 4326)

patches
