# Sample the resulte.

library(raster)
library(sp)

res_evi_ndmi_ndvi <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2/evi-ndmi-ndvi/random-forest_1000/postprocessing_first_masked.tif" %>% 
    raster::raster() %>%
    raster::sampleStratified(size = 200, xy = TRUE, sp = TRUE, na.rm = TRUE) %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(id = 1:nrow(.))

res_all_bands <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/results5/approx_v2/blue-bnir-green-nnir-red-swir1-swir2/random-forest_1000/postprocessing_first_masked.tif" %>% 
    raster::raster() %>%
    raster::sampleStratified(size = 200, xy = TRUE, sp = TRUE, na.rm = TRUE) %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(id = 1:nrow(.))

table(res_evi_ndmi_ndvi$postprocessing_first_masked)
table(res_all_bands$postprocessing_first_masked)

s_forest <- res_evi_ndmi_ndvi %>% 
    dplyr::filter(postprocessing_first_masked == 2) %>% 
    dplyr::sample_n(size = 141)
s_pasture <- res_evi_ndmi_ndvi %>% 
    dplyr::filter(postprocessing_first_masked == 3) %>% 
    dplyr::sample_n(size = 141)
res_evi_ndmi_ndvi %>% 
    dplyr::filter(postprocessing_first_masked == 1) %>% 
    rbind(s_forest) %>% 
    rbind(s_pasture) %>% 
    #dplyr::select(-postprocessing_first_masked, -cell, -x, -y) %>% 
    sf::write_sf("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/secret_validation_evi_ndmi_ndvi_v1.shp")

s_forest <- res_all_bands %>% 
    dplyr::filter(postprocessing_first_masked == 2) %>% 
    dplyr::sample_n(size = 141)
s_pasture <- res_all_bands %>% 
    dplyr::filter(postprocessing_first_masked == 3) %>% 
    dplyr::sample_n(size = 141)
res_all_bands %>% 
    dplyr::filter(postprocessing_first_masked == 1) %>% 
    rbind(s_forest) %>% 
    rbind(s_pasture) %>% 
    #dplyr::select(-postprocessing_first_masked, -cell, -x, -y) %>% 
    sf::write_sf("/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/secret_validation_all_bands_v1.shp")

