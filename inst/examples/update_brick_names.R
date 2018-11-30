# update name of brick to new rules

# brick_imgs_233067_2017.Rdata 2016-08-10
# brick_imgs_225063_2017.Rdata 2016-08-02
# brick_imgs_225063_2014.Rdata 2013-08-10
# brick_imgs_226064_2017.Rdata 2016-08-09

yd_tb <- tibble::tribble(
    ~scene, ~year,  ~first_image,
    225063, 2013, "2013-08-10",
    225063, 2014, "2014-08-13",
    225063, 2015, "2015-08-16",
    225063, 2016, "2016-08-02",
    225063, 2017, "2017-08-21",

    226064, 2013, "2013-08-01",
    226064, 2014, "2014-08-04",
    226064, 2015, "2015-08-07",
    226064, 2016, "2016-08-09",
    226064, 2017, "2017-07-27",

    233067, 2013, "2013-08-02",
    233067, 2014, "2014-08-05",
    233067, 2015, "2015-08-08",
    233067, 2016, "2016-08-10",
    233067, 2017, "2017-08-13"
)

library(tidyverse)
library(sits.starfm)


sname <- SPECS_L8_SR$short_name %>% stringr::str_replace(" ", "_") %>% tolower()
names(sname) <- SPECS_L8_SR$band_designation

rm(brick_files)
brick_files <- "/home/alber/shared/brick" %>%
    list.files(pattern = "^brick_LC08MOD_[0-9]{6}_[0-9]{4}_.*[.]tif$", full.names = TRUE) %>%
    dplyr::as_tibble() %>% dplyr::mutate(
        scene = as.numeric(stringr::str_extract(basename(value), "[0-9]{6}")),
        band = sits.starfm::get_landsat_band(value),
        year = as.numeric(stringr::str_sub(stringr::str_extract(basename(value), "_[0-9]{4}_"), 2, 5)) - 1,
        short_name = sname[band]
    ) %>% dplyr::filter(!is.na(short_name)) %>%
    dplyr::left_join(yd_tb, by = c("scene", "year")) %>%
    dplyr::mutate(fn1 = file.path(dirname(value), paste("LC8SR-MOD13Q1-STARFM", scene, first_image, short_name, "STACK_BRICK.tif", sep = "_"))) %>%
    dplyr::select(value, fn1) %>%
    dplyr::mutate(
        cmd = paste("mv", value, fn1)
        )

brick_files %>% as.data.frame() %>% head()

brick_files$cmd
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2014_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2015_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2014-08-13_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2015_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2014-08-13_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2015_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2014-08-13_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2015_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2014-08-13_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2017_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2016-08-02_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2017_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2016-08-02_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2017_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2016-08-02_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_225063_2017_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2016-08-02_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2015_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2014-08-04_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2015_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2014-08-04_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2015_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2014-08-04_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2015_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2014-08-04_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2016_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2015-08-07_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2016_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2015-08-07_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2016_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2015-08-07_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2016_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2015-08-07_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2017_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2016-08-09_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2017_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2016-08-09_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2017_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2016-08-09_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_226064_2017_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_226064_2016-08-09_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2015_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2015_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2015_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2015_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2016_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2016_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2016_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2016_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_swir2_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band2.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_blue_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band4.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_red_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band5.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_nir_STACK_BRICK.tif
# mv /home/alber/shared/brick/brick_LC08MOD_233067_2017_sr_band7.tif /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_swir2_STACK_BRICK.tif
#
# mv ./brick_LC08MOD_225063_2014_ndvi.tif ./LC8SR-MOD13Q1-STARFM_225063_2013-08-10_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_225063_2015_ndvi.tif ./LC8SR-MOD13Q1-STARFM_225063_2014-08-13_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_225063_2017_ndvi.tif ./LC8SR-MOD13Q1-STARFM_225063_2016-08-02_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_225063_2014_savi.tif ./LC8SR-MOD13Q1-STARFM_225063_2013-08-10_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_225063_2015_savi.tif ./LC8SR-MOD13Q1-STARFM_225063_2014-08-13_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_225063_2017_savi.tif ./LC8SR-MOD13Q1-STARFM_225063_2016-08-02_savi_STACK_BRICK.tif
#
# mv ./brick_LC08MOD_226064_2015_ndvi.tif ./LC8SR-MOD13Q1-STARFM_226064_2014-08-04_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_226064_2016_ndvi.tif ./LC8SR-MOD13Q1-STARFM_226064_2015-08-07_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_226064_2017_ndvi.tif ./LC8SR-MOD13Q1-STARFM_226064_2016-08-09_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_226064_2015_savi.tif ./LC8SR-MOD13Q1-STARFM_226064_2014-08-04_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_226064_2016_savi.tif ./LC8SR-MOD13Q1-STARFM_226064_2015-08-07_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_226064_2017_savi.tif ./LC8SR-MOD13Q1-STARFM_226064_2016-08-09_savi_STACK_BRICK.tif
#
# mv ./brick_LC08MOD_233067_2015_ndvi.tif ./LC8SR-MOD13Q1-STARFM_233067_2014-08-05_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_233067_2016_ndvi.tif ./LC8SR-MOD13Q1-STARFM_233067_2015-08-08_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_233067_2017_ndvi.tif ./LC8SR-MOD13Q1-STARFM_233067_2016-08-10_ndvi_STACK_BRICK.tif
# mv ./brick_LC08MOD_233067_2015_savi.tif ./LC8SR-MOD13Q1-STARFM_233067_2014-08-05_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_233067_2016_savi.tif ./LC8SR-MOD13Q1-STARFM_233067_2015-08-08_savi_STACK_BRICK.tif
# mv ./brick_LC08MOD_233067_2017_savi.tif ./LC8SR-MOD13Q1-STARFM_233067_2016-08-10_savi_STACK_BRICK.tif
