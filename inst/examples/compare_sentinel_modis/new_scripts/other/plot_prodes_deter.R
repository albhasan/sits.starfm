library(dplyr)
library(ggplot2)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/terrabrasilis"

prodes_tb <- base_path %>% 
    file.path("terrabrasilis_legal_amazon_10_5_2020_1591839231375.csv") %>% 
    readr::read_csv2(col_names = c("Year", "Area_km2"),
                     readr::cols(Year     = readr::col_integer(), 
                                 Area_km2 = readr::col_number()),
                     skip = 1) %>% 
    dplyr::mutate(Source = "PRODES", 
                  Month  = 8,
                  className = "deforestation",
                  numPol  = 0) %>% 
    dplyr::arrange(Year, Month) %>% 
    dplyr::select(Year, Month, className, Source, Area_km2, numPol)

deter_original_tb <- base_path %>% 
    file.path("deter-amz-aggregated-6-7-2020-5_58_19_PM.csv") %>% 
    readr::read_csv(col_names = TRUE,
                    readr::cols(year      = readr::col_character(),
                                month     = readr::col_double(),
                                area      = readr::col_double(),
                                uf        = readr::col_character(),
                                className = readr::col_character(),
                                numPol    = readr::col_integer())) %>% 
    dplyr::mutate(year = stringr::str_extract(year, pattern = "/[0-9]+"),
                  year = as.integer(stringr::str_sub(year, 2)),
                  Source = "DETER",
                  className = dplyr::recode(className, 
                                            "CICATRIZ_DE_QUEIMADA" = "deforestation",
                                            "CORTE_SELETIVO"       = "deforestation",
                                            "CS_DESORDENADO"       = "deforestation",
                                            "CS_GEOMETRICO"        = "deforestation",
                                            "DEGRADACAO"           = "deforestation",
                                            "DESMATAMENTO_CR"      = "deforestation",
                                            "DESMATAMENTO_VEG"     = "deforestation",
                                            "MINERACAO"            = "deforestation")) %>% 
    dplyr::rename(Year = "year",
                  Area_km2 = "area",
                  Month = "month") 

deter_tb <- deter_original_tb %>% 
    dplyr::group_by(Year, Month, className, Source) %>% 
    dplyr::summarise(Area_km2 = sum(Area_km2),
                     numPol = sum(numPol)) %>% 
    dplyr::arrange(Year, Month)

space <- function(x, ...) { 
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

plot_tb <- prodes_tb %>% 
    dplyr::bind_rows(deter_tb) %>% 
    dplyr::filter(Source == "PRODES") %>% 
    #dplyr::mutate(positive_trend = Area_km2 > dplyr::lag(Area_km2)) %>% 
    dplyr::mutate(id = dplyr::row_number(),
                  my_position = dplyr::case_when(id == 1       ~ "up" ,
                                                 id == nrow(.) ~ "up",
                                                 Area_km2 > dplyr::lag(Area_km2) ~ "up",
                                                 TRUE ~ "down")) %>% 
    dplyr::mutate(point = dplyr::case_when(Year == 1988L ~ TRUE,
                                           Year == 1991L ~ TRUE,
                                           Year == 1995L ~ TRUE,
                                           Year == 1997L ~ TRUE,
                                           Year == 2004L ~ TRUE,
                                           Year == 2008L ~ TRUE,
                                           Year == 2012L ~ TRUE,
                                           Year == 2019L ~ TRUE))

my_x_lab <- c(1988, seq(1990, 2015, by = 5), 2019)
my_x_lab <- c(1988, 1991, 1995, 1997, 2004, 2008, 2012, 2019)
plot_tb %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_area(ggplot2::aes(x = Year, y = Area_km2), fill = "#faa6a5") + 
    ggplot2::geom_line(ggplot2::aes(x = Year, y = Area_km2), size = 1, color = "red") + 
    ggplot2::geom_point(data = dplyr::filter(plot_tb, point == TRUE), ggplot2::aes(x = Year, y = Area_km2), shape = 16, size = 5, color = "red") +
    ggplot2::geom_point(data = dplyr::filter(plot_tb, point == TRUE), ggplot2::aes(x = Year, y = Area_km2), shape = 16, size = 3, color = "white") +
    ggplot2::geom_text(data  = dplyr::filter(plot_tb, point == TRUE, my_position == "up"), ggplot2::aes(x = Year, y = Area_km2, label = format(Area_km2, nsmall = 0, big.mark = ",")),
                       nudge_x = 0, 
                       nudge_y = 1000, 
                       check_overlap = TRUE) + 
    ggplot2::geom_text(data  = dplyr::filter(plot_tb, point == TRUE, my_position == "down"), ggplot2::aes(x = Year, y = Area_km2, label = format(Area_km2, nsmall = 0, big.mark = ",")),
                       nudge_x = 0, 
                       nudge_y = -1000, 
                       check_overlap = TRUE) + 
    ggplot2::geom_text(data  = dplyr::filter(plot_tb, point == TRUE, my_position == "right"), ggplot2::aes(x = Year, y = Area_km2, label = format(Area_km2, nsmall = 0, big.mark = ",")),
                       nudge_x = 2, 
                       nudge_y = 0, 
                       check_overlap = TRUE) + 
    ggplot2::geom_text(data  = dplyr::filter(plot_tb, point == TRUE, my_position == "left"), ggplot2::aes(x = Year, y = Area_km2, label = format(Area_km2, nsmall = 0, big.mark = ",")),
                       nudge_x = -2, 
                       nudge_y = 0, 
                       check_overlap = TRUE) + 
    ggplot2::scale_y_continuous(labels = space) + 
    ggplot2::scale_x_continuous(breaks = my_x_lab, labels = my_x_lab) + 
    ggplot2::xlab("Year") + 
    ggplot2::ylab("Area km2") + 
    ggplot2::labs(y = expression(Km^2)) + 
    ggplot2::theme(text = element_text(size = 14)) +
    ggplot2::ggsave(filename = "/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/deforestation_trend.png", 
                width = 9.6, height = 6)
