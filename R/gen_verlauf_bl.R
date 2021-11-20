library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


bl_data <- list.files("data_clean/", pattern =  "_clean_kpi_bl.rds", full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(datum = str_extract(file, "[0-9]{8}"),
         datum = ymd(datum)) %>% 
  arrange(desc(datum)) %>% 
  pull(file) %>% 
  .[1] %>% 
  readRDS() %>% 
  filter(year(data_date) > 2020) %>% 
  pivot_wider(
    names_from = category,
    values_from = value
  ) %>% 
  mutate(students_perc = students_infected/students_total*100,
         teacher_perc = teacher_infected/teacher_total*100)
         
bl_data %>%
  mutate(date = as.Date(date, origin = "1900-01-01")) %>% readr::write_excel_csv2("kmk_bl-20211018.csv")

bl.shp <- raster::getData("GADM", country = "DEU", level = 1) 
bl_tidy <- bl.shp %>% 
  broom::tidy(region = "CC_1")

shape_data <- bl.shp@data %>% 
  as_tibble() %>% 
  select(CC_1, key = HASC_1, NAME_1) %>% 
  left_join(
    bl_data %>% 
      mutate(key = str_replace(key, "bl_", "DE."),
             key = str_to_upper(key),
             key = str_replace_all(key, "DE.BB", "DE.BR")) %>% 
      filter(grepl("DE.", key)) 
  ) %>% 
  mutate(date = as.Date(date, origin = "1899-01-01"))

perc_nan <- shape_data$students_perc[!is.nan(shape_data$students_perc)]
perc_inf <- perc_nan[!is.infinite(perc_nan)]
perc_na <- perc_inf[!is.na(perc_inf)]

g_bl_all <- ggplot() +
  geom_polygon(
    data = bl_tidy %>% 
      left_join(shape_data, by = c("id" = "CC_1")),
    aes(long, lat, group = group, fill = students_perc),
    color = "black") +
  coord_quickmap() +
  theme_void() +
  facet_wrap(~data_date, nrow = 3) +
  labs(fill = "Infizierte in %",
       title = "Infizierte Schüler in Deutschland",
       subtitle = "Datenquelle @KWK") +
  theme(legend.position = "bottom",
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
  scale_fill_gradient2(low = "#053061", high = "#67001f", mid = "#f7f7f7",
                       midpoint = max(perc_na)/2)

save(g_bl_all, file = "leaflet_maps/gall_bl.RData")
ggsave(g_bl_all, filename = "leaflet_maps/gall_bl.png", dpi = 300, scale = 4)  
