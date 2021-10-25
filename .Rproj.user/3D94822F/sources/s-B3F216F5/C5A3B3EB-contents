library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)

kpi_kreis <- list.files(path = "data_clean/", pattern = "kreise", full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(datum = str_extract(file, "[0-9]{8}"),
         datum = ymd(datum)) %>% 
  arrange(desc(datum)) %>% 
  pull(file) %>% 
  .[1] %>% 
  readRDS() %>% 
  mutate(kw_label = ifelse(nchar(kw_label) == 7, kw_label, str_replace_all(kw_label, "-", "-0")),
         values = as.numeric(values),
         week_date = as.Date(paste0(kw_label, "-1"), "%Y-%U-%u")) 

kreise_wide <- kpi_kreis %>% 
  pivot_wider(
    names_from = "category",
    values_from = "values",
    values_fill = 0
  ) %>% 
  mutate(students_perc = students_infected/students_total*100,
         teacher_perc = teacher_infected/teacher_total*100)



landkreise.shp <- raster::getData("GADM", country = "DEU", level = 2)
tidy_lk <- landkreise.shp %>% 
  broom::tidy(region = "CC_2")
bl.shp <- raster::getData("GADM", country = "DEU", level = 1) 
bl_tidy <- bl.shp%>% 
  broom::tidy(region = "CC_1")

geo_infected <- tidy_lk %>%
  left_join(
    kreise_wide %>%
      filter(!is.na(CC_2),
             !is.na(students_perc)),
    by = c("id" = "CC_2")
  ) %>% filter(!is.na(students_perc)) %>%
  mutate(kw_label = ifelse(nchar(kw_label) == 7, kw_label, str_replace_all(kw_label, "-", "-0")),
         kw_label = paste0("KW ", kw_label))


g_all <- ggplot() +
  geom_polygon(data = geo_infected ,
               mapping = aes(long, lat, group = group, fill = students_perc),
               color = "gray",
               size = 0.5) +
  scale_fill_viridis_c() +
  geom_polygon(data = bl_tidy, mapping = aes(long, lat, group = group),
               color = "black", fill = NA) +
  coord_quickmap() +
  theme_void() +
  facet_wrap(~kw_label, nrow = 3) +
  labs(fill = "Infizierte in %",
       title = "Infizierte Schüler in Deutschland",
       subtitle = "Datenquelle @ KWK") +
  theme(legend.position = "bottom",
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(g_all, filename = "leaflet_maps/gall.png", dpi = 300, scale = 4)
