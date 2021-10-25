library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(emojifont)
library(tidyr)
library(patchwork)
theme_set(theme_light())

my_theme <- theme(legend.position = "bottom",
                  strip.background = element_rect(fill = "#000075"),
                  strip.text = element_text(colour = 'white', face = "bold")) 

kpi_kreis <- list.files(path = "../data_clean/", pattern = "kreise", full.names = TRUE) %>% 
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

create_county_plot <- function(county) {
  data <- kpi_kreis %>% 
    filter(CC_2 == county) %>% 
    filter(grepl("infected|quarantine", category)) %>% 
    mutate(category = str_replace_all(category, "_", " "),
           category = str_to_title(category)) 
  
  county_name <- unique(data$key)
  
  data %>% 
    ggplot(aes(week_date, values, group = category)) +
    geom_line() +
    geom_point() +
    expand_limits(y = c(0, Inf)) +
    facet_wrap(~category, ncol = 2, scales = "free_y") +
    labs(subtitle = paste0("Kreis: ", county_name),
         title = "Kreis Übersicht",
         x = "Datum",
         y = "Anzahl") +
    my_theme
}



create_value_box_data <- function(county) {
  kpi_kreis %>% 
    filter(CC_2 == county) %>% 
    filter(grepl("infected|quarantine", category)) %>% 
    mutate(category = str_replace_all(category, "_", " "),
           category = str_to_title(category)) %>% 
    group_by(category) %>% 
    summarise(total = sum(values))
}

#create_value_box_data("08115")


create_value_boxes <- function(county) {
  df2 <- tibble(
    x = rep(seq(2, 15, 6.5), 2),
    y = c(rep(6.5, 3), rep(2,3)),
    h = rep(4.25, 6),
    w = rep(6.25, 6),
    value = c(create_value_box_data(county)$total, 0, 0),
    info = c("Gesamt infizierte\nSchüler bis heute",
             "Gesamt Schüler\nin Quarantäne",
             "Gesamt infizierte\nLehrkräfte bis heute",
             "Gesamt Lehrkräfte\nin Quarantäne",
             "KPI1",
             "KPI2"),
    icon = c(fontawesome(search_fontawesome("chart")),
             emoji("athletic_shoe")),
    font_family = c(rep("fontawesome-webfont", 5),
                    "EmojiOne"),
    color = factor(1:6)
  ) 
  
  ggplot(df2, aes(x, y, height = h, width = w, label = info)) +
    ## Create the tiles using the `color` column
    geom_tile(aes(fill = color)) +
    ## Add the numeric values as text in `value` column
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
    ## Add the labels for each box stored in the `info` column
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - 2.9, y = y - 1), hjust = 0, size = 4) +
    coord_fixed() +
    scale_fill_brewer(type = "qual",palette = "Dark2") +
    ## Use `geom_text()` to add the icons by specifying the unicode symbol.
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + 1.5, y = y + 0.5), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)
}


create_patch_plot <- function(county) {
  g1 <- create_value_boxes(county)
  g2 <- create_county_plot(county)

  g1 / g2 +
    plot_layout(width = unit(0.75*600, "points"))
}
#create_patch_plot("08115")
#create_patch_plot("16051")
