library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)

recent_forcasts <- list.files("data_clean/", pattern = "_student.csv|_teacher.csv", full.names = TRUE) %>%
  as_tibble() %>%
  mutate(woche = str_extract(value, "[0-9]{2}")) %>%
  arrange(desc(woche)) %>%
  slice(1:2) %>%
  pull(value)

forecast_students <- read_csv(grep("student", recent_forcasts, value = TRUE)) %>%
  mutate(source = "student")
forecast_teacher <- read_csv(grep("teacher", recent_forcasts, value = TRUE)) %>%
  mutate(source = "teacher")
time_kmk <- read_csv("data_clean/KMK_data.csv")
# combine both datasets
forecast <- bind_rows(forecast_students,
          forecast_teacher)


get_country <- function(country_code, plot_string) {
  #country_code <- "TH"
  
  plot_data <- forecast %>%
    mutate(key = paste0(type, "_", quantile),
           ind_week = nchar(target_week),
           target_week = ifelse(ind_week == 1, paste0("0", target_week), as.character(target_week)),
           target_week = paste0(target_year, "-", target_week)) %>%
    select(target_week, source, location, key, value) %>%
    filter(location == country_code) %>%
    mutate(value = as.numeric(value)) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    bind_rows(time_kmk %>% 
                filter(location == country_code) %>% 
                mutate(ind_week = nchar(week),
                       target_week = ifelse(ind_week == 1, paste0("0", week), as.character(week)),
                       target_week = paste0(year, "-", target_week)) %>% 
                pivot_longer(cols = c('teacher', "student"), names_to = "source", values_to = "point_NA") %>% 
                select(target_week, source,
                       location, point_NA) %>% 
                mutate(point_NA = ifelse(point_NA == "-", NA, point_NA),
                       point_NA = as.numeric(point_NA)))
  
  x_axis <- sort(plot_data$target_week)
  
  plot <- plot_data %>% 
    mutate(source = ifelse(source == "student", "Schüler", "Lehrkräfte"),
           source = fct_rev(source)) %>% 
    ggplot(aes(x = target_week, y = point_NA, group = 1)) +
    geom_ribbon(
      aes(ymin = quantile_0.025,
          ymax = quantile_0.975),
      alpha = 0.25,
      fill = "#27408b"
    ) +
    facet_wrap(~source, ncol = 1, scales = "free_y") +
    theme_light(base_size = 30) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
          strip.background = element_rect(fill = "#152238"),
          strip.text = element_text(colour = 'white', face = "bold")) +
    scale_x_discrete(breaks = x_axis[seq(1, length(x_axis), 5)]) +
      labs(x = "Kalenderwoche (KW)",
           y = "Anzahl der Infizierten",
           title = "Vorhersage infizierte Schüler und Lehrkräfte",
           subtitle = paste0("Bundesland: ", plot_string)) + 
    geom_ribbon(aes(ymin = quantile_0.25,
                    ymax = quantile_0.75),
                alpha = 0.25,
                fill = "#27408b") +
    geom_point(color = "#27408b", size = 3) +
    geom_line(color = "#27408b", size = 1)
  
  return(plot)
}

# get_country("BW", "Baden-Württemberg")
# get_country("TH", "Thüringen")
counties <- tribble(
  ~kurz, ~string,
  "BW", "Baden-Württemberg",
  "BY", "Bayern",
  "BE", "Berlin",
  "BB", "Brandenburg",
  "HB", "Bremen",
  "HH", "Hamburg",
  "HE", "Hessen",
  "NI", "Niedersachsen",
  "MV", "Mecklenburg-Vorpommern",
  "NW", "Nordrhein-Westfalen",
  "RP", "Rheinland-Pfalz",
  "SL", "Saarland",
  "SN", "Sachsen",
  "ST", "Sachsen-Anhalt",
  "SH", "Schleswig-Holstein",
  "TH", "Thüringen")

forecast_plots <- counties %>% 
  rowwise() %>% 
  mutate(plot = list(get_country(kurz, string)))
forecast_list <- forecast_plots$plot 
names(forecast_list) <- forecast_plots$kurz

save(forecast_list, file = "leaflet_maps/forecast.RData")
 
