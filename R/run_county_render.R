library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)


# bundesland merge daten
bl.shp <- raster::getData("GADM", country = "DEU", level = 1) 
bl_tidy <- bl.shp%>% 
  broom::tidy(region = "CC_1")

# cleaning
kpi_bl <- list.files(path = "data_clean/", pattern = "_clean_kpi_bl.rds", full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(datum = str_extract(file, "[0-9]{8}"),
         datum = ymd(datum)) %>% 
  arrange(desc(datum)) %>% 
  pull(file) %>% 
  .[1] %>% 
  readRDS() %>% 
  filter(year(data_date) >= 2021) %>% 
  pivot_wider(
    names_from = "category",
    values_from = "value",
    values_fill = 0
  ) %>% 
  separate(key, c("bl", "bl2"), remove = FALSE) %>% 
  mutate(students_perc = students_infected/students_total*100,
         teacher_perc = teacher_infected/teacher_total*100,
         HASC_1 = paste0("DE.", toupper(bl2)),
         students_perc = ifelse(is.nan(students_perc), NA, students_perc),
         students_perc = ifelse(is.infinite(students_perc), NA, students_perc),
         teacher_perc = ifelse(is.nan(teacher_perc), NA, teacher_perc),
         teacher_perc = ifelse(is.infinite(teacher_perc), NA, teacher_perc),
         date = format(as.Date(as.numeric(date), "1899-12-30"), "%Y-%m-%d"),
         date = ymd(date)) %>% 
  left_join(
    bl.shp@data %>% 
      as_tibble() %>% 
      select(NAME_1, HASC_1) %>% 
      mutate(bl2 = str_extract(HASC_1, ".[A-Z]{2}"),
             bl2 = str_remove(bl2, "."),
             bl2 = tolower(bl2),
             bl2 = ifelse(bl2 == "br", "bb", bl2)) %>% 
      select(bl2, bl_name = NAME_1)
  )


tab_students <- kpi_bl %>% 
  select(bl_name, data_date, contains("students"), -students_perc) %>% 
  filter(!is.na(bl_name)) %>% 
  mutate_if(is.numeric, ~round(., 4)) %>% 
  set_names("Bundesland", "Woche", "Infizierte Schüler", "Infizierte Schüler in %",
            "Schüler in Quaratäne", "Schüler in Quaratäne in %", "Gesamte Schüler") %>% 
  DT::datatable(extensions = 'Buttons', options = list(pageLength = 16, 
                                                       dom = 'Bfrtip',
                                                       buttons = c('csv', 'excel')))

tab_teacher <- kpi_bl %>% 
  select(bl_name, data_date, contains("teacher"), -teacher_perc) %>% 
  mutate_if(is.numeric, ~round(., 4)) %>% 
  set_names("Bundesland", "Woche", "Infizierte Lehrkräfte", "Infizierte Lehrkräfte in %",
            "Lehrkräfte in Quaratäne", "Lehrkräfte in Quaratäne in %", "Gesamte Lehrkräfte") %>% 
  DT::datatable(extensions = 'Buttons', options = list(pageLength = 16, 
                               dom = 'Bfrtip',
                               buttons = c('csv', 'excel')))

save(tab_students, tab_teacher, file = "data_clean/current_tabs.RData")
