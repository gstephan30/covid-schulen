library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)


kpi_bl <- list.files(path = "data_clean/", pattern = "kmkdata_bl.rds", full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(datum = str_extract(file, "[0-9]{8}"),
         datum = ymd(datum)) %>% 
  arrange(desc(datum)) %>% 
  pull(file) %>% 
  .[1] %>% 
  readRDS()

tab_students <- kpi_bl %>% 
  select(NAME_1, contains("students"), -students_perc) %>% 
  mutate_if(is.numeric, ~round(., 4)) %>% 
  set_names("Bundesland", "Infizierte Schüler", "Infizierte Schüler in %",
            "Schüler in Quaratäne", "Schüler in Quaratäne in %", "Gesamte Schüler") %>% 
  DT::datatable(options = list(pageLength = 16, dom = 'tip'))

tab_teacher <- kpi_bl %>% 
  select(NAME_1, contains("teacher"), -teacher_perc, -label_teacher) %>% 
  mutate_if(is.numeric, ~round(., 4)) %>% 
  set_names("Bundesland", "Infizierte Lehrkräfte", "Infizierte Lehrkräfte in %",
            "Lehrkräfte in Quaratäne", "Lehrkräfte in Quaratäne in %", "Gesamte Lehrkräfte") %>% 
  DT::datatable(options = list(pageLength = 16, dom = 'tip'))

save(tab_students, tab_teacher, file = "data_clean/current_tabs.RData")
