library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
#library(purrr)

# download new data
source("R/kmk_scrap.R")
scrap_kmk()

data_files <- list.files(path = "data_raw/kmk.org/", full.names = TRUE) %>% 
  grep("chulart", .,  value = TRUE, invert = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(week = str_remove_all(file, "Covid-19|Covid19|-|_|Werte|Zahlen|neu|2022|.xlsx|data/"),
         week = str_extract(week, "[0-9][0-9]|[0-9]"),
         week = as.numeric(week),
         year = ifelse(grepl("Zahlen|_AW.xlsx", file), 2020, ifelse(grepl("2022|01.xlsx", file), 2022, 2021))) %>%
  arrange(desc(year), desc(week))

## subset the data
grep_kpi <- function(file) {
  #file <- data_files[1, 1] %>% pull()
  print(paste0("Reading file: ", file))
  
  data <- file %>% 
    read_excel(skip = 3) %>% 
    filter(!is.na(Land),
           !grepl("Anmerk", Land))
  
  data_date <- data[1, ] %>% 
    select(-Land, -contains("summe"), -contains("bereinigt")) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "state",
      values_to = "date"
    ) %>% 
    mutate(date = format(as.Date(as.numeric(date), "1899-12-30"), "%Y-%m-%d")) %>% 
    count(date, sort = TRUE) %>% 
    .[1, 1] %>% 
    pull()
  
  kpi_data <- data %>% 
    select(Land, contains("Bereinigt"),
           bl_BW = starts_with("BW", ignore.case = FALSE),
           bl_BY = starts_with("BY", ignore.case = FALSE),
           bl_BE = starts_with("BE", ignore.case = FALSE),
           bl_BB = starts_with("BB", ignore.case = FALSE),
           bl_HB = starts_with("HB", ignore.case = FALSE),
           bl_HH = starts_with("HH", ignore.case = FALSE),
           bl_HE = starts_with("HE", ignore.case = FALSE),
           bl_MV = starts_with("MV", ignore.case = FALSE),
           bl_NI = starts_with("NI", ignore.case = FALSE),
           bl_NW = starts_with("NW", ignore.case = FALSE),
           bl_RP = starts_with("RP", ignore.case = FALSE),
           bl_SL = starts_with("SL", ignore.case = FALSE),
           bl_SN = starts_with("SN", ignore.case = FALSE),
           bl_ST = starts_with("ST", ignore.case = FALSE),
           bl_SH = starts_with("SH", ignore.case = FALSE),
           bl_TH = starts_with("TH", ignore.case = FALSE)) %>% 
    janitor::clean_names() %>% 
    #mutate(ind = lag(land)) %>% 
    #select(key = ind, 2, 3) %>% 
    #select(land, everything(), -ind) %>% 
    mutate(ind = ifelse(grepl("Anteil", land), NA, land),
           ind2 = case_when(grepl("ohne Präsen", land) ~ "schools_closed",
                            grepl("^Schulen mit eingeschränktem", land) ~ "schools_limited",
                            grepl("Darunter: Schulen", land) ~ "schools_limited_distance",
                            grepl("einbezogene Schulen", land) ~ "schools_total",
                            grepl("infizierte Schüler", land) ~ "students_infected",
                            grepl("Quarantäne befindliche Schüler", land) ~ "students_quarantine",
                            grepl("einbezogene Schüler", land) ~ "students_total",
                            grepl("infizierte Lehrkräfte", land) ~ "teacher_infected",
                            grepl("Quarantäne befindliche Lehrkräfte", land) ~ "teacher_quarantine",
                            grepl("einbezogene Lehrkräfte", land) ~ "teacher_total",
                            grepl("Stichtag", land) ~ "date",
                            TRUE ~ "invalid"),
           ind2 = ifelse(ind2 == "invalid", NA, ind2)) %>% 
    fill(ind2, .direction = "down") %>% 
    mutate(ind3 = ifelse(grepl("Anteil", land), paste0(ind2, "_perc"), ind2)) %>% 
    select(category = ind3, contains("bl_"), contains("bereinigt")) %>% 
    mutate_all(as.character) %>%
    pivot_longer(
      cols = -category,
      names_to = "key",
      values_to = "value"
    ) %>% 
    filter(!grepl("bereinigte", value)) %>% 
    filter(!(category == "date" & key == "bereinigter_anteil"))
  
  
  return(
    list(
      "data_date" = data_date,
      "kpi_data" = kpi_data  
    )
  )
  
}

df_kpi <- data_files %>% 
  rowwise() %>% 
  mutate(kpi = map(file, grep_kpi)) %>% 
  unnest_wider(kpi)

clean_kpi <- df_kpi %>% 
  unnest_wider(kpi_data) %>% 
  select(4:7) %>% 
  unnest(cols = c(category, key, value)) %>% 
  mutate(value = as.numeric(value)) 

heute_str <- gsub("-", "", Sys.Date())
saveRDS(clean_kpi, file = paste0("data_clean/", heute_str, "_clean_kpi_bl.rds"))
