library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)

lkr_files <- list.files(path = "data_raw/kreise/", pattern = "LKR", full.names = TRUE, recursive = TRUE)
heute_str <- gsub("-", "", Sys.Date())

# read_lkrggplot2read_lkr <- function(file) {
#   map(excel_sheets(file), read_excel, path = file) %>% 
#     set_names(excel_sheets(file))
# }

read_lkr <- function(file){
  list <- NULL
  for(i in seq_along(excel_sheets(file))) {
    # get kw
    # file <- lkr_files[11]
    # i <- 1
    kw <- read_excel(file, sheet = i) %>% .[1, 1] %>% pull()
    
    # get rid of comments
    first_comment_row <- read_excel(file, sheet = i, skip = 3) %>% 
      rename(index = 1) %>% 
      rownames_to_column("id") %>% 
      filter(grepl("zeile", index, ignore.case = TRUE)) %>% 
      .[1, 1] %>% 
      pull()
    
    list[[i]] <- read_excel(file, sheet = i, skip = 3) %>% 
      select(-contains("bitte")) %>% 
      rownames_to_column("id") %>% 
      slice(1:as.numeric(first_comment_row)-1) %>% 
      mutate_at(vars(contains("summen")), as.numeric) %>% 
      mutate_all(as.character) %>% 
      pivot_longer(
        cols = -c("id", "LK"),
        names_to = "key",
        values_to = "values"
      ) %>% 
      filter(!is.na(values)) %>% 
      mutate(woche = kw,
             bl = excel_sheets(file)[i], 
             file = file)
    
  }
  return(list)
}

df <- map_df(lkr_files, read_lkr) %>% 
  mutate(
    jahr = str_extract(woche, "[0-9]{4}"),
    woche = str_remove_all(woche, ". Kalenderwoche 2021"),
    kw_label = paste0(jahr, "-", woche)) %>% 
  select(-id)

### typo in kw 25 sheet
df <- df %>% 
  mutate(kw_label = ifelse(grepl("25", file) &
                           grepl("24", kw_label), "2021-25", kw_label)) 

# df %>% 
#   filter(grepl("Eisenach", key)) %>% 
#   filter(grepl("infizierte Schülerinnen|infizierte SuS", LK)) %>% 
#   select(kw_label, values) %>% 
#   ggplot(aes(kw_label, values)) +
#   geom_point() +
#   geom_line() +
#   #scale_x_date(labels = scales::date_format(format = "%b %d, %Y")) +
#   theme_light() +
#   labs(x = "Date",
#        y = "Proportion of infected students\nin %")

landkreise.shp <- raster::getData("GADM", country = "DEU", level = 2)
land_ccs <- landkreise.shp@data %>% 
  as_tibble() %>% 
  select(NAME_2, TYPE_2, CC_2) 

#### Example
df_cc <- df %>% 
  filter(!grepl("Summen", key),
         !grepl("0", key)) %>% 
  #select(bl, key) %>% distinct() %>% 
  mutate(NAME_2 = str_remove_all(key, "Stadt "),
         NAME_2 = str_remove_all(NAME_2, " \\(LKR\\)"),
         NAME_2 = str_remove_all(NAME_2, " \\(SKR\\)"),
         NAME_2 = str_remove_all(NAME_2, "[0-9]{3} - "),
         NAME_2 = str_remove_all(NAME_2, "LK "),
         NAME_2 = str_remove_all(NAME_2, "^Kreis "),
         NAME_2 = str_remove_all(NAME_2, "Landkreis "),
         NAME_2 = str_remove_all(NAME_2, "kreisfreie "),
         NAME_2 = str_remove_all(NAME_2, "Landeshauptstadt "),
         NAME_2 = str_remove_all(NAME_2, ", Stadt"),
         NAME_2 = str_remove_all(NAME_2, ", Landeshauptstadt"),
         NAME_2 = str_replace_all(NAME_2, "Ludwigshafen", "Ludwigshafen am Rhein"),
         NAME_2 = str_replace_all(NAME_2, "Landau", "Landau in der Pfalz"),
         NAME_2 = str_replace_all(NAME_2, "Nordachsen", "Nordsachsen"),
         NAME_2 = str_replace_all(NAME_2, "Salzland", "Salzlandkreis"),
         NAME_2 = str_replace_all(NAME_2, "Hzgt. Lauenburg", "Herzogtum Lauenburg"),
         NAME_2 = str_replace_all(NAME_2, "Schwäbisch-Hall", "Schwäbisch Hall"),
         NAME_2 = str_replace_all(NAME_2, "Mülheim a.d.Ruhr", "Mülheim an der Ruhr"),
         NAME_2 = str_replace_all(NAME_2, "Oberbergischen Kreis", "Oberbergischer Kreis"),
         NAME_2 = str_replace_all(NAME_2, "Rheinisch-Bergischen Kreis", "Rheinisch-Bergischer Kreis"),
         NAME_2 = str_replace_all(NAME_2, "Märkischen Kreis", "Märkischer Kreis"),
         NAME_2 = str_replace_all(NAME_2, "Altenkirchen", "Altenkirchen (Westerwald)"),
         NAME_2 = str_replace_all(NAME_2, "Frankenthal", "Frankenthal (Pfalz)"),
         NAME_2 = str_replace_all(NAME_2, "Neustadt a.d. Wstr.", "Neustadt an der Weinstraße"),
         NAME_2 = str_replace_all(NAME_2, "Saar-Pfalz-Kreis", "Saarpfalz-Kreis")) %>% 
  left_join(land_ccs) 

clean_kpi_kreise <- df_cc %>% 
  mutate(category = case_when(grepl("ohne Präsen", LK) ~ "schools_closed",
                         grepl("^Schulen mit eingeschränktem", LK) ~ "schools_limited",
                         grepl("Darunter: Schulen", LK) ~ "schools_limited_distance",
                         grepl("Lerngruppen an Schulen mit eingeschränktem", LK) ~ "schools_classes_distance",
                         grepl("einbezogene Verwaltungseinheiten", LK) ~ "schools_total",
                         grepl("infizierte SuS", LK) ~ "students_infected",
                         grepl("Quarantäne befindliche SuS", LK) ~ "students_quarantine",
                         grepl("einbezogene Schüler", LK) ~ "students_total",
                         grepl("infizierte Lehrkräfte", LK) ~ "teacher_infected",
                         grepl("Quarantäne befindliche Lehrkräfte", LK) ~ "teacher_quarantine",
                         grepl("einbezogene Lehrkräfte", LK) ~ "teacher_total",
                         TRUE ~ "invalid")) %>% 
  select(kw_label, category, key,  CC_2, values)

heute_str <- gsub("-", "", Sys.Date())
saveRDS(clean_kpi_kreise, file = paste0("data_clean/", heute_str, "_clean_kpi_kreise.rds"))
