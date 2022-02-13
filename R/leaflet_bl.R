library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(htmltools)
library(htmlwidgets)



kpi_kreis <- list.files(path = "data_clean/", pattern = "clean_kpi_bl", full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(file = value) %>% 
  mutate(datum = str_extract(file, "[0-9]{8}"),
         datum = ymd(datum)) %>% 
  arrange(desc(datum)) %>% 
  pull(file) %>% 
  .[1] %>% 
  readRDS()

infected <- kpi_kreis %>% 
  filter(year(data_date) == 2021) %>% 
  pivot_wider(
    names_from = "category",
    values_from = "value",
    values_fill = 0
  ) %>% 
  separate(key, c("bl", "bl2"), remove = FALSE) %>% 
  # calculation of percent
  mutate(students_perc = students_infected/students_total*100,
         teacher_perc = teacher_infected/teacher_total*100,
         HASC_1 = paste0("DE.", toupper(bl2)),
         students_perc = ifelse(is.nan(students_perc), NA, students_perc),
         students_perc = ifelse(is.infinite(students_perc), NA, students_perc),
         teacher_perc = ifelse(is.nan(teacher_perc), NA, teacher_perc),
         teacher_perc = ifelse(is.infinite(teacher_perc), NA, teacher_perc))
  


# landkreise.shp <- raster::getData("GADM", country = "DEU", level = 2)
# tidy_lk <- landkreise.shp %>% 
#   broom::tidy(region = "CC_2")
bl.shp <- raster::getData("GADM", country = "DEU", level = 1) 
bl_tidy <- bl.shp %>% 
  broom::tidy(region = "CC_1")


kmk_data <- bl.shp

kmk_data@data <- bl.shp@data %>% 
  as_tibble() %>% 
  left_join(infected %>% 
              filter(data_date == max(data_date)) %>% 
              mutate(HASC_1 = str_replace_all(HASC_1, "DE.BB", "DE.BR")),
            by = "HASC_1") %>% 
  mutate(label = paste0(NAME_1, "\nInfiziert: ", round(students_perc, 4), "%"),
         label = ifelse(grepl("NA%$", label), paste0(NAME_1, " - Schule geschlossen"), label),
         label_teacher = paste0(NAME_1, "\nInfiziert: ", round(teacher_perc, 4), "%"),
         label_teacher = ifelse(grepl("NA%$", label_teacher), paste0(NAME_1, " - Schule geschlossen"), label_teacher))

heute_str <- gsub("-", "", Sys.Date())
saveRDS(kmk_data@data, file = paste0("data_clean/", heute_str, "_kmkdata_bl.rds"))


#### students
pal <- colorNumeric(
  palette = "YlOrRd",
  reverse = FALSE,
  domain = kmk_data@data$students_perc,
  na.color = "#ffffff")


## create javascript elements for plot
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
title_string_new <- paste0(
  'KMK Dashboard - Infected students
  </br> Stand ', 
  format(Sys.Date(), "%d/%m%/%Y")
)
title <- tags$div(
  tag.map.title, HTML(title_string_new)
) 

pall <- colorNumeric(
  palette = "YlOrRd",
  reverse = FALSE,
  domain = kmk_data$student_perc[!is.na(kmk_data$students_perc)]
)

kmk_students_bl <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
  minZoom = 6.2,
  maxZoom = 9
)) %>%
  addPolygons(
    data = kmk_data,
    #label = ~map(label_map, HTML),
    popup = ~label, 
    smoothFactor = 0.000005,
    opacity = 1,
    fillColor = ~pal(students_perc),
    color = '#cccccc',
    weight = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(
      color = "#D23264",
      weight = 3,
      stroke = 4,
      bringToFront = TRUE,
      sendToBack = FALSE
    )
  ) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  #addControl(title, position = "topleft", className="map-title") %>% 
  addLegend(position = "bottomright",
            pal = pall, 
            values = kmk_data$students_perc[!is.na(kmk_data$students_perc)],
            title = "% wöchentlich gemeldeter<br>Infektionen von SchülerInnen", na.label="")

#####################################################################################
#### teacher
pal <- colorNumeric(
  palette = "YlOrRd",
  reverse = FALSE,
  domain = kmk_data@data$teacher_perc,
  na.color = "#ffffff")


## create javascript elements for plot
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
title_string_new <- paste0(
  'KMK Dashboard - Infected teacher
  </br> Stand ', 
  format(Sys.Date(), "%d/%m%/%Y")
)
title <- tags$div(
  tag.map.title, HTML(title_string_new)
) 

pall <- colorNumeric(
  palette = "YlOrRd",
  reverse = FALSE,
  domain = kmk_data$teacher_perc[!is.na(kmk_data$teacher_perc)]
)

kmk_teacher_bl <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
  minZoom = 6.2,
  maxZoom = 9
)) %>%
  addPolygons(
    data = kmk_data,
    #label = ~map(label_map, HTML),
    popup = ~label_teacher, 
    smoothFactor = 0.000005,
    opacity = 1,
    fillColor = ~pal(teacher_perc),
    color = '#cccccc',
    weight = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(
      color = "#D23264",
      weight = 3,
      stroke = 4,
      bringToFront = TRUE,
      sendToBack = FALSE
    )
  ) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  #addControl(title, position = "topleft", className="map-title") %>% 
  addLegend(position = "bottomright",
            pal = pall, 
            values = kmk_data$teacher_perc[!is.na(kmk_data$teacher_perc)],
            title = "% wöchentlich gemeldeter<br>Infektionen von LehrerInnen", na.label="")

save(kmk_students_bl, kmk_teacher_bl, file = "leaflet_maps/recent_maps_bl.RData")
