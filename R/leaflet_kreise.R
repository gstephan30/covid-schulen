library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(htmltools)
library(htmlwidgets)


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

infected <- kpi_kreis %>% 
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


kmk_data <- landkreise.shp

kmk_data@data <- landkreise.shp@data %>% 
  as_tibble() %>% 
  left_join(infected %>% 
              filter(week_date == max(week_date)),
            by = "CC_2") %>% 
  mutate(plot_link = paste0("county_report/", CC_2, ".html"),
         plot_url = paste0('<a href = "', plot_link, '"></a>'))

heute_str <- gsub("-", "", Sys.Date())
saveRDS(kmk_data@data, file = paste0("data_clean/", heute_str, "_kmkdata.rds"))


#### students
pal <- colorNumeric(
  palette = "viridis",
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
  palette = "viridis",
  domain = kmk_data$student_perc[!is.na(kmk_data$students_perc)]
)

kmk_students <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
  minZoom = 6.2,
  maxZoom = 9
)) %>%
  addPolygons(
    data = kmk_data,
    #label = ~map(label_map, HTML),
    popup = ~HTML(plot_url), 
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
  addPolylines(
    data = bl.shp,
    col = '#0e1111',
    weight = 3,
    opacity = 0.8,
    stroke = TRUE
  ) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  #addControl(title, position = "topleft", className="map-title") %>% 
  addLegend(position = "bottomright",
            pal = pall, 
            values = kmk_data$students_perc[!is.na(kmk_data$students_perc)],
            title = "Infected Students\nin %", na.label="")

#####################################################################################
#### teacher
pal <- colorNumeric(
  palette = "viridis",
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
  palette = "viridis",
  domain = kmk_data$teacher_perc[!is.na(kmk_data$teacher_perc)]
)

kmk_teacher <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
  minZoom = 6.2,
  maxZoom = 9
)) %>%
  addPolygons(
    data = kmk_data,
    #label = ~map(label_map, HTML),
    popup = ~HTML(plot_url), 
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
  addPolylines(
    data = bl.shp,
    col = '#0e1111',
    weight = 3,
    opacity = 0.8,
    stroke = TRUE
  ) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  #addControl(title, position = "topleft", className="map-title") %>% 
  addLegend(position = "bottomright",
            pal = pall, 
            values = kmk_data$teacher_perc[!is.na(kmk_data$teacher_perc)],
            title = "Infected Teacher\nin %", na.label="")

save(kmk_students, kmk_teacher, file = "leaflet_maps/recent_maps_kreise.RData")