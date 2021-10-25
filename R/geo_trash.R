

%>% 
  pivot_wider(
    names_from = "ind",
    values_from = "values",
    values_fill = "0"
  ) %>% 
  mutate_at(vars(schools_distance:teacher_n), as.numeric) %>% 
  mutate(schools_perc = schools_distance/schools_n*100,
         student_perc = student_infected/student_n*100,
         teacher_perc = teacher_infected/teacher_n*100) %>% 
  separate(kw_label, c("year", "week"), remove = FALSE) %>% 
  mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) 



# infected <- df_cc %>% 
#   filter(grepl("infizierte SuS|Schüler insgesamt", LK)) %>% 
#   mutate(ind = ifelse(grepl("insgesamt", LK), "total", "n")) %>% 
#   select(land = key, values, kw_label, CC_2, ind) %>% distinct() %>% 
#   pivot_wider(
#     names_from = "ind",
#     values_from = "values",
#     values_fill = "0"
#   ) %>% 
#   mutate(n = as.numeric(n),
#          total = as.numeric(total),
#          perc = n/total*100) %>% 
#   separate(kw_label, c("year", "week"), remove = FALSE) %>% 
#   mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) 

tidy_lk <- landkreise.shp %>% 
  broom::tidy(region = "CC_2")
bl.shp <- raster::getData("GADM", country = "DEU", level = 1) 
bl_tidy <- bl.shp%>% 
  broom::tidy(region = "CC_1")

# geo_infected <- tidy_lk %>% 
#   left_join(
#     infected %>%
#       filter(!is.na(CC_2),
#              !is.na(perc)), 
#     by = c("id" = "CC_2")
#   ) %>% filter(!is.na(perc)) %>% 
#   mutate(kw_label = ifelse(nchar(kw_label) == 7, kw_label, str_replace_all(kw_label, "-", "-0")),
#          kw_label = paste0("KW ", kw_label))
# 
# # recent date
# ggplot() +
#   geom_polygon(data = geo_infected %>%
#                  filter(date == max(date)),
#                mapping = aes(long, lat, group = group, fill = perc),
#                color = "gray",
#                size = 0.5) +
#   scale_fill_viridis_c() +
#   geom_polygon(data = bl_tidy, mapping = aes(long, lat, group = group), 
#                color = "black", fill = NA) +
#   coord_quickmap() +
#   theme_void() +
#   facet_wrap(~kw_label, nrow = 2) +
#   labs(fill = "Infected in %",
#        title = "student infected in Germany",
#        subtitle = "Data Source @ KWK") +
#   theme(legend.position = "bottom")
# 



### leflet map
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(htmltools)
library(htmlwidgets)
km_data <- landkreise.shp
# km_data@data <- km_data@data %>% 
#   as_tibble() %>% 
#   left_join(infected %>%
#               filter(date == max(date), 
#                      !is.na(CC_2), 
#                      !is.na(perc)),
#             by = "CC_2") %>% 
#   distinct(CC_2, .keep_all = TRUE)


kmk_test <- landkreise.shp

test <- infected %>%
  # silly but easier to transform
  # account for that later
  rename(schools_infected = schools_distance) %>% 
  pivot_longer(cols = c("schools_infected", "schools_n", "student_infected", 
                        "student_n", "teacher_infected", "teacher_n"),
               names_to = "key",
               values_to = "values") %>% 
  select(-contains("perc")) %>% 
  separate(key, c("key", "ind")) %>% 
  pivot_wider(
    names_from = ind, 
    values_from = values, 
    values_fill = 0) %>% 
  group_by(CC_2, date, key) %>%
  ## fix: issue not distinct kreis label (e.g. grepl("Leipzig", land))
  summarise(infected = sum(infected, na.rm = TRUE),
            n = sum(n, na.rm = TRUE),
            perc = infected/n) %>% 
  group_by(CC_2) %>% 
  arrange(CC_2, date) %>% 
  #filter(CC_2 == "01001") %>% 
  mutate(student_recent = ifelse(date == max(date) & key == "student", perc, NA),
         teacher_recent = ifelse(date == max(date) & key == "teacher", perc, NA),
         school_recent = ifelse(date == max(date) & key == "schools", perc, NA)) %>% 
  fill(student_recent, .direction = "updown") %>%
  fill(teacher_recent, .direction = "updown") %>%
  fill(school_recent, .direction = "updown") %>%
  
  #filter(CC_2 %in% c("08111", "08125")) %>% 
  group_by(CC_2) %>% 
  nest(data = c(date, key, infected, n, perc)) %>%
  left_join(kmk_test@data %>% 
              select(CC_2, NAME_2)) %>% 
  rename(label = NAME_2)  %>% 
  mutate(label_map = paste0("<b>", label, "</b></br></br>Recent student infected: ", round(student_recent, 3),
                            "%</br>Recent teacher infected: ", round(teacher_recent, 3),
                            "%</br>Recent schools with distance learning: ", round(school_recent, 3), "%")) %>% 
  mutate(graph = map2(data, label,  ~ ggplot(
    data = .x %>% 
      mutate(key = case_when(key == "student" ~ "Infected students",
                             key == "schools" ~ "Schools with distance learning",
                             key == "teacher" ~ "Infected teacher",
                             TRUE ~ key)),
    aes(date, infected)) +
      geom_point() +
      geom_line() +
      facet_wrap(~key, ncol = 1, scales = "free_y") +
      labs(title = paste0("Landkreis: ", label),
           x = "",
           y = "Percentage") +
      #scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      expand_limits(y = c(0, Inf)) +
      theme_light() +
      my_theme))

### SAVING DATA
save(test, file = paste0("meta_data/", heute, "_meta.RData"))


kmk_test@data <- landkreise.shp@data %>% 
  as_tibble() %>% 
  left_join(test,
            by = "CC_2") 

pal <- colorNumeric(
  palette = "viridis",
  domain = kmk_test@data$student_recent,
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
  'KMK Dashboard - Infected student
  </br> Stand ', 
  format(Sys.Date(), "%d/%m%/%Y")
)
title <- tags$div(
  tag.map.title, HTML(title_string_new)
) 

pall <- colorNumeric(
  palette = "viridis",
  domain = kmk_test$student_recent[!is.na(kmk_test$student_recent)]
)

kmk_landing <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
  minZoom = 7,
  maxZoom = 9
)) %>%
  addPolygons(
    data = kmk_test,
    label = ~map(label_map, HTML),
    popup = "link", 
    smoothFactor = 0.000005,
    opacity = 1,
    fillColor = ~pal(student_recent),
    color = '#cccccc',
    weight = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(
      color = "#D23264",
      weight = 3,
      stroke = 4,
      bringToFront = TRUE,
      sendToBack = TRUE
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
  addControl(title, position = "topleft", className="map-title") %>% 
  addLegend(position = "bottomright",
            pal = pall, 
            values = kmk_test$student_recent[!is.na(kmk_test$student_recent)],
            title = "An Obvious Legend", na.label="")




kmk_landing
save_to <- "landing/"
heute <- gsub("-", "", Sys.Date())
save(kmk_landing, file = paste0(save_to, heute, "_kmk.RData "))

saveWidget(kmk_landing, 
           file = paste0(save_to, heute, "_kmk.html"), 
           knitrOptions = list(encoding = "UTF-8"), 
           selfcontained = TRUE,
           title = "KMK Landing Page")
