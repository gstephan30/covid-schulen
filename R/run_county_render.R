library(dplyr)
library(stringr)
library(lubridate)
library(purrr)


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

kreise <- unique(kpi_kreis$CC_2[!is.na(kpi_kreis$CC_2)])

render_county <- function(county_code) {
  print(paste0("Writing: ", county_code))
  rmarkdown::render(
    input = "county_template/county_template.Rmd",
    params = list(
      county = county_code
    ),
    output_file = paste0("../county_report/", county_code, ".html")
  )
  
}

map(kreise, render_county)

