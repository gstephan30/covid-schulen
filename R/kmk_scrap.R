library(rvest)
library(purrr)

scrap_kmk <- function() {
  # Scrapping the data
  link <- "https://www.kmk.org/dokumentation-statistik/statistik/schulstatistik/schulstatistische-informationen-zur-covid-19-pandemie.html"
  
  xlsx_files <- read_html(link) %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    grep("xlsx", ., value = TRUE) %>% 
    paste0("https://www.kmk.org", .)
  
  # Downloading the data
  map(xlsx_files, function(x) download.file(x, destfile = paste0("data_raw/kmk.org/",  basename(x)), mode = "wb"))
  
  ## TODO stop downloading files, already downloaded
}


