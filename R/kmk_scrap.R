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
  
  current_files <- xlsx_files %>% basename() 
  downloaded_files <- list.files("data_raw/kmk.org/")
  
  new_files <- setdiff(current_files, downloaded_files)
 
  
  if (length(new_files)>0) {
    print("Downloading new file(s)...")
    
    new_download <- grep(new_files, xlsx_files, value = TRUE)
    
    # Downloading the data
    map(new_download, function(x)
      download.file(
        x,
        destfile = paste0("data_raw/kmk.org/",  basename(x)),
        mode = "wb"
      ))
  } else {
    print("No new data available.")
  }
  
  
}


