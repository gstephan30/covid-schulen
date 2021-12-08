# The COVID-Schulen Dashboard

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/423734106.svg)](https://zenodo.org/badge/latestdoi/423734106)
<!-- badges: end -->


This [Covid19 Schulen dashboard](https://hzi-braunschweig.github.io/covid-schulen/) provides an overview of reported infection of SARS-CoV-2 infections in German school. The dashboard is built with R.  
<br>
<img src="pictures/kmk.PNG" width="100%" />

**Data source**

The data source is the documentation of the [German Kultusministerium](https://www.kmk.org/dokumentation-statistik/statistik/schulstatistik/schulstatistische-informationen-zur-covid-19-pandemie.html), which is weekly updated. The data is scrapped with this [function](R/kmk_scrap.R).

**Data**

The raw data are excel files, containing information about infected pupils, infected teacher and quarantie on state (Bundesland) level.

**Used Packages**

  - Visualisation:
      + [ggplot2](https://ggplot2.tidyverse.org/)
      + [leaflet](https://cran.r-project.org/web/packages/leaflet/)
      + [DT](https://cran.r-project.org/web/packages/DT/)
      + [flexdashboard](https://pkgs.rstudio.com/flexdashboard/)
      
  - Data wrangling:
      + [purrr](https://purrr.tidyverse.org/)
      + [dplyr](https://dplyr.tidyverse.org/)
      + [stringr](https://stringr.tidyverse.org/)
      + [tidyr](https://tidyr.tidyverse.org/)
      + [lubridate](https://lubridate.tidyverse.org/)
      + [broom](https://broom.tidymodels.org/)
      + [forcats](https://forcats.tidyverse.org/)
      
  - Data scrapping & data import:
      + [rvest](https://rvest.tidyverse.org/)
      + [readr](readr)
      + [readxl](https://readxl.tidyverse.org/)
      
  - Helper:
      + [leaflet.extras](https://cran.r-project.org/web/packages/leaflet.extras/)
      + [leafpop](https://cran.r-project.org/web/packages/leafpop/)
      + [htmltools](https://cran.r-project.org/web/packages/htmltools/)
      + [htmlwidgets](https://cran.r-project.org/web/packages/htmlwidgets/)

**Deployment and reproducibly**

The dashboard is deployed via Github actions/Github pages.

For any question or feedback, you can either open an [issue](https://github.com/gstephan30/covid-schulen/issues) or look in the imprint/impressum of the dashboard for more contacts.


