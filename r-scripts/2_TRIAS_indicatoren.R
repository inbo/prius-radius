---
title: "TRIAS_indicators"
author: "Jasmijn"
date: '2022-08-17'
output: html_document
---

```{r setup, include=FALSE}
#SETUP
library(tidyverse) # To do data science
library(tidylog) # To provide feedback on dplyr functions
library(progress) # To add progress bars
library(here) # To find files
library(lubridate) # To work with dates
library(rgbif) # To get taxa from publisehd unified checklist
```


```{r cars}
#get alien occurence data
df <- read_csv(
  file = "./data/input/be_species_cube.csv",
  col_types = cols(
    year = col_double(),
    eea_cell_code = col_character(),
    speciesKey = col_double(),
    n = col_double(),
    min_coord_uncertainty = col_double()
  ),
  na = ""
)

df <-
  df %>%
  select(-min_coord_uncertainty)

df <-
  df %>%
  rename(obs = n,
         taxonKey = speciesKey)

#baseline
df_bl <- read_csv(
  file = "https://raw.githubusercontent.com/trias-project/occ-cube-alien/master/data/processed/be_classes_cube.csv",
  col_types = cols(
    year = col_double(),
    eea_cell_code = col_character(),
    classKey = col_double(),
    n = col_double(),
    min_coord_uncertainty = col_double()
  ),
  na = ""
)

df_bl <-
  df_bl %>%
  select(-min_coord_uncertainty)

df_bl <-
  df_bl %>%
  rename(cobs = n)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
