library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Excercise2

wildschwein <- read_delim("wildschwein_BE_2056.csv")

## Task1

wildschwein <- st_as_sf(wildschwein, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein

