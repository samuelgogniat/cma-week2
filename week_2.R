library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Excercise2

wildschwein <- read_delim("wildschwein_BE_2056.csv")

## Task1

wildschwein <- st_as_sf(wildschwein, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein

## Task2

wildschwein <- arrange(wildschwein, TierName, DatetimeUTC)

wildschwein <- wildschwein |>
  group_by(TierName) |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec")))

wildschwein$TierName |> unique() 
#drei Tiere: Rosa, Ruth, Sabi

summary(wildschwein)
# Messung insgesamt von 2014-08-22 bis 2015-07-27, timelag zwischen 12s und 916s

ggplot(wildschwein, aes(DatetimeUTC, TierName))+ 
  geom_point()
#keine Lücken erkennbar, Sabi am längsten, von dez 2014 bis jul 2015 Daten von allen drei

ggplot(wildschwein, aes(timelag/60)) +  
  geom_histogram(binwidth =1) +
  lims(x = c(0,5000/60)) +
  scale_y_log10()
#timelag am häufigsten ca. 15min, aber auch hohe Zahlen bei 30min, 45min, 60min -> auf jeden Fall unregelmässig

wildschwein |>
  filter(DatetimeUTC < "2014-08-24") |>
  filter(TierName == "Sabi") |>
  ggplot(aes(x=DatetimeUTC, y=timelag/60, color=TierName))+
  geom_point()+
  geom_line()
#timelags während des Tages grössser als in der Nacht -> Tiere schlafen dann, deshalb wurde Messung so eingestellt, dass insbesondere in der aktiven Zeit gemessen wird

