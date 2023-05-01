library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Toolskit

## difftime

now <- Sys.time()
later <- now + 10000

now
later

later - now

difference <- difftime(later, now, units =  "mins")
str(difference)

difference_num <- as.numeric(difftime(later, now, units =  "mins"))
str(difference_num)

## lead and lag

mynumbers <- 1:10

lead(mynumbers)

lead(mynumbers) - mynumbers

lead(mynumbers, n=2)

lag(mynumbers) #lag() is the opposite of the lead function

## mutate

wildschwein <- tibble(
  TierID = c(rep("Hans", 5), rep("Klara", 5)),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

wildschwein

wildschwein$timelag <- as.numeric(difftime(lead(wildschwein$DatetimeUTC), wildschwein$DatetimeUTC))
wildschwein <- mutate(wildschwein, diff = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC)))

wildschwein

## group by

wildschwein <- group_by(wildschwein, TierID)
wildschwein <- mutate(wildschwein, timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC)))


## Piping

wildschwein |> # Take wildschwein...
  group_by(TierID) |> # ...group it by TierID
  summarise( # Summarise the data...
    mean_timelag = mean(timelag, na.rm = T) # ...by calculating the mean timelag
  )

### Excercise2

wildschwein <- read_delim("wildschwein_BE_2056.csv")
wildschwein <- st_as_sf(wildschwein, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein

arrange(wildschwein, TierName, DatetimeUTC)

wildschwein <- wildschwein |>
  group_by(TierName) |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec")))

wildschwein$TierName |> unique() #drei Tiere
summary(wildschwein) # von 2014-08-22 bis 2015-07-27

ggplot(wildschwein, aes(DatetimeUTC, TierName))+ #keine grossen Lücken, Sabi am längsten, grosse Zeitspanne alle drei
  geom_point()

ggplot(wildschwein, aes(timelag/60)) +  #die meisten ca. 15min, aber auch 30, 45, 60, 75...
  geom_histogram(binwidth =1) +
  lims(x = c(0,5000/60)) +
  scale_y_log10()

wildschwein |>
  filter(DatetimeUTC < "2014-08-24") |>
  filter(TierName == "Sabi") |>
  ggplot(wildschwein, aes(x=DatetimeUTC, y=timelag, color=TierName))+
  geom_point()+
  geom_line()

N1 <- 1204752
N2 <- 1204863
E1 <- 2570409
E2 <- 2570402

sqrt((E1-E2)^2+(N1-N2)^2)

wildschwein <- wildschwein |> 
  group_by(TierName) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2))


wildschwein <- wildschwein |> 
  mutate(speed_ms = steplength/timelag)

hist(wildschwein$speed_ms, 100)
hist(log10(wildschwein$speed_ms), 100)

