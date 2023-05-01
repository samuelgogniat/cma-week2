library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Toolskit Demo

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
