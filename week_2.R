library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(zoo)

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

## Task3

N1 <- 1204752
N2 <- 1204863
E1 <- 2570409
E2 <- 2570402

sqrt((E1-E2)^2+(N1-N2)^2)

wildschwein <- wildschwein |> 
  group_by(TierName) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2))

hist(wildschwein$steplength, 100)
hist(log10(wildschwein$steplength), 100)

wildschwein <- wildschwein |> 
  mutate(speed_ms = steplength/timelag)

hist(wildschwein$speed_ms, 100)
hist(log10(wildschwein$speed_ms), 100)

#Die Einheit ist Meter/sekunde, weil ich Meter durch Sekunden teile

## Task4

caro <- read_delim("caro60.csv")
caro #KS noch nicht hinterlegt
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)
caro

?slice()

nrow(caro) #200 Datenpunkte

seq3 <- seq(1,200, by=3)
length(seq3)#67 Werte -> sollte stimmen
seq6<- seq(1,200, by=6)
seq9<- seq(1,200, by=9)

caro_3 <- slice(caro, seq3)
caro_6 <- slice(caro, seq6)
caro_9 <- slice(caro, seq9)

nrow(caro_3)
nrow(caro_6)
nrow(caro_9)#die Zahlen stimmen

caro <- caro |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec"))) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)) |> 
  mutate(speed_ms = steplength/timelag)

caro_3 <- caro_3 |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec"))) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)) |> 
  mutate(speed_ms = steplength/timelag)

caro_6 <- caro_6 |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec"))) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)) |> 
  mutate(speed_ms = steplength/timelag)

caro_9 <- caro_9 |>
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units="sec"))) |>
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)) |> 
  mutate(speed_ms = steplength/timelag)

caro |> 
  rbind(caro_3, caro_6, caro_9) |> 
  mutate(timelag_min = paste0(timelag/60, " min")) |> 
  na.omit() |> 
  ggplot(aes(x=DatetimeUTC, y=speed_ms, color=timelag_min))+
  geom_line(size=0.7)+
  theme_bw()+
  labs(x="Time",y="Speed (m/s)",color="Granularity")

#Die Geschwindigkeit ist bei hoher Granularität (also kurzen Zeitabständen zwischen den Messpunkten) deutlich höher. 

caro |> 
  rbind(caro_3) |> 
  mutate(Trajectory = paste0(timelag/60, " min")) |> 
  na.omit() |> 
  ggplot(aes(x=E, y=N, color=Trajectory,alpha=Trajectory))+
  geom_point(size=2)+
  geom_path(size=0.7) +
  theme_bw()+
  scale_alpha_manual(values=c(0.5,1))+
  labs(title="Comparing original- with 3 minutes-resampled data")

caro |> 
  rbind(caro_6) |> 
  mutate(Trajectory = paste0(timelag/60, " min")) |> 
  na.omit() |> 
  ggplot(aes(x=E, y=N, color=Trajectory,alpha=Trajectory))+
  geom_point(size=2)+
  geom_path(size=0.7) +
  theme_bw()+
  scale_alpha_manual(values=c(0.5,1))+
  labs(title="Comparing original- with 3 minutes-resampled data")

caro |> 
  rbind(caro_9) |> 
  mutate(Trajectory = paste0(timelag/60, " min")) |> 
  na.omit() |> 
  ggplot(aes(x=E, y=N, color=Trajectory, alpha=Trajectory))+
  geom_point(size=2)+
  geom_path(size=0.7) +
  theme_bw()+
  scale_alpha_manual(values=c(0.5,1))+
  labs(title="Comparing original- with 3 minutes-resampled data")

#Die Karten zeigen auf, dass der gemessene Weg mit zunehmenden Zeitabständen kleiner wird, da gewisse Umwege nicht mehr erfasst wurden. Dies erklärt auch wieso die Geschwindigkeit dann "tiefer" ist, da in der selben Zeit weniger Strecke zurückgelegt wurde. 


## Task5

example <- rnorm(10)
example
?rollmean()
rollmean(example, k = 3, fill = NA, align = "left")
rollmean(example, k = 4, fill = NA, align = "left")


caro_k3 <- caro |> 
  mutate(speed_ms = rollmean(speed_ms, k=3, fill=NA, align="left")) |> 
  mutate(k = "3k")
caro_k4 <- caro |> 
  mutate(speed_ms = rollmean(speed_ms, k=4, fill=NA, align="left")) |> 
  mutate(k = "4k")
caro_k5 <- caro |> 
  mutate(speed_ms = rollmean(speed_ms, k=5, fill=NA, align="left")) |> 
  mutate(k = "5k")
caro_k10 <- caro |> 
  mutate(speed_ms = rollmean(speed_ms, k=10, fill=NA, align="left")) |> 
  mutate(k = "10k")

caro |> 
  mutate(k = "none") |> 
  rbind(caro_k4, caro_k5, caro_k10) |> 
  ggplot(aes(x=DatetimeUTC, y=speed_ms, color=k))+
  geom_line(size=0.7)+
  theme_bw()+
  labs(x="Time",y="Speed (m/s)",color="Window")


#umso grösser das Fenster wird, umso stärker auch das "smoothing". D.h. die Peaks werden zunehmend kleiner.

## Task7

posmo <- read_delim("private/posmo_2023-01-01T00 00 00+01 00-2023-05-01T23 59 59+02 00.csv")
posmo #mit long/lat
posmo <- st_as_sf(posmo,coords = c("lon_x", "lat_y"),crs = 4326)
posmo <- st_transform(posmo, 2056)
posmo

# Daten erkunden
posmo$user_id |> unique() 
#logischerweise nur die Daten von einem User (mir)

posmo <- posmo |>
  arrange(datetime) |> 
  mutate(timelag = as.integer(difftime(lead(datetime), datetime, units="sec")))

summary(posmo)
#insgesamt 23'394 Messpunkte zwischen 2023-04-12 und 2023-04-30, timelag zwischen 0 und 98351s, im mittel aber nur 63.48

ggplot(posmo, aes(timelag)) +  
  geom_histogram(binwidth =1) +
  lims(x = c(0,1000)) +
  scale_y_log10()
#timelag am in der Regel nur wenige Sekunden

ggplot(posmo, aes(timelag)) +  
  geom_histogram(binwidth =1) +
  lims(x = c(0,25)) +
  scale_y_log10()
#ca. 5s

  ggplot(posmo, aes(x=datetime, y=timelag))+
  geom_point()+
  geom_line()
#Zeitabstände mit einer Ausnahme relativ regelmässig

posmo |> 
  filter(datetime > "2023-04-22") |> 
  ggplot(aes(x=datetime, y=timelag))+
    geom_point()+
    geom_line()
#in der Nacht grössere Zeitabstände, am Tag kleinere. Vermutlich weil Zuhause "gefenced"

ggplot(posmo)+
  geom_sf()+
  coord_sf(datum=2056)+
  theme_bw()

#einige Punkte weit weg. Verzerrt alles so sehr, das alltägliche Umgebung verloren geht:

posmo |> 
  filter(datetime > "2023-04-27") |> 
  ggplot()+
  geom_sf()+
  coord_sf(datum=2056)+
  theme_bw()

#ohne Hintergrundkarte noch nicht sehr aussagekräftig

tmap_mode("view") 
tm_shape(posmo)+
  tm_dots()
