library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)



##################################Setup##################################

#Read trip data from 2013 to 2019
trips_13to19 <- fread("Data/DivvyTrips/DivvyTrips2013-2019.csv") %>%
  select("START TIME",
         "FROM STATION NAME", "TO STATION NAME",
         "FROM LATITUDE", "FROM LONGITUDE",
         "TO LATITUDE", "TO LONGITUDE") %>%
  rename("started_at"= "START TIME",
         "start_station_name" = "FROM STATION NAME",
         "end_station_name" = "TO STATION NAME",
         "start_lat" = "FROM LATITUDE",
         "start_lng" = "FROM LONGITUDE",
         "end_lat" = "TO LATITUDE",
         "end_lng" = "TO LONGITUDE") %>%
  mutate(started_at = mdy_hms(started_at)) %>%
  mutate(Year = year(started_at)) %>%
  mutate(Month = month(started_at)) %>%
  mutate(Weekday = wday(started_at))


#Read trip data from 2020 to 2025
trips_20to25 <- list.files(path="Data/DivvyTrips/All_2020to2025", full.names = TRUE) %>% 
  lapply(function(x) {fread(x) %>%
      select("started_at",
             "start_station_name", "end_station_name",
             "start_lat", "start_lng",
             "end_lat", "end_lng")}) %>%
  bind_rows() %>% 
  mutate(started_at = parse_date_time(started_at, orders = c("ymd_HMS", "ymd"))) %>% #Had to parse with two different string types because some rows are missing the time string 
  mutate(Year = year(started_at)) %>%
  mutate(Month = month(started_at)) %>%
  mutate(Weekday = wday(started_at))


#Stations located on Franklin in the Loop (some stations are located farther north)
stations <- c("Franklin St & Adams St", "Franklin St & Adams St (Temp)", "Franklin St & Adams St Corral", "Franklin St & Arcade Pl",  "Franklin St & Jackson Blvd", "Franklin St & Lake St", "Franklin St & Madison St", "Franklin St & Monroe St", "Franklin St & Monroe St Corral", "Franklin St & Quincy St", "Franklin St & Washington St")


#######Adjust to Franklin and Combine all years#######
# 1.) Trips beginning on Franklin
FranklinStart <- bind_rows(trips_20to25, trips_13to19) %>%
  filter(start_station_name %in% stations)

# 1.) Trips ending on Franklin
FranklinEnd <- bind_rows(trips_20to25, trips_13to19) %>%
  filter(end_station_name %in% stations)



#######Corridor-level ridership#######

#Trips that start on Franklin, by year
Annual_FranklinStart <- FranklinStart %>%
  group_by(Year) %>%
  summarise(StartTripCount = n())

#Trips that end on Franklin, by year
Annual_FranklinEnd <- FranklinEnd %>%
  group_by(Year) %>%
  summarise(EndTripCount = n())

#Combine
Annual_FranklinAll <- left_join(Annual_FranklinStart, Annual_FranklinEnd, by = join_by(Year))

rm(Annual_FranklinStart)
rm(Annual_FranklinEnd)

#######Station-level ridership#######

#Total trips by station (2020 to 2025)
TripsToByStation <- trips_20to25 %>%
  filter(end_station_name %in% stations) %>%
  group_by(end_station_name) %>%
  summarise(TripsTo = n())

TripsFromByStation <- trips_20to25 %>%
  filter(start_station_name %in% stations) %>%
  group_by(start_station_name) %>%
  summarise(TripsFrom = n())

AllTripsByStation <- left_join(TripsToByStation, TripsFromByStation, by = join_by("end_station_name" == "start_station_name"))

rm(TripsToByStation)
rm(TripsFromByStation)


#df for trips that start on Franklin, by year and by station
Annual_FranklinStart_ByStation <- FranklinStart %>%
  group_by(Year, start_station_name) %>%
  rename(Station = start_station_name) %>%
  summarise(StartTripCount = n())

Annual_FranklinEnd_ByStation <- FranklinEnd %>%
  group_by(Year, end_station_name) %>%
  rename(Station = end_station_name) %>%
  summarise(EndTripCount = n())
#Annual count of all trips occurring on Franklin, by station
Annual_FranklinAll_ByStation <- left_join(Annual_FranklinStart_ByStation, Annual_FranklinEnd_ByStation, by = join_by(Year, Station))

View(Annual_Franklin_ByStation)

# Write to CSV----------------------------------------

# All 2020-2025 Trips by Station
write.csv(AllTripsByStation, "R/Outputs/AllTripsByStation.csv")

# Annual Trips on Franklin
write.csv(Annual_FranklinAll, "R/Outputs/AnnualDivvyTrips.csv")

# Annual Trips on Franklin, by Station
write.csv(Annual_FranklinAll_ByStation, "R/Outputs/AnnualDivvyTripsByStation.csv")

# Trips Starting on Franklin
write.csv(FranklinStart, "R/Outputs/DivvyTrips_FStart.csv")

#Trips Ending on Franklin
write.csv(FranklinEnd, "R/Outputs/DivvyTrips_FEnd.csv")

