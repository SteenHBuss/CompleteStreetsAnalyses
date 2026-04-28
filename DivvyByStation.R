#For home PC:
#setwd("C:/Users/SelfaSteen/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
#For work PC:
setwd("C:/Users/shaus/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)



##################################Setup##################################

#Raw trip data (2020 to 2025
alltrips <- list.files(path="Data/DivvyTrips/All_2020to2025", full.names = TRUE) %>% 
  lapply(function(x) {fread(x) %>%
      select("ride_id", "started_at",
             "start_station_name", "end_station_name",
             "start_lat", "start_lng",
             "end_lat", "end_lng")}) %>% #All original names are listed at bottom of script
  bind_rows() %>% 
  mutate(started_at = parse_date_time(started_at, orders = c("ymd_HMS", "ymd"))) %>% #Had to parse with two different string types because some rows are missing the time string 
  mutate(Year = year(started_at)) %>%
  mutate(Month = month(started_at)) %>%
  mutate(Weekday = wday(started_at)) %>%
  rename(StartDateTime = started_at)

#Locations of all Divvy Stations
allstations <- read.csv("Data/Divvy Stations Locations/Divvy_Stations.csv") %>%
  select("Station.Name", "Latitude", "Longitude")

#List of stations located on Franklin in the Loop (some stations are located farther north)
FrankStations <- c("Franklin St & Adams St", "Franklin St & Adams St (Temp)", "Franklin St & Adams St Corral",  "Franklin St & Jackson Blvd", "Franklin St & Lake St", "Franklin St & Madison St", "Franklin St & Monroe St", "Franklin St & Monroe St Corral", "Franklin St & Washington St")


#####################Adjusting for Franklin St Only######################

#Filtering for trips that began at a Franklin station
#Renamed Temporary Stations to Respective Permanent Stations
FranklinAllStarts <- alltrips %>%
  filter(start_station_name %in% FrankStations) %>%
  mutate(start_station_name = ifelse(start_station_name == "Franklin St & Adams St (Temp)", "Franklin St & Adams St", 
                                     ifelse(start_station_name == "Franklin St & Adams St Corral", "Franklin St & Adams St",
                                            ifelse(start_station_name == "Franklin St & Monroe St Corral", "Franklin St & Monroe St",
                                                   start_station_name))))


#Filtering for trips that ended at a Franklin station
#Renamed Temporary Stations to Respective Permanent Stations
FranklinAllEnds <- alltrips %>%
  filter(end_station_name %in% FrankStations) %>%
  mutate(end_station_name = ifelse(end_station_name == "Franklin St & Adams St (Temp)", "Franklin St & Adams St", 
                                   ifelse(end_station_name == "Franklin St & Adams St Corral", "Franklin St & Adams St",
                                          ifelse(end_station_name == "Franklin St & Monroe St Corral", "Franklin St & Monroe St",
                                                 end_station_name))))


#########################################################################
#########Create dfs for Starts and Ends at Each Franklin Station#########
#########################################################################

### Adams St---------------------------------------------------------------
# All Trips Starting at the Adams station
FromAdamsAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Adams St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# All Trips Ending at the Adams Station
ToAdamsAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Adams St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Adams: Origin
FromAdams_O <- na.omit(left_join(FromAdamsAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Starts at Adams: Destination
FromAdams_D <- na.omit(left_join(FromAdamsAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Adams: Origin
ToAdams_O <- na.omit(left_join(ToAdamsAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Ends at Adams: Destination
ToAdams_D <- na.omit(left_join(ToAdamsAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")


###  Jackson Blvd----------------------------------------------------------
# All Trips Starting at the Jackson station
FromJacksonAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Jackson Blvd") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")
# All Trips Ending at the Jackson Station
ToJacksonAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Jackson Blvd") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Jackson: Origin
FromJackson_O <- na.omit(left_join(FromJacksonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Starts at Jackson: Destination
FromJackson_D <- na.omit(left_join(FromJacksonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Jackson: Origin
ToJackson_O <- na.omit(left_join(ToJacksonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Ends at Jackson: Destination
ToJackson_D <- na.omit(left_join(ToJacksonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")


### Lake St----------------------------------------------------------
# All Trips Starting at the Lake station
FromLakeAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Lake St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")
# All Trips Ending at the Jackson Station
ToLakeAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Lake St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Lake: Origin
FromLake_O <- na.omit(left_join(FromLakeAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Starts at Lake: Destination
FromLake_D <- na.omit(left_join(FromLakeAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Lake: Origin
ToLake_O <- na.omit(left_join(ToLakeAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Ends at Lake: Destination
ToLake_D <- na.omit(left_join(ToLakeAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

### Madison St----------------------------------------------------------
# All Trips Starting at the Madison station
FromMadisonAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Madison St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# All Trips Ending at the Adams Station
ToMadisonAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Madison St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Madison: Origin
FromMadison_O <- na.omit(left_join(FromMadisonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Starts at Madison: Destination
FromMadison_D <- na.omit(left_join(FromMadisonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Madison: Origin
ToMadison_O <- na.omit(left_join(ToMadisonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Ends at Madison: Destination
ToMadison_D <- na.omit(left_join(ToMadisonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

### Monroe St----------------------------------------------------------
# All Trips Starting at the Monroe station
FromMonroeAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Monroe St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# All Trips Ending at the Monroe Station
ToMonroeAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Monroe St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Monroe: Origin
FromMonroe_O <- na.omit(left_join(FromMonroeAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")

# Starts at Monroe: Destination
FromMonroe_D <- na.omit(left_join(FromMonroeAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Monroe: Origin
ToMonroe_O <- na.omit(left_join(ToMonroeAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")
# Ends at Monroe: Destination
ToMonroe_D <- na.omit(left_join(ToMonroeAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

### Washington St----------------------------------------------------------
# All Trips Starting at the Washington station
FromWashingtonAll <- FranklinAllStarts %>%
  filter(start_station_name %in% "Franklin St & Washington St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# All Trips Ending at the Washington Station
ToWashingtonAll <- FranklinAllEnds %>%
  filter(end_station_name %in% "Franklin St & Washington St") %>%
  select("ride_id", "StartDateTime", "start_station_name", "end_station_name", "Year", "Month", "Weekday")

# Starts at Washington: Origin
FromWashington_O <- na.omit(left_join(FromWashingtonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")

# Starts at Washington: Destination
FromWashington_D <- na.omit(left_join(FromWashingtonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")

# Ends at Washington: Origin
ToWashington_O <- na.omit(left_join(ToWashingtonAll, allstations, by = join_by("start_station_name" == "Station.Name"))) %>%
  rename("StartLat" = "Latitude", "StartLong" = "Longitude")

# Ends at Washington: Destination
ToWashington_D <- na.omit(left_join(ToWashingtonAll, allstations, by = join_by("end_station_name" == "Station.Name"))) %>%
  rename("EndLat" = "Latitude", "EndLong" = "Longitude")


##############################Output to csv##############################


### Adams-----------------------------------
#Starts
write.csv(FromAdams_O, "R/Outputs/O-Ds/Franklin Starts/Adams/FromAdams_O.csv")
write.csv(FromAdams_D, "R/Outputs/O-Ds/Franklin Starts/Adams/FromAdams_D.csv")
#Ends
write.csv(ToAdams_O, "R/Outputs/O-Ds/Franklin Ends/Adams/ToAdams_O.csv")
write.csv(ToAdams_D, "R/Outputs/O-Ds/Franklin Ends/Adams/ToAdams_D.csv")

### Jackson---------------------------------
#Starts
write.csv(FromJackson_O, "R/Outputs/O-Ds/Franklin Starts/Jackson/FromJackson_O.csv")
write.csv(FromJackson_D, "R/Outputs/O-Ds/Franklin Starts/Jackson/FromJackson_D.csv")
#Ends
write.csv(ToJackson_O, "R/Outputs/O-Ds/Franklin Ends/Jackson/ToJackson_O.csv")
write.csv(ToJackson_D, "R/Outputs/O-Ds/Franklin Ends/Jackson/ToJackson_D.csv")

### Lake------------------------------------
#Starts
write.csv(FromLake_O, "R/Outputs/O-Ds/Franklin Starts/Lake/FromLake_O.csv")
write.csv(FromLake_D, "R/Outputs/O-Ds/Franklin Starts/Lake/FromLake_D.csv")
#Ends
write.csv(ToLake_O, "R/Outputs/O-Ds/Franklin Ends/Lake/ToLake_O.csv")
write.csv(ToLake_D, "R/Outputs/O-Ds/Franklin Ends/Lake/ToLake_D.csv")

### Madison---------------------------------
#Starts
write.csv(FromMadison_O, "R/Outputs/O-Ds/Franklin Starts/Madison/FromMadison_O.csv")
write.csv(FromMadison_D, "R/Outputs/O-Ds/Franklin Starts/Madison/FromMadison_D.csv")
#Ends
write.csv(ToMadison_O, "R/Outputs/O-Ds/Franklin Ends/Madison/ToMadison_O.csv")
write.csv(ToMadison_D, "R/Outputs/O-Ds/Franklin Ends/Madison/ToMadison_D.csv")

### Monroe----------------------------------
#Starts
write.csv(FromMonroe_O, "R/Outputs/O-Ds/Franklin Starts/Monroe/FromMonroe_O.csv")
write.csv(FromMonroe_D, "R/Outputs/O-Ds/Franklin Starts/Monroe/FromMonroe_D.csv")
#Ends
write.csv(ToMonroe_O, "R/Outputs/O-Ds/Franklin Ends/Monroe/ToMonroe_O.csv")
write.csv(ToMonroe_D, "R/Outputs/O-Ds/Franklin Ends/Monroe/ToMonroe_D.csv")

### Washington------------------------------
#Starts
write.csv(FromWashington_O, "R/Outputs/O-Ds/Franklin Starts/Washington/FromWashington_O.csv")
write.csv(FromWashington_D, "R/Outputs/O-Ds/Franklin Starts/Washington/FromWashington_D.csv")
#Ends
write.csv(ToWashington_O, "R/Outputs/O-Ds/Franklin Ends/Washington/ToWashington_O.csv")
write.csv(ToWashington_D, "R/Outputs/O-Ds/Franklin Ends/Washington/ToWashington_D.csv")


# Column headers present in full trips data set
# "ride_id" "rideable_type" "StartDataTime" "ended_at" "start_station_name" "end_station_name" "start_lat"
# "start_lng" "end_lat" "end_lng" "member_casual" "Year" "Month" "Weekday"
# "start_station_id" "end_station_id" #Note that the CSVs dont bind when these 2 are included because some are type chr and some are type int. They would need to be changed if there is a deisre to include them later




