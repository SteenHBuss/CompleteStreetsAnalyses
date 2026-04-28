library(dplyr)
library(stringr)
library(readr)
library(data.table)

###############Data imports###############
##########################################

#Crash Data
franklin_crashes <- fread("Data/TrafficCrashes.csv") %>%
  select("CRASH_DATE",
         "CRASH_TYPE",
         "DAMAGE",
         "PRIM_CONTRIBUTORY_CAUSE",
         "SEC_CONTRIBUTORY_CAUSE",
         "STREET_NO",
         "STREET_DIRECTION",
         "STREET_NAME",
         "DOORING_I",
         "MOST_SEVERE_INJURY",
         "INJURIES_TOTAL",
         "INJURIES_FATAL",
         "INJURIES_INCAPACITATING",
         "INJURIES_NON_INCAPACITATING",
         "INJURIES_REPORTED_NOT_EVIDENT",
         "ROADWAY_SURFACE_COND",
         "CRASH_HOUR",
         "CRASH_DAY_OF_WEEK",
         "CRASH_MONTH",
         "LATITUDE",
         "LONGITUDE",
         "LOCATION") %>%
  filter(STREET_NAME == "FRANKLIN ST") %>%
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE)) %>%
  mutate(Year = year(CRASH_DATE))

#AADT Data
franklin_aadt <- fread("Data/AADT.csv") %>%
  select("AADT_YR",
         "ROAD_NAME",
         "AADT",
         "HCV_AADT",
         "MU_AADT",   
         "SU_AADT",
         "COUNTY_NAM") %>%
  filter(ROAD_NAME == "FRANKLIN ST", COUNTY_NAM == "COOK")


#ILMAT 2020 Data
franklin_ilmat20 <- fread("Data/ILMAT2020.csv") %>%
  select("Road Name",
         "AADT",
         "Annual Person Hours of Delay",
         "Annual Truck Vehicle Hours of Delay",
         "Annual Excess Fuel Used Due to Congestion (Gallons)",
         "Annual Excess Fuel Used Due to Truck Congestion (Gallons)",
         "Annual Congestion Cost",
         "VMT",
         "Truck VMT",
         "Average Peak Period Speed (based on 6-9am, 4-7pm)",
         "Freeflow Speed for All Vehicles",
         "Freeflow Speed for Trucks",
         "Annual Average Daily Heavy Commercial Volume",
         "Annual Average Daily Single Unit and Multiple Unit Volume Year",
         "Annual Average Daily Multiple Unit Volume",
         "Annual Average Daily Single Unit Volume") %>%
  rename("RoadName20" = "Road Name",
         "AADT20" = "AADT",
         "AnnualPersonHrsDelay20" = "Annual Person Hours of Delay",
         "AnnualTruckVhclHrsDelay20" = "Annual Truck Vehicle Hours of Delay",
         "AnnualExcessFuelFromCongestion20" = "Annual Excess Fuel Used Due to Congestion (Gallons)",
         "AnnualExcessFuelFromCongestionTrucks20" = "Annual Excess Fuel Used Due to Truck Congestion (Gallons)",
         "AnnualCongestionCost20" = "Annual Congestion Cost",
         "VMT20" = "VMT",
         "TruckVMT20" = "Truck VMT",
         "AvgPeakPrdSpeed20" = "Average Peak Period Speed (based on 6-9am, 4-7pm)",
         "FreeflowSpeedAllVhcls20" = "Freeflow Speed for All Vehicles",
         "FreeflowSpeedTrucks20" = "Freeflow Speed for Trucks",
         "AvgDailyHvyCmrclVolume20" = "Annual Average Daily Heavy Commercial Volume",
         "AvgDailySUandMUVolume_Year20" = "Annual Average Daily Single Unit and Multiple Unit Volume Year",
         "AnnualAvgDailyMUVolume20" = "Annual Average Daily Multiple Unit Volume",
         "AnnualAvgDailySUVolume20" = "Annual Average Daily Single Unit Volume") %>%
  filter(RoadName20 == "FRANKLIN ST")

#ILMAT 2024 Data
franklin_ilmat24 <- fread("Data/ILMAT2024.csv") %>%
  select("Road Name",
         "AADT",
         "Annual Person Hours of Delay",
         "Annual Truck Vehicle Hours of Delay",
         "Annual Excess Fuel Used Due to Congestion (Gallons)",
         "Annual Excess Fuel Used Due to Truck Congestion (Gallons)",
         "Annual Congestion Cost",
         "VMT",
         "Truck VMT",
         "Average Peak Period Speed (based on 6-9am, 4-7pm)",
         "Freeflow Speed for All Vehicles",
         "Freeflow Speed for Trucks",
         "Annual Average Daily Heavy Commercial Volume",
         "Annual Average Daily Single Unit and Multiple Unit Volume Year",
         "Annual Average Daily Multiple Unit Volume",
         "Annual Average Daily Single Unit Volume") %>%
  rename("RoadName24" = "Road Name",
         "AADT24" = "AADT",
         "AnnualPersonHrsDelay24" = "Annual Person Hours of Delay",
         "AnnualTruckVhclHrsDelay24" = "Annual Truck Vehicle Hours of Delay",
         "AnnualExcessFuelFromCongestion24" = "Annual Excess Fuel Used Due to Congestion (Gallons)",
         "AnnualExcessFuelFromCongestionTrucks24" = "Annual Excess Fuel Used Due to Truck Congestion (Gallons)",
         "AnnualCongestionCost24" = "Annual Congestion Cost",
         "VMT24" = "VMT",
         "TruckVMT24" = "Truck VMT",
         "AvgPeakPrdSpeed24" = "Average Peak Period Speed (based on 6-9am, 4-7pm)",
         "FreeflowSpeedAllVhcls24" = "Freeflow Speed for All Vehicles",
         "FreeflowSpeedTrucks24" = "Freeflow Speed for Trucks",
         "AvgDailyHvyCmrclVolume24" = "Annual Average Daily Heavy Commercial Volume",
         "AvgDailySUandMUVolume_Year24" = "Annual Average Daily Single Unit and Multiple Unit Volume Year",
         "AnnualAvgDailyMUVolume24" = "Annual Average Daily Multiple Unit Volume",
         "AnnualAvgDailySUVolume24" = "Annual Average Daily Single Unit Volume") %>%
  filter(RoadName24 == "FRANKLIN ST")


#########Divvy trip Data - split by years exists due to different data sources.

#Trips starting on Franklin, 2013 to 2019--------------------
fromfranklin_divvytrips2013to2019 <- fread("Data/DivvyTrips2013-2019.csv") %>%
  select("START TIME",
         "FROM STATION NAME",
         "TO STATION NAME",
         "USER TYPE",
         "GENDER",
         "BIRTH YEAR",
         "FROM LATITUDE",
         "FROM LONGITUDE",
         "FROM LOCATION",
         "TO LATITUDE",
         "TO LONGITUDE",
         "TO LOCATION") %>%
  rename("StartStation" = "FROM STATION NAME") %>%
  filter(str_detect(StartStation, "Franklin St"))

#Trips ending on Franklin, 2013 to 2019----------------------
tofranklin_divvytrips2013to2019 <- fread("Data/DivvyTrips2013-2019.csv") %>%
  select("START TIME",
         "FROM STATION NAME",
         "TO STATION NAME",
         "USER TYPE",
         "GENDER",
         "BIRTH YEAR",
         "FROM LATITUDE",
         "FROM LONGITUDE",
         "FROM LOCATION",
         "TO LATITUDE",
         "TO LONGITUDE",
         "TO LOCATION") %>%
  rename("EndStation" = "TO STATION NAME") %>%
  filter(str_detect(EndStation, "Franklin St"))

#Trips starting on Franklin, 2020 to 2025--------------------
fromfranklin_divvytrips2020to2025 <- list.files(path="Data/DivvyTrips2020-2025/Aggregated", full.names = TRUE) %>% 
  lapply(function(x) {fread(x) %>% 
      select("started_at",
             "ended_at",
             "start_station_name",
             "end_station_name",
             "start_lat",
             "start_lng",
             "end_lat",
             "end_lng")}) %>%
  bind_rows() %>%
  filter(str_detect(start_station_name, "Franklin St"))

#Trips ending on Franklin, 2020 to 2025----------------------
tofranklin_divvytrips2020to2025 <- list.files(path="Data/DivvyTrips2020-2025/Aggregated", full.names = TRUE) %>% 
  lapply(function(x) {fread(x) %>% 
      select("started_at",
             "ended_at",
             "start_station_name",
             "end_station_name",
             "start_lat",
             "start_lng",
             "end_lat",
             "end_lng")}) %>%
  bind_rows() %>%
  filter(str_detect(end_station_name, "Franklin St"))

#Have not used the 311 data yet. Remove comment key if needed later
#data_311 <- fread("Data/311_34_42.csv")


##################################Corridor-level crash analysis##################################
#################################################################################################

#Create df for crashes that occur on Franklin, by year
franklin_annualcrashes <- franklin_crashes %>%
  group_by(Year) %>%
  summarise(Crashes = n())

##########Divvy Preliminary Analysis##########


#Add year columns to each Divvy trips df
fromfranklin_divvytrips2013to2019$Year <- str_sub(fromfranklin_divvytrips2013to2019$`START TIME`, 7, 10)
fromfranklin_divvytrips2020to2025$Year <- str_sub(fromfranklin_divvytrips2020to2025$started_at, 1, 4)
tofranklin_divvytrips2013to2019$Year <- str_sub(tofranklin_divvytrips2013to2019$`START TIME`, 7, 10)
tofranklin_divvytrips2020to2025$Year <- str_sub(tofranklin_divvytrips2020to2025$started_at, 1, 4)

#Divvy trips that start on Franklin, by year (2013-2019)
fromfranklin_divvytrips_annual_2013to2019 <- fromfranklin_divvytrips2013to2019 %>%
  mutate(started_at = parse_date_time(`START TIME`, orders = c("ymd_HMS", "ymd"))) %>% 
  mutate(Year = year(started_at)) %>%
  group_by(Year) %>%
  summarise(TripsToFranklin = n())

#Divvy trips that start on Franklin, by year (2020-2025)
fromfranklin_divvytrips_annual_2020to2025 <- fromfranklin_divvytrips2020to2025 %>%
  mutate(started_at = parse_date_time(started_at, orders = c("ymd_HMS", "ymd"))) %>% 
  mutate(Year = year(started_at)) %>%
  group_by(Year) %>%
  summarise(TripsToFranklin = n())

# Combine all years
fromfranklin_divvytrips_annual <- bind_rows(fromfranklin_divvytrips_annual_2013to2019, fromfranklin_divvytrips_annual_2020to2025)

# Remove unneeded dfs
rm(fromfranklin_divvytrips_annual_2013to2019)
rm(fromfranklin_divvytrips_annual_2020to2025)

#Divvy trips that end on Franklin, by year (2020-2025)
tofranklin_divvytrips_annual_2013to2019 <- tofranklin_divvytrips2013to2019 %>%
  mutate(started_at = parse_date_time(`START TIME`, orders = c("ymd_HMS", "ymd"))) %>% 
  mutate(Year = year(started_at)) %>%
  group_by(Year) %>%
  summarise(TripsFromFranklin = n())

#Divvy trips that end on Franklin, by year (2020-2025)
tofranklin_divvytrips_annual_2020to2025 <- tofranklin_divvytrips2020to2025 %>%
  mutate(started_at = parse_date_time(started_at, orders = c("ymd_HMS", "ymd"))) %>% 
  mutate(Year = year(started_at)) %>%
  group_by(Year) %>%
  summarise(TripsFromFranklin = n())

# Combine all years
tofranklin_divvytrips_annual <- bind_rows(tofranklin_divvytrips_annual_2013to2019, tofranklin_divvytrips_annual_2020to2025)

# Remove unneeded dfs
rm(tofranklin_divvytrips_annual_2013to2019)
rm(tofranklin_divvytrips_annual_2020to2025)


#Final df for annual Divvy trips, combining to and from Divvy stations, located on Franklin 
franklin_annual_divvy_trips <- left_join(fromfranklin_divvytrips_annual, tofranklin_divvytrips_annual, by = join_by(Year))

# Remove unneeded dfs
rm(fromfranklin_divvytrips_annual)
rm(tofranklin_divvytrips_annual)


##############Final Stats##############
#######################################


cat("Crash & Injury Stats on Franklin, 2015-2025", "\n") %>%
  cat("Crashes Last Year:", franklin_annualcrashes$Crashes[11], "\n") %>%
  cat("Total Crashes, 2015 to 2025:", nrow(franklin_crashes), "\n") %>%
  cat("% Change in Crashes, 2015 to 2025:", ((franklin_annualcrashes$Crashes[11] - franklin_annualcrashes$Crashes[1])/franklin_annualcrashes$Crashes[1]) * 100, "\n") %>%
  cat("Total Injuries, 2015 to 2025:", sum(franklin_crashes$INJURIES_TOTAL, na.rm = TRUE), "\n") %>%
  cat("Incapacitating Injuries, 2015 to 2025:", sum(franklin_crashes$INJURIES_INCAPACITATING, na.rm = TRUE), "\n") %>% 
  cat("Fatal Injuries, 2015 to 2025:", sum(franklin_crashes$INJURIES_FATAL, na.rm = TRUE), "\n") %>%
  cat("Dooring Incidents, 2015 to 2025:", sum(franklin_crashes$DOORING_I == "Y"), "\n") %>%
  
  cat("Vehicular volume stats on Franklin, 2020-2024", "\n") %>%
  cat("Current AADT Estimate:", franklin_ilmat24$AADT24[2], "\n") %>% 
  cat("Percent Change, 2020 to 2024:", (franklin_ilmat24$AADT24[2] - franklin_ilmat20$AADT20[2])/franklin_ilmat20$AADT20[2]*100, "\n") %>%
  cat("Current Truck VMT Estimate:", franklin_ilmat24$TruckVMT24[9], "\n") %>% 
  cat("Truck VMT YoY Change (%):", ((franklin_ilmat24$TruckVMT24[9] - franklin_ilmat20$TruckVMT20[9])/franklin_ilmat20$TruckVMT20[9]*100), "\n") %>%
  
  cat("Congestion/Movement Stats on Franklin, 2020-2024", "\n") %>%
  cat("Person Hours of Delay in 2024:", sum(franklin_ilmat24$AnnualPersonHrsDelay24), "\n") %>% #Not doing a YoY analysis because the 2020 data is sus
  cat("Average Freeflow Vehicle Speed Between Madison & Van Buren in 2024 (mph):", franklin_ilmat24$FreeflowSpeedAllVhcls24[5], "\n") %>%
  cat("Average Freeflow Vehicle Speed Between Madison & Van Buren in 2020 (mph):", franklin_ilmat20$FreeflowSpeedAllVhcls20[5], "\n") %>%
  cat("% Difference in Average Freeflow Speed Between Madison & Van Buren in 2024:", ((franklin_ilmat24$FreeflowSpeedAllVhcls24[5] - franklin_ilmat20$FreeflowSpeedAllVhcls20[5]) / franklin_ilmat20$FreeflowSpeedAllVhcls20[5]) * 100, "\n") %>%
  cat("Average Peak Hour Vehicle Speed Between Madison & Van Buren in 2024 (mph):", franklin_ilmat24$AvgPeakPrdSpeed24[5], "\n") %>%
  cat("Average Peak Hour Vehicle Speed Between Madison & Van Buren in 2020 (mph):", franklin_ilmat20$AvgPeakPrdSpeed20[5], "\n") %>%
  cat("% Difference in Average Peak Hour Speed Between Madison & Van Buren in 2024:", ((franklin_ilmat24$AvgPeakPrdSpeed24[5] - franklin_ilmat20$AvgPeakPrdSpeed20[5]) / franklin_ilmat20$AvgPeakPrdSpeed20[5]) * 100, "\n" ) %>%
  
  cat("Divvy Stats", "\n") %>%
  cat("Divvy Trips That Began on Franklin in 2025:", franklin_annual_divvy_trips$TripsFromFranklin[13], "\n") %>% 
  cat("Divvy Trips That Ended on Franklin in 2025:", franklin_annual_divvy_trips$TripsToFranklin[13], "\n") %>% 
  cat("Percent Change in Divvy Trips Beginning on Franklin, 2015 to 2025:", (franklin_annual_divvy_trips$TripsFromFranklin[13] - franklin_annual_divvy_trips$TripsFromFranklin[3])/franklin_annual_divvy_trips$TripsFromFranklin[3]*100, "\n") %>%
  cat("Percent Change in Divvy Trips Ending on Franklin, 2015 to 2025:", (franklin_annual_divvy_trips$TripsToFranklin[13] - franklin_annual_divvy_trips$TripsToFranklin[3])/franklin_annual_divvy_trips$TripsToFranklin[3]*100, "\n") %>%
  cat("Percent Change in Divvy Trips Beginning on Franklin, 2020 to 2025:", (franklin_annual_divvy_trips$TripsFromFranklin[13] - franklin_annual_divvy_trips$TripsFromFranklin[8])/franklin_annual_divvy_trips$TripsFromFranklin[3]*100, "\n") %>%
  cat("Percent Change in Divvy Trips Ending on Franklin, 2020 to 2025:", (franklin_annual_divvy_trips$TripsToFranklin[13] - franklin_annual_divvy_trips$TripsToFranklin[8])/franklin_annual_divvy_trips$TripsToFranklin[3]*100, "\n")


######Tables & Visuals######
View(franklin_annual_divvy_trips)
View(franklin_annualcrashes)
View(franklin_ilmat20)
View(franklin_ilmat24)
View(franklin_aadt)
plot(franklin_annual_divvy_trips$TripsToFranklin)
plot(franklin_annual_divvy_trips$TripsFromFranklin)

