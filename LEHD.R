#For home PC:
#setwd("C:/Users/SelfaSteen/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
#For work PC:
setwd("C:/Users/shaus/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(R.utils)
library(tidyr)
library(tidycensus)


# NOTE: As of now, this analysis will not work.
# The commuter characteristics table only goes down to the tract level
# Tract 17031839100


# "il_od_main_JT00_2023" table from LEHD LODES
# "JT00" indicates that it includes all jobs
# "main" indicates that it includes jobs where the workplace and residence are within the state of Illinois
# Accessed 202060410 from https://lehd.ces.census.gov/data/
data <- fread("Data/LODES_JT00_2023.csv.gz") %>%
  select("w_geocode", "h_geocode", "S000", "createdate") %>%
  rename("WorkBlock" = "w_geocode",
         "HomeBlock" = "h_geocode",
         "TotJobs" = "S000") %>%
  mutate(WorkTract = str_sub(WorkBlock, 1, 11)) %>% #Remove last 2 numbers to define tracts
  mutate(HomeTract = str_sub(HomeBlock, 1, 11)) %>%
  filter(WorkTract == 17031839100) #Filter to tract 17031839100, which encompasses the Loop area 


# Define Census Blocks where Trips Originate
OriginTracts <- c(sort(unique(data$HomeTract)))

totalbytract <- data %>%
  group_by(HomeTract) %>%
  summarise(Jobs = sum(TotJobs))


# Define Census Table Variables
variables <- c(
  "B08141_006E", #Car, truck, or van - drove alone
  "B08141_011E", #Car, truck, or van - carpooled
  "B08141_016E", #Public Transportation
  "B08141_021E", #Walked
  "B08141_026E") #Taxi or ride-hailing services, motorcycle, bicycle, or other means


# 2024 data --------------------------------------------------------------------
censusdata <- get_acs(
  geography = "tract",
  state = "IL",
  variables = variables,
  year = 2024)

census_cleaned24 <- censusdata %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(values_from = estimate,
              names_from = variable) %>%
  rename(CarTruckVanAlone = "B08141_006",
         CarTruckVanPool = "B08141_011",
         Transit = "B08141_016",
         Walk = "B08141_021",
         TaxiRideShareBikeMoto = "B08141_026")

HomeTractsMode24 <- left_join(totalbytract, census_cleaned24, by = join_by("HomeTract" == "GEOID")) %>%
  mutate(Commuters = rowSums(across(c(CarTruckVanAlone,
                                         CarTruckVanPool,
                                         Transit,
                                         Walk,
                                         TaxiRideShareBikeMoto)))) %>% #Adding up commuters by mode
  mutate(PercVehAlone = CarTruckVanAlone/Commuters) %>% # Percentages by commute mode
  mutate(PercCarpool = CarTruckVanPool/Commuters) %>%
  mutate(PercTransit = Transit/Commuters) %>%
  mutate(PercWalk = Walk/Commuters) %>%
  mutate(PercOther = TaxiRideShareBikeMoto/Commuters) %>% # Mode percentage applied to number of jobs
  mutate(JobsVehAlone = round(Jobs * PercVehAlone)) %>%
  mutate(JobsCarpool = round(Jobs * PercCarpool)) %>%
  mutate(JobsTransit = round(Jobs * PercTransit)) %>%
  mutate(JobsWalk = round(Jobs * PercWalk)) %>%
  mutate(JobsOther = round(Jobs * PercOther))

# Total share by mode:
sum(HomeTractsMode24$JobsVehAlone, na.rm = TRUE)
sum(HomeTractsMode24$JobsCarpool, na.rm = TRUE)
sum(HomeTractsMode24$JobsTransit, na.rm = TRUE)
sum(HomeTractsMode24$JobsWalk, na.rm = TRUE)
sum(HomeTractsMode24$JobsOther, na.rm = TRUE)


# 2020 data --------------------------------------------------------------------
censusdata <- get_acs(
  geography = "tract",
  state = "IL",
  variables = variables,
  year = 2020)

census_cleaned20 <- censusdata %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(values_from = estimate,
              names_from = variable) %>%
  rename(CarTruckVanAlone = "B08141_006",
         CarTruckVanPool = "B08141_011",
         Transit = "B08141_016",
         Walk = "B08141_021",
         TaxiRideShareBikeMoto = "B08141_026")

HomeTractsMode20 <- left_join(totalbytract, census_cleaned20, by = join_by("HomeTract" == "GEOID")) %>%
  mutate(Commuters = rowSums(across(c(CarTruckVanAlone,
                                      CarTruckVanPool,
                                      Transit,
                                      Walk,
                                      TaxiRideShareBikeMoto)))) %>% #Adding up commuters by mode
  mutate(PercVehAlone = CarTruckVanAlone/Commuters) %>% # Percentages by commute mode
  mutate(PercCarpool = CarTruckVanPool/Commuters) %>%
  mutate(PercTransit = Transit/Commuters) %>%
  mutate(PercWalk = Walk/Commuters) %>%
  mutate(PercOther = TaxiRideShareBikeMoto/Commuters) %>% # Mode percentage applied to number of jobs
  mutate(JobsVehAlone = round(Jobs * PercVehAlone)) %>%
  mutate(JobsCarpool = round(Jobs * PercCarpool)) %>%
  mutate(JobsTransit = round(Jobs * PercTransit)) %>%
  mutate(JobsWalk = round(Jobs * PercWalk)) %>%
  mutate(JobsOther = round(Jobs * PercOther))

# Total share by mode:
sum(HomeTractsMode20$JobsVehAlone, na.rm = TRUE)
sum(HomeTractsMode20$JobsCarpool, na.rm = TRUE)
sum(HomeTractsMode20$JobsTransit, na.rm = TRUE)
sum(HomeTractsMode20$JobsWalk, na.rm = TRUE)
sum(HomeTractsMode20$JobsOther, na.rm = TRUE)

