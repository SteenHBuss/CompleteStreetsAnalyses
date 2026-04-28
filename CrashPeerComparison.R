library(dplyr)
library(stringr)
library(readr)
library(data.table)
library(lubridate)
library(tidyr)


#Corridors for Comparison
peers <- c("Adams St", "Clark St", "Dearborn St", "Franklin St", "Jackson Blvd", "Lake St", "La Salle St", "Madison St", "Monroe St", "Randolph St", "Van Buren St", "Washington Sty", "Wells St")


#Crash Data
corridor_crashes <- fread("Data/TrafficCrashes.csv") %>%
  select("CRASH_DATE", "CRASH_TYPE",
         "DAMAGE", "PRIM_CONTRIBUTORY_CAUSE",
         "SEC_CONTRIBUTORY_CAUSE", "STREET_NO",
         "STREET_DIRECTION", "STREET_NAME",
         "DOORING_I", "MOST_SEVERE_INJURY",
         "INJURIES_TOTAL", "INJURIES_FATAL",
         "INJURIES_INCAPACITATING", "INJURIES_NON_INCAPACITATING",
         "INJURIES_REPORTED_NOT_EVIDENT", "ROADWAY_SURFACE_COND",
         "LATITUDE", "LONGITUDE",
         "LOCATION") %>%
  mutate(STREET_NAME = str_to_title(STREET_NAME)) %>%
  filter(STREET_NAME %in% peers) %>%
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE)) %>%
  mutate(Year = year(CRASH_DATE)) %>%
  mutate(Month = month(CRASH_DATE)) %>%
  mutate(Weekday = wday(CRASH_DATE)) %>%
  filter(between(Year, 2015, 2025)) %>%
  filter(between(LATITUDE, 41.874498, 41.886859)) %>% #Keeping crash location coordinates in the Loop area
  filter(between(LONGITUDE, -87.636719, -87.624541))


####Annual crashes in each corridor, 2015 to 2025
annual_crashes <- corridor_crashes %>%
  group_by(Year, STREET_NAME) %>%
  summarise(Crashes = n()) %>%
  pivot_wider(names_from = STREET_NAME, values_from = Crashes)

####Annual injury crashes in each corridor, 2015 to 2025
annual_bikeped <- corridor_crashes %>%
  group_by(Year, STREET_NAME) %>%
  summarise(Injuries = sum(INJURIES_TOTAL)) %>%
  pivot_wider(names_from = STREET_NAME, values_from = Injuries)



##### Output to csv
#Annual crashes
write.csv(annual_crashes, "R/Outputs/PeerAnnualCrashes.csv")

#All crashes
write.csv(corridor_crashes, "R/Outputs/PeerCorridorCrashes.csv")

# Annual bike/ped crashes
write.csv(annual_bikeped, "R/Outputs/BikePedCrashes.csv")
