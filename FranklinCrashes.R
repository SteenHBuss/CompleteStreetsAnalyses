#For home PC:
#setwd("C:/Users/SelfaSteen/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
#For work PC:
setwd("C:/Users/shaus/OneDrive - University of Illinois Chicago/Current Classes/UPP565/Assignment 2/Analysis")
library(dplyr)
library(stringr)
library(readr)
library(data.table)

###############Data imports###############
##########################################

# Raw Crash Data----------------
data_crashes <- fread("Data/TrafficCrashes.csv")

# Filter to Franklin & Adjust Columns------------
franklin_crashes <- data_crashes %>%
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



##################################Corridor-level crash analysis##################################
#################################################################################################

# Crashes that occur on Franklin, by year
franklin_annualcrashes <- franklin_crashes %>%
  group_by(Year) %>%
  summarise(Crashes = n())

#export to CSV for GIS
write.csv(franklin_crashes, "R/Outputs/FranklinAllCrashes.csv")



