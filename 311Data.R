library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(sf)



# Setup-------------------------------------------------------------------------
#Data: Chicago 311 requests; queried on source page to include only wards 34 and 42 (where the study area is located)
#Obtained 3/15/2026 from the Chicago Data Portal (https://data.cityofchicago.org/Service-Requests/311-Service-Requests/v6vf-nfxy/about_data)
# Time span of data is between 12/18/2018 (when new 311 system was launched) and 3/15/2026 (date of download)
data <- read.csv("Data/311_34_42.csv") %>%
  select("STREET_NAME",
         "SR_NUMBER",
         "SR_TYPE",
         "SR_SHORT_CODE",
         "CREATED_DATE",
         "LATITUDE",
         "LONGITUDE") %>%
  mutate(CREATED_DATE = mdy_hms(CREATED_DATE)) %>%
  mutate(Year = year(CREATED_DATE)) %>%
  mutate(Month = month(CREATED_DATE)) %>%
  mutate(Weekday = wday(CREATED_DATE)) %>%
  rename("StreetName" = "STREET_NAME",
         "SR_Num" = "SR_NUMBER",
         "SR_Type" = "SR_TYPE",
         "SR_Code" = "SR_SHORT_CODE",
         "DateTime" = "CREATED_DATE",
         "Lat" = "LATITUDE",
         "Long" = "LONGITUDE") %>%
  filter(between(Lat, 41.8744, 41.8867)) %>%   #Filtering for area around Franklin (see reference) to account for possible misrepresentation in listed street name
  filter(between(Long, -87.6355, -87.6351))


# Filtering relevant request types----------------------------------------------

RelevantRequests <- data %>%
  filter(SR_Type %in% c("Divvy Bike Parking Complaint",
                        "E-Scooter Parking Complaint",
                        "Ice and Snow Removal Request",
                        "Snow – Uncleared Sidewalk Complaint",
                        "Pothole in Street Complaint",
                        "Alley Pothole Complaint",
                        "Street Cleaning Request",
                        "Vehicle Parked in Bike Lane Complaint",
                        "Water On Street Complaint")) %>%
  mutate(Category = ifelse(SR_Type %in% c("Divvy Bike Parking Complaint", "E-Scooter Parking Complaint"), "BikePedParking",
                           ifelse(SR_Type %in% c("Ice and Snow Removal Request", "Snow – Uncleared Sidewalk Complaint"), "SnowOnSideWalk",
                                  ifelse(SR_Type %in% c("Pothole in Street Complaint", "Alley Pothole Complaint", "Street Cleaning Request"), "StreetCondition",
                                         ifelse(SR_Type %in% "Vehicle Parked in Bike Lane Complaint", "CarInBikeLane",
                                                ifelse(SR_Type %in% "Water On Street Complaint", "WaterOnStreet",
                                                       "None"))))))


summary <- RelevantRequests %>%
  group_by(Category) %>%
  summarise(Count = n())


View(summary)

# Output------------------------------------------------------------------------

# Shapefile for mapping
sf_requests <- st_as_sf(RelevantRequests, coords = c("Long", "Lat"), na.fail = FALSE, crs = 4326)
st_write(sf_requests, "R/Outputs/Filtered311Requests/311Requests.shp")

# Summary table
write.csv(summary, "R/Outputs/311Summary.csv")

### For Reference---------------------------------------------------------------

# Coordinates used for spatial filter
#87.6352654°W 41.8744604°N   #Southwest corner, 15m from Franklin St centerline
#87.6352480°W 41.8867307°N  #Northeast corner, 15 from Franklin St centerline

#Names Present in Original Data Set
#[1] "SR_NUMBER"                "SR_TYPE"                  "SR_SHORT_CODE"            "CREATED_DATE"             "STREET_ADDRESS"          
#[6] "ZIP_CODE"                 "STREET_NUMBER"            "STREET_DIRECTION"         "STREET_NAME"              "STREET_TYPE"             
#[11] "DUPLICATE"                "LEGACY_RECORD"            "LEGACY_SR_NUMBER"         "PARENT_SR_NUMBER"         "COMMUNITY_AREA"          
#[16] "WARD"                     "ELECTRICAL_DISTRICT"      "ELECTRICITY_GRID"         "POLICE_SECTOR"            "POLICE_DISTRICT"         
#[21] "POLICE_BEAT"              "PRECINCT"                 "SANITATION_DIVISION_DAYS" "CREATED_HOUR"             "CREATED_DAY_OF_WEEK"     
#[26] "CREATED_MONTH"            "X_COORDINATE"             "Y_COORDINATE"             "LATITUDE"                 "LONGITUDE"               
#[31] "LOCATION"

# SR Types Present in Original Data Set
#[1] "311 INFORMATION ONLY CALL"                     "Abandoned Vehicle Complaint"                  
#[3] "Alley Light Out Complaint"                     "Alley Pothole Complaint"                      
#[5] "Bee/Wasp Removal"                              "Bicycle Request/Complaint"                    
#[7] "Building Violation"                            "Buildings - Plumbing Violation"               
#[9] "Business Complaints"                           "Cab Feedback"                                 
#[11] "Check for Leak"                                "Clean Vacant Lot Request"                     
#[13] "Commercial Fire Safety Inspection Request"     "Consumer Fraud Complaint"                     
#[15] "Consumer Retail Business Complaint"            "Coyote Interaction Complaint"                 
#[17] "Dead Animal Pick-Up Request"                   "Dead Bird"                                    
#[19] "Divvy Bike Parking Complaint"                  "E-Scooter"                                    
#[21] "E-Scooter Parking Complaint"                   "Finance Parking Code Enforcement Review"      
#[23] "Fly Dumping Complaint"                         "Graffiti Removal Request"                     
#[25] "Ice and Snow Removal Request"                  "Inspect Public Way Request"                   
#[27] "Low Water Pressure Complaint"                  "Missed Garbage Pick-Up Complaint"             
#[29] "No Building Permit and Construction Violation" "Open Fire Hydrant Complaint"                  
#[31] "Paid Sick Leave Violation"                     "Pet Wellness Check Request"                   
#[33] "Pothole in Street Complaint"                   "Public Vehicle/Valet Complaint"               
#[35] "Report an Injured Animal"                      "Restaurant Complaint"                         
#[37] "Ridesharing Complaint"                         "Rodent Baiting/Rat Complaint"                 
#[39] "Sanitation Code Violation"                     "Sewer Cave-In Inspection Request"             
#[41] "Sewer Cleaning Inspection Request"             "Sidewalk Café/Outdoor Dining Complaint"       
#[43] "Sidewalk Inspection Request"                   "Sign Repair Request - All Other Signs"        
#[45] "Sign Repair Request - Do Not Enter Sign"       "Sign Repair Request - One Way Sign"           
#[47] "Sign Repair Request - Stop Sign"               "Snow – Uncleared Sidewalk Complaint"          
#[49] "Stray Animal Complaint"                        "Street Cleaning Request"                      
#[51] "Street Light On During Day Complaint"          "Street Light Out Complaint"                   
#[53] "Street Light Pole Damage Complaint"            "Street Light Pole Door Missing Complaint"     
#[55] "Traffic Signal Out Complaint"                  "Tree Emergency"                               
#[57] "Tree Removal Inspection"                       "Tree Trim Request (NO LONGER BEING ACCEPTED)" 
#[59] "Vehicle Parked in Bike Lane Complaint"         "Vicious Animal Complaint"                     
#[61] "Water Lead Test Kit Request"                   "Water On Street Complaint"                    
#[63] "Wire Basket Request" 
