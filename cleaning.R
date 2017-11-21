#required libraries
require(dplyr)

#loading in stava acitivy data as dataframe 
data <- read.csv("strava_activity.csv")

#removed rows that didn't have a gender assigned to them
data <- filter(data, athlete.sex == 'M' | athlete.sex == 'F')

#fixing for mispellings of countries  
data$location_country[data$location_country == "The Netherlands" | 
                      data$location_country == "Nederland"] <- "Netherlands"
data$location_country[data$location_country == "Brasil"] <- "Brazil"

#removed rows that had an elapsed time of 0
data <- filter(data, elapsed_time > 0)

#creating dataframe with cleaned data
cleaned.data <- select(data, location_country, average_watts, type, average_heartrate, max_heartrate, 
                       moving_time, elapsed_time) %>%
                filter(location_country == "United States" | location_country == "Australia" |
                       location_country == "United Kingdom" | location_country == "Netherlands" |
                       location_country == "Brazil")
  
write.csv(cleaned.data, file="cleanedData.csv", row.names = FALSE)

