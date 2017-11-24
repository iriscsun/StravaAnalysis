#required libraries
require(dplyr)

#loading in stava acitivy data as dataframe 
data <- read.csv("~/Desktop/StravaAnalysis/strava_activity.csv")

#fixing for mispellings of countries  
data$location_country[data$location_country == "The Netherlands" | 
                      data$location_country == "Nederland"] <- "Netherlands"
data$location_country[data$location_country == "Brasil"] <- "Brazil"

#removed rows that had an elapsed and moving time of 0
data <- filter(data, elapsed_time > 0 & moving_time > 0)

#find top exercise types by number of entries
topType <- select(data, type) %>%
  group_by(type) %>%
  summarise(numEntries = n()) %>%
  arrange(desc(numEntries)) 

#find top countries by number of entries
topCountry <- select(data, location_country) %>%
  group_by(location_country) %>%
  summarise(numEntries = n()) %>%
  arrange(desc(numEntries)) 
 
#creating dataframe for question 1 with cleaned data
cleaned.dataQ1 <- select(data, athlete.sex, average_speed, max_speed, distance, 
                         average_heartrate, max_heartrate, type) %>%
  
                  #use rows only from type of top 2 types by number of entries
                  filter(type == "Ride" | type == "Run") %>%
                  
                  #removed rows that didn't have a gender assigned to them
                  filter(athlete.sex == 'M' | athlete.sex == 'F')  %>%
                  
                  #removed rows that have an distance or speed of 0 or less
                  filter(distance > 0 & average_speed > 0) %>%
                  
                  #removed rows that have an average heart rate of 0 or less
                  filter(average_heartrate > 0)


#creating dataframe for question 2 with cleaned data
cleaned.dataQ2 <- select(data, location_country, average_watts, moving_time, elapsed_time, total_elevation_gain, type) %>%
                
                #use rows only from location_country of top 5 countries by number of entries
                filter(location_country == "United States" | location_country == "Australia" |
                       location_country == "United Kingdom" | location_country == "Netherlands" |
                       location_country == "Brazil") %>%
  
                #use rows only from type of top 5 types by number of entries         
                filter(type == "Ride" | type == "Run" |
                       type == "Walk" | type == "Swim" |
                       type == "Workout") %>%
    
                #creating new column to calculate a ratio between moving and elapsed time
                mutate(activeRatio = moving_time/elapsed_time)

#export dataframes as csv
write.csv(cleaned.dataQ1, file="cleanedDataQ1.csv", row.names = FALSE)
write.csv(cleaned.dataQ2, file="cleanedDataQ2.csv", row.names = FALSE)

