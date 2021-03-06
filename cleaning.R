#required libraries
require(dplyr)

#loading in stava acitivy data as dataframe 
data <- read.csv("~/Desktop/StravaAnalysis/strava_activity.csv")

#fixing for mispellings of countries  
data$location_country[data$location_country == "The Netherlands" | 
                      data$location_country == "Nederland"] <- "Netherlands"
data$location_country[data$location_country == "Brasil"] <- "Brazil"

#remove rows that had an elapsed and moving time of 0, 
data <- filter(data, elapsed_time > 0 & moving_time > 0,
               
               #remove rows with exercise lasting longer than a day
               elapsed_time < 86400)

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
 
#creating dataframe for question 1, remove rows that have a higher avg speed than max speed
cleaned.dataQ1 <- filter(data, average_speed < max_speed) %>%
  
                  #add sex, average speed, distance, average heart rate, and type to dataframe 
                  select(athlete.sex, average_speed, distance, average_heartrate, type) %>%
  
                  #use rows only from type of top 2 types by number of entries
                  filter(type == "Ride" | type == "Run") %>%
                  
                  #remove rows that didn't have a gender assigned to them
                  filter(athlete.sex == 'M' | athlete.sex == 'F')  %>%
                  
                  #remove rows that have an distance or speed of 0 or less
                  filter(distance > 0 & average_speed > 0) %>%
  
  
                  #remove rows that have an average heart rate of 0 or less
                  filter(average_heartrate > 0)


#creating dataframe for question 2 with cleaned data
cleaned.dataQ2 <- select(data, location_country, average_watts, kilojoules, moving_time, elapsed_time, athlete_count, total_elevation_gain, kudos_count, type) %>%
  
                  #use rows only from location_country of top 5 countries by number of entries
                  filter(location_country == "United States" | location_country == "Australia" |
                         location_country == "United Kingdom" | location_country == "Netherlands" |
                         location_country == "Brazil") %>%
                  #omit na
                  na.omit() %>%
  
                  #use rows only from type of top 5 types by number of entries         
                  filter(type == "Ride") %>%
                  
                  #using rows with more than 0 kudos but at most 30
                  filter(kudos_count > 0) %>%
                  filter(kudos_count <= 30) %>%
  
                  #creating new column to calculate a ratio between moving and elapsed time
                  mutate(activeRatio = moving_time/elapsed_time)

#export dataframes as csv
write.csv(cleaned.dataQ1, file="cleanedDataQ1.csv", row.names = FALSE)
write.csv(cleaned.dataQ2, file="cleanedDataQ2.csv", row.names = FALSE)
