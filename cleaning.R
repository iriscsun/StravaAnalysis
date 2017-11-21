#required libraries
require(dplyr)

#loading in stava acitivy data as dataframe 
data <- read.csv("strava_activity.csv")

#fixing for mispellings of countries  
data$location_country[data$location_country == "The Netherlands" | 
                      data$location_country == "Nederland"] <- "Netherlands"
data$location_country[data$location_country == "Brasil"] <- "Brazil"

#removed rows that had an elapsed time of 0
data <- filter(data, elapsed_time > 0)

#find top 2 exercise types by number of entries
top2 <- select(data, type) %>%
  group_by(type) %>%
  summarise(numEntries = n()) %>%
  arrange(desc(numEntries)) 

#find top 5 countries by number of entries
top5 <- select(data, location_country) %>%
  group_by(location_country) %>%
  summarise(numEntries = n()) %>%
  arrange(desc(numEntries)) 

#creating dataframe for question 1 with cleaned data
cleaned.dataQ1 <- select(data, athlete.sex, average_speed, max_speed, distance, 
                         average_heartrate, max_heartrate, type) %>%
  
                  #use rows only from type of top 2 types by entries
                  filter(type == "Ride" | type == "Run") %>%
  
                  #removed rows that didn't have a gender assigned to them
                  filter(data, athlete.sex == 'M' | athlete.sex == 'F')

#creating dataframe for question 2 with cleaned data
cleaned.dataQ2 <- select(data, location_country, average_watts, moving_time, elapsed_time, type) %>%
                
                #use rows only from location_country of top 5 countries by entries
                filter(location_country == "United States" | location_country == "Australia" |
                       location_country == "United Kingdom" | location_country == "Netherlands" |
                       location_country == "Brazil") %>%
  
                #creating new column to calculate a ratio between moving and elapsed time
                mutate(activeRatio = moving_time/elapsed_time)



#export dataframes as csv
write.csv(cleaned.dataQ1, file="cleanedDataQ1.csv", row.names = FALSE)
write.csv(cleaned.dataQ2, file="cleanedDataQ2.csv", row.names = FALSE)

