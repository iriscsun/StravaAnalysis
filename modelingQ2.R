df <- read.csv("~/Desktop/StravaAnalysis/cleanedDataQ2.csv", header = TRUE)
library(dplyr)
library(tidyr)
#average watts
powerdf <- df%>%
  group_by(location_country) %>%
  filter(average_watts > 0) %>%
  summarize(mean_power = mean(average_watts, na.rm = TRUE))

p <- ggplot(powerdf, aes(x= location_country, y= mean_power, group = 1))
p +geom_line(stat = "identity")+labs(title = "Average Athlete Power for each Country", y="Power (Average)", x = "Location")

#average elevation gain
elev <- df%>%
  group_by(location_country) %>%
  filter(total_elevation_gain > 0) %>%
  summarize(elevation_avg = mean(total_elevation_gain, na.rm = TRUE))

p <- ggplot(elev, aes(x= location_country, y= elevation_avg, group = 1))
p +geom_line(stat = "identity")+labs(title = "Average Athlete Power for each Country", y="Power (Average)", x = "Location")

#moving time and elapsed time ratio
time <- df %>%
  group_by(location_country) %>%
  summarise(active_time = mean(activeRatio, na.rm = TRUE))

p <- ggplot(time, aes(x= location_country, y=active_time, group = 1))
p +geom_line(stat = "identity")+labs(title = "Activity in each Country", y="Activity (moving time/elapsed time)", x = "Location")

#top activity with all countried combined
typeAll <- df %>%
  group_by(type) %>%
  summarise(freq=n())

p <- ggplot(typeAll, aes(x= type, y=freq, group = 1))
p +geom_bar(stat = "identity")+labs(title = "Total Count of Activities in Countries", y="Frequency", x = "Location")

#activity type by each country
typeCountry <- df %>%
  group_by(location_country, type) %>%
  summarise(freq=n())

  
p<- ggplot(typeCountry, aes(location_country, typeCountry$freq, fill = typeCountry$type, order = typeCountry$type))+geom_bar(stat="identity")
p+labs(title = "Type of Activity for each Country", y="Frequency", x = "Location")+ scale_fill_discrete(name = "Activity Type")

two <- df %>%
  filter(location_country == "United States"|location_country == "United Kingdom") %>%
  select(location_country, average_watts)

US <- two %>%
  filter(location_country == "United States")
UK <- two %>%
  filter(location_country == "United Kingdom")

US <- US %>% drop_na(average_watts)
UK <- UK %>% drop_na(average_watts)

var.test(US$average_watts, UK$average_watts)

qf(0.95, 1041, 820)
#tabulated F is more than computed F, which means there is homogeneity of variances

t.test(US$average_watts, UK$average_watts, var.equal=TRUE, paired=FALSE)
