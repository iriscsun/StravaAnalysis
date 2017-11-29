require(dplyr)
require(ggplot2)

#reading in cleaned data for Question 1
genderData <- read.csv("cleanedDataQ1.csv")

#filtering on activity types by gender, will use for speed and distance
f.rides <- filter(genderData, athlete.sex == "F", type == "Ride")
m.rides <- filter(genderData, athlete.sex == "M", type == "Ride")

f.runs <- filter(genderData, athlete.sex == "F", type == "Run")
m.runs <- filter(genderData, athlete.sex == "M", type == "Run")

#filtering by gender, will use for heart-rate
f.hr <- filter(genderData, athlete.sex == "F")
m.hr <- filter(genderData, athlete.sex == "M")

#randomly sample 250 entries from each gender for rides
randF.rides <- f.rides[sample(nrow(f.rides), 250), ]
randM.rides <- m.rides[sample(nrow(m.rides), 250), ]

#randomly sample 200 entries from each gender for runs
randF.runs <- f.runs[sample(nrow(f.runs), 200), ]
randM.runs <- m.runs[sample(nrow(m.runs), 200), ]

#randomly sample 400 entries from each gender for heart-rate
randF.hr <- f.hr[sample(nrow(f.hr), 400), ]
randM.hr <- m.hr[sample(nrow(m.hr), 400), ]

#combine rides from both genders
rides <- rbind(randF.rides, randM.rides)

#combine runs from both genders
runs <- rbind(randF.runs, randM.runs)

#combine both genders for heart-rate
hr <- rbind(randF.hr, randM.hr)

#export dataframes as csv for modeling
write.csv(rides, file="genderRides.csv", row.names = FALSE)
write.csv(runs, file="genderRuns.csv", row.names = FALSE)
write.csv(hr, file="genderHR.csv", row.names = FALSE)

#Plot distribution of average ride speeds for each gender
rideSpeed.plot <- ggplot(rides ,aes(x=average_speed)) + 
  geom_histogram(data=subset(rides , athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.5) +
  geom_histogram(data=subset(rides , athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.5) +
  labs(x = "Average Speed (Meters per Second)", y = "Count", 
       title = "Distribution of Average Speeds for Bike Rides") +
  scale_fill_manual(name="Gender", values=c("light green","darkgray"),labels=c("Female","Male")) +
  theme(legend.position = c(0.15,0.85)) +
  ylim(0, 45) +
  xlim(0, 12.5)

#Plot distribution of ride distances for each gender
rideDistance.plot <- ggplot(rides, aes(x=distance)) + 
  geom_histogram(data=subset(rides, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  geom_histogram(data=subset(rides, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Distance (Meters)", y = "Count", 
       title = "Distribution of Distances for Bike Rides") +
  scale_fill_manual(name="Gender", values=c("light blue","darkgray"),labels=c("Female","Male")) +
  theme(legend.position = c(0.45,0.8)) +
  xlim(0, 150000)

#Plot distribution of average run speeds for each gender
runSpeed.plot <- ggplot(runs, aes(x=average_speed)) + 
  geom_histogram(data=subset(runs, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.5) +
  geom_histogram(data=subset(runs, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.5) +
  labs(x = "Average Speed (Meters per Second)", y = "Count", 
       title = "Distribution of Average Speeds for Runs") +
  theme(legend.position = c(0.85,0.85)) +
  scale_fill_manual(name="Gender", values=c("light green","darkgray"),labels=c("Female","Male"))

#Plot distribution of run distances for each gender
runDistance.plot <- ggplot(runs, aes(x=distance)) + 
  geom_histogram(data=subset(runs, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  geom_histogram(data=subset(runs, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Distance (Meters)", y = "Count", 
       title = "Distribution of Distances for Runs") +
  scale_fill_manual(name="Gender", values=c("light blue","darkgray"),labels=c("Female","Male")) +
  theme(legend.position = c(0.75,0.75)) + 
  xlim(0, 25000)

#Plot distribution of average heart rates for each gender
heartRate.plot <- ggplot(hr, aes(average_heartrate)) +
  geom_histogram(data=subset(hr, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.4) +
  geom_histogram(data=subset(hr, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Average Heart Rate (Beats per Minute)", y = "Count", 
       title = "Distribution of Average Heart Rates") +
  scale_fill_manual(name="Gender", values=c("red","darkgray"),labels=c("Female","Male")) +
  theme(legend.position = c(0.15,0.80))


#reading in cleaned data for Question 2
df <- read.csv("~/Desktop/StravaAnalysis/cleanedDataQ2.csv", header = TRUE)

g <- ggplot(df ,aes(x=kudos_count)) 
g+geom_histogram(binwidth = 1, color = "Black", fill = "white")+labs(title="Distribution of Kudos", x="Kudos")

#distribution of average watts
g <- ggplot(df ,aes(x=average_watts)) 
g+geom_histogram(bins = 20, color = "Black", fill = "white")+labs(title="Distribution of Average Watts", x="Watts")

#distribution of kilojoules
g <- ggplot(df ,aes(x=kilojoules)) 
g+geom_histogram(bins =20, color = "Black", fill = "white")+labs(title="Distribution of Kilojoules", x="Kilojoules")

#distribution of total elevation gain
g <- ggplot(df ,aes(x=total_elevation_gain)) 
g+geom_histogram(bins=20, color = "Black", fill = "white")+labs(title="Distribution for Total Elevation Gain", x="Total Elevation Gain")

#distribution of active ratio
g <- ggplot(df ,aes(x=activeRatio)) 
g+geom_histogram(bins=20, color = "Black", fill = "white")+labs(title="Distribution of Activeness (Active Ratio)", x="Active Ratio")

#distribution of athlete count
g <- ggplot(df ,aes(x=athlete_count)) 
g+geom_histogram(bins=15, color = "Black", fill = "white")+labs(title="Distribution of Athlete Count", x="Athletes")

#distribution of location
g <- ggplot(df ,aes(x=location_country)) 
g+geom_bar(color="Black", fill="white")+ labs(title="Distribution of Location", x="Location")


