library(ggplot2)
df <- read.csv("~/Desktop/StravaAnalysis/cleanedDataQ2.csv", header = TRUE)

#randomly sample from 400
rand <-df[sample(nrow(df), 400), ]

g <- ggplot(rand ,aes(x=kudos_count)) 
g+geom_histogram(binwidth = 1, color = "Black", fill = "white")+labs(title="Distribution of Average Kudos", x="Kudos")

#randomly sample from 250
rand2 <-df[sample(nrow(df), 250), ]

#distribution of average watts
g <- ggplot(rand2 ,aes(x=average_watts)) 
g+geom_histogram(bins = 20, color = "Black", fill = "white")+labs(title="Distribution of Average Watts", x="Watts")

#distribution of kilojoules
g <- ggplot(rand2 ,aes(x=kilojoules)) 
g+geom_histogram(bins =20, color = "Black", fill = "white")+labs(title="Distribution of Average Kilojoules", x="Kilojoules")

#distribution of total elevation gain
g <- ggplot(rand2 ,aes(x=total_elevation_gain)) 
g+geom_histogram(bins=20, color = "Black", fill = "white")+labs(title="Distribution for Total Elevation Gain", x="Total Elevation Gain")

#distribution of active ratio
g <- ggplot(rand2 ,aes(x=activeRatio)) 
g+geom_histogram(bins=20, color = "Black", fill = "white")+labs(title="Distribution of Average Activeness (Active Ratio)", x="Active Ratio")

#distribution of athlete count
g <- ggplot(rand2 ,aes(x=athlete_count)) 
g+geom_histogram(bins=15, color = "Black", fill = "white")+labs(title="Distribution of Average Athlete Count", x="Athletes")

#distribution of location
g <- ggplot(rand2 ,aes(x=location_country)) 
g+geom_bar(color="Black", fill="white")+ labs(title="Distribution of Location", x="Location")
