require(dplyr)

#reading in randomly sampled data for Question 1
genderRides <- read.csv("genderRides.csv")
genderRuns <- read.csv("genderRuns.csv")
genderHR <- read.csv("genderHR.csv")

#reshaping ride data for t-test
m.rides <- filter(genderRides, athlete.sex == "M") %>%
  select(average_speed, distance)
f.rides <- filter(genderRides, athlete.sex == "F") %>%
  select(average_speed, distance)

#reshaping run data for t-test
m.runs <- filter(genderRuns, athlete.sex == "M") %>%
  select(average_speed, distance)
f.runs <- filter(genderRuns, athlete.sex == "F") %>%
  select(average_speed, distance)

#reshaping heart rate data for t-test
m.hr <- filter(genderHR, athlete.sex == "M") %>%
  select(average_heartrate)
f.hr <- filter(genderHR, athlete.sex == "F") %>%
  select(average_heartrate)

#t-tests for speeds and distance on activity type ride
t.test(m.rides$average_speed, f.rides$average_speed, alternative = "greater", conf.level = .95)
t.test(m.rides$distance, f.rides$distance, alternative = "greater", conf.level = .95)

#t-tests for speeds and distance on activity type run
t.test(m.runs$average_speed, f.runs$average_speed, alternative = "greater", conf.level = .95)
t.test(m.runs$distance, f.runs$distance, alternative = "greater", conf.level = .95)

#t-tests for heart rate
t.test(m.hr, f.hr, alternative = "greater", conf.level = .95)


