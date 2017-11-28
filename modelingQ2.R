df <- read.csv("~/Desktop/StravaAnalysis/cleanedDataQ2.csv", header = TRUE)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)

#multiple step regression
linearMod = lm(df$kudos_count ~ df$location_country + df$athlete_count + df$kilojoules + df$average_watts + df$total_elevation_gain + df$activeRatio, data = df)
summary(linearMod)

#using backward-stepwise regression
fit = lm(kudos_count ~ location_country + athlete_count + kilojoules + average_watts + activeRatio, data = df)
step <- stepAIC(fit, direction="backward")
step$anova

#linear model for country, athlete count, kilojoules, and averae_watts
linearCountry <- lm(kudos_count ~ location_country + athlete_count + kilojoules + average_watts,data = df)
summary(linearCountry)
print(linearCountry)

#plot residual
res <- resid(linearCountry)
plot(df$kudos_count , res, ylab="Residual", xlab="Fitted Model") 
abline(0, 0)
