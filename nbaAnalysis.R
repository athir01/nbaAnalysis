## The data will be analyzed by Quantity, Location, Usage
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(GGally)

##Exploratory Analysis
data=read.csv("nbateam.csv", header = TRUE, stringsAsFactors = FALSE)
attach(data)
head(data, n=10)

##Categorical Variate W=1 L=0/Quantitative Variates
win<-as.numeric(data$WINorLOSS=="W")

##Linear Regression Model
data.lm <- lm(win~data$TeamPoints+data$OpponentPoints+data$FieldGoals+data$FieldGoalsAttempted+data$X3PointShots+data$X3PointShotsAttempted+data$FreeThrowsAttempted+data$OffRebounds+data$TotalRebounds+data$Assists+data$Steals+data$Blocks+data$Turnovers+data$TotalFouls+data$Opp.FieldGoals+data$Opp.FieldGoalsAttempted+data$Opp.3PointShots+data$Opp.3PointShotsAttempted+data$Opp.FreeThrowsAttempted+data$Opp.OffRebounds+data$Opp.TotalRebounds+data$Opp.Assists+data$Opp.Steals+data$Opp.Blocks+data$Opp.Turnovers+data$Opp.TotalFouls)
summary(data.lm)
plot(data.lm)


##Success Binary Against 3 points attempted
threepoint.lm <- lm(win~data$X3PointShotsAttempted)
summary(threepoint.lm)
plot(win~data$X3PointShotsAttempted)
abline(0.4412239,0.0022938)

##Logistical Regression model
data.logit <- glm(win~data$TeamPoints+data$OpponentPoints+data$FieldGoals+data$FieldGoalsAttempted+data$X3PointShots+data$X3PointShotsAttempted+data$FreeThrowsAttempted+data$OffRebounds+data$TotalRebounds+data$Assists+data$Steals+data$Blocks+data$Turnovers+data$TotalFouls+data$Opp.FieldGoals+data$Opp.FieldGoalsAttempted+data$Opp.3PointShots+data$Opp.3PointShotsAttempted+data$Opp.FreeThrowsAttempted+data$Opp.OffRebounds+data$Opp.TotalRebounds+data$Opp.Assists+data$Opp.Steals+data$Opp.Blocks+data$Opp.Turnovers+data$Opp.TotalFouls)
summary(data.logit)
anova(data.logit)


