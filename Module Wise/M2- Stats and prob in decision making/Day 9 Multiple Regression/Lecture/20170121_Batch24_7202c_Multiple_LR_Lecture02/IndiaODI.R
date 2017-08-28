setwd("F:/Users/AnandJayaraman/Documents/Research/Talks n Presentations/Insofe/RegressionTimeSeries/2017-01-14/Day2")

bike <- read.csv("BikeShare.csv")
str(bike)

OrigData <- bike

#Start by declaring which variables are categorical (or factors)
bike$weather <- factor(bike$weather)
bike$holiday <- factor(bike$holiday)
bike$workingday <- factor(bike$workingday)
bike$season <- factor(bike$season)

str(bike)

#create day of week column
bike$day <- weekdays(as.Date(bike$datetime))
bike$day <- factor(bike$day)

#Now lets extract date and time from the datetime stamp
bike$time <- substring(bike$datetime,12,20)
