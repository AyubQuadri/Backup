library(caret)
library(MASS)
library(car)
library(Metrics)

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

set.seed(107)
#Lets divide the dataset into Training data and Test data
inTrain <- createDataPartition(y = bike$count, p = .75,list = FALSE) #75% data is training data

# First attempt at building the linear model
lmbike0 <- lm(count~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed,data=bike[inTrain,])
summary(lmbike0)

TestData <- bike[-inTrain,]
#Lets extract the predictions of the model for the TestData
OutputForTest0 <- predict(lmbike0,newdata=TestData)

#Lets compute the root-mean-square error between actual and predicted
 Error0<-rmse(TestData$count,OutputForTest0)

#create day of week column
bike$day <- weekdays(as.Date(bike$datetime))
bike$day <- factor(bike$day)

#Now lets extract date and time from the datetime stamp, which is between 12th & 20th character of timestamp
bike$time <- substring(bike$datetime,12,20)  #substring cuts a input string at the places given by the arguments

hournum <- as.numeric(substr(bike$time,1,2))

# Lets see which weekdays has how much demand
aggregate(bike[,"count"],list(bike$day),mean)

#second attempt
lmbike1 <- lm(count~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day +time,data=bike[inTrain,])

summary(lmbike1)

TestData <- bike[-inTrain,]
#Lets extract the predictions of the model for the TestData
OutputForTest1 <- predict(lmbike1,newdata=TestData)

#Lets compute the root-mean-square error between actual and predicted
 Error1<-rmse(TestData$count,OutputForTest1)


#create daypart column, default to 0 to make things easier for ourselves
bike$daypart <- "0"
bike$daypart[(hournum < 22) & (hournum >= 7)] <- 1

#create daypart column, default to 4 to make things easier for ourselves
bike$daypart <- "4"


#4AM - 10AM = 1
bike$daypart[(hournum < 10) & (hournum > 3)] <- 1

#11AM - 3PM = 2
bike$daypart[(hournum < 16) & (hournum > 9)] <- 2

#4PM - 9PM = 3
bike$daypart[(hournum < 22) & (hournum > 15)] <- 3

#convert daypart to factor
bike$daypart <- as.factor(bike$daypart)


#second attempt
lmbike2 <- lm(count~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day +time+daypart,data=bike[inTrain,])
summary(lmbike2)

# The NA in the coefficients indicate that some of the inputs are perfectly correlated
#Daypart doesn't seem to add any value

stepOut<-stepAIC(lmbike2, direction="both")

OutputForTest2 <- predict(stepOut,newdata=TestData)
Error2<-rmse(TestData$count,OutputForTest2)
Error2

summary(OutputForTest2)
summary(TestData$count)

#We don't want negative values as forecast for bike count!
Output2Mod <- OutputForTest2
Output2Mod[OutputForTest2<=0] <-1  #Replace all negative numbers with 1

rmse(TestData$count,Output2Mod)

#If we want to penalize under-prediction of demand, rmsle might be a better metric
Error2<-rmsle(TestData$count,Output2Mod)

#Lets check the residuals 
par(mfrow=c(2,2))
plot(stepOut)



#third attempt
lmbike3 <- lm(log(count)~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day +time+daypart,data=bike[inTrain,])
summary(lmbike3)

# The NA in the coefficients indicate that some of the inputs are perfectly correlated
#Daypart doesn't seem to add any value

stepOut<-stepAIC(lmbike3, direction="both")

OutputForTest3 <- predict(stepOut,newdata=TestData)
OutputForTest3 <- exp(OutputForTest3)
Error3<-rmsle(TestData$count,OutputForTest3)
Error3


 

