#output: price value
#Problem: Regression 
#Models 
# SVM
# Linear
# Dpart
# 


rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Module Wise/M3- Methods and Algorithms in Machine Learning/GNQ Data Set")

#Read the data set
data <- read.csv("SaratogaHouses_Labday.csv",header = T,sep = ",")


#Check for Null values
  sapply(data, function(x) sum(is.na(x)))
  
#Convert to dummies 
  library(dummies)
  library(dplyr)
  library(vegan)
  
  categorical <- select_if(data,is.factor)
  
  categorical <- dummy.data.frame(categorical)
  categorical
  numerical <- select_if(data, is.numeric)
  std
  dataTotal <- cbind(numerical,categorical)
  
  stdCategorical <- decostand(categorical,"range")
  
  
# Normailize data
  
  StdData <- decostand(dataTotal,"range")
  
# Split the data into train and test
  
  
#Model
  #linear regression
  
      lmOut <- lm(price~.,data = StdData)  
      summary(lmOut)
      
      library(MASS)
      stepAIC(lmOut)
      
      lmfinal <- lm(formula = price ~ lotSize + age + landValue + livingArea + 
                      bedrooms + bathrooms + rooms + `heatinghot air` + waterfrontNo + 
                      newConstructionNo + centralAirNo, data = StdData)
      summary(lmfinal)
      
  #predict the values
      predLm <- predict(lmfinal,dataTotal)
      
      