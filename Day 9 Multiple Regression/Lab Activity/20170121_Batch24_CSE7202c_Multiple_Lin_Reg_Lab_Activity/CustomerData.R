### Author: Ayub Quadri ###
### Problem: Mulitple Linear Regression ###
### Data Set: CustomerData.csv ###
### Objective: build a regression model to predict the customer revenue based on other factors and understand the influence of other attributes on revenue.###


rm(list=ls())

#set working directory
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 9 Multiple Regression/Lab Activity/20170121_Batch24_CSE7202c_Multiple_Lin_Reg_Lab_Activity")

#read the data
  CustData <- read.csv("CustomerData.csv",header = T,sep = ",")
  
  # Drop Customer ID
    CustData <- subset(CustData, select = -c(CustomerID))
    
  # Check for the Null Values
    sapply(CustData, function(x) sum(is.na(x)))
  
    CustData$City <- as.factor(as.character(CustData$City))

  # Split the data into test and train  
    require(caTools)
    set.seed(123) 
    sample = sample.split(CustData$TotalRevenueGenerated, SplitRatio = .70)
    train = subset(CustData, sample == TRUE)
    test = subset(CustData, sample == FALSE)      
  
 # Apply the linear regression model
    
    lmOut = lm(TotalRevenueGenerated ~., data = train)
    summary(lmOut)
    
    library(MASS)
    step(lmOut, direction = "both")
    Model_AIC = lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
                     Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
                     NoOfGamesPlayed + NoOfGamesBought + FavoriteChannelOfTransaction + 
                     FavoriteGame, data = train)
    summary(Model_AIC)
    
    predicted <- predict(Model_AIC,train)
    
    conf_matrix = table(train$TotalRevenueGenerated,predicted)
    
    predicted <- predict(Model_AIC,test)
    residual = test$TotalRevenueGenerated - predicted
    plot(residual)
    