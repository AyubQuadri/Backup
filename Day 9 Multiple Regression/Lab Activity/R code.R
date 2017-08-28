rm(list = ls())
setwd()

data <- read.csv("CustomerData.csv",header = T, sep = ",")

str(data)
#subset the data without CustomerID
data1 <- subset(data, select = -CustomerID)

str(data1)
#convert city to factor 
data1$City <- as.factor(data1$City)
str(data1)


#split the data into train and test
library(caTools)
require(caTools)
set.seed(123) 
sample = sample.split(data1$TotalRevenueGenerated, SplitRatio = .70)
train = subset(data1, sample == TRUE)
test = subset(data1, sample == FALSE)

str(train)
str(test)
attach(data1)



#linar regression model for
lmOut <- lm(TotalRevenueGenerated ~.,data = train)
summary(lmOut)

#AIC
install.packages('MASS')
library("MASS")
require("MASS")
step(lmOut, direction="both")

model_AIC= lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
                Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
                NoOfGamesPlayed + NoOfGamesBought + FavoriteChannelOfTransaction  
                , data = train)
summary(model_AIC)

predicted <- predict(model_AIC,test)

residual = test$TotalRevenueGenerated - predicted

plot(residual)


