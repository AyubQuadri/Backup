rm(list=ls(all=TRUE))

# Set directory and read the data 
setwd() 
data<-read.csv("CustomerData.csv",header=T)

# Data exploration
str(data)
summary(data)
levels(data$FavoriteGame)

# remove CustomerID column
data = data[,-1]

# convert City attribute as factor
data$City = as.factor(data$City)
dataForModel = data

# split the data into train and test data sets
rows=seq(1,nrow(dataForModel),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(dataForModel))/100)
train = dataForModel[trainRows,] 
test = dataForModel[-trainRows,] 

# BUILD LINEAR REGRESSION MODEL 

# build model with all attributes into model 
LinReg1<- lm(TotalRevenueGenerated ~ ., data=train)
summary(LinReg1)
#predict(LinReg1)
#resid(LinReg1) OBS-PRED 


par(mfrow=c(2,2))
plot(LinReg1)
plot(LinReg1,which=4)
par(mfrow=c(1,1)) #reset default
#hist(LinReg1$residuals)

# build model with significant attributes
LinReg2<-lm(TotalRevenueGenerated~.-NoOfGamesPlayed,data=train)
summary(LinReg2)

LinReg3<-lm(TotalRevenueGenerated~.-NoOfGamesPlayed-MaxAgeOfChild,data=train)
summary(LinReg3)

# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg2$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg2,test)
regr.eval(test$TotalRevenueGenerated, Pred)
####################################################
#Scaling the data
library(vegan)
str(data)
data_cat <- data[,c(1,11,12)]
data_num <-data[,-c(1,11,12)]
data_num_std <- data.frame(decostand(data_num[,-10],method = "range"),"TotalRevenueGenerated" = data_num$TotalRevenueGenerated)
#in the above line we exclude the target variable during standardization

#Combine standardized attributes back with the categorical attributes
data_std_final <-data.frame(data_cat,data_num_std)

#Split the data into train and test data sets
rows=seq(1,nrow(data_std_final),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data_std_final))/100)
train_std = data_std_final[trainRows,] 
test_std = data_std_final[-trainRows,]

# Build linear regression with all attributes
LinReg_std1<-lm(TotalRevenueGenerated~., data=train_std)
summary(LinReg_std1)

# Check for multicollinearity and perform dimensionality reduction analysis

# 1. VIF: (check attributes with high VIF value)
library(car)
vif(LinReg_std1)
str(train_std)

# 2. Stepwise Regression
library(MASS)
Step1 <- stepAIC(LinReg_std1, direction="backward")
# Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinReg1, direction="both")

Mass_LinReg1 <- lm(TotalRevenueGenerated~City+MinAgeOfChild+FavoriteChannelOfTransaction
                   +FrquncyOfPurchase+FavoriteGame+NoOfGamesBought+MinAgeOfChild
                   +Tenure+FrequencyOFPlay+NoOfUnitsPurchased,data=train_std)

summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))

# build model without the influencial point (record #2729) 
which(rownames(train_std)%in%c(2729))
#train_std[230,]
LinReg_No_infl<- lm(TotalRevenueGenerated ~ ., data=train_std[-230,])
summary(LinReg_No_infl)

#Error verification on train data
regr.eval(train_std$TotalRevenueGenerated, Mass_LinReg1$fitted.values) 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,test_std)
regr.eval(test$TotalRevenueGenerated, MASS_Pred1)


Error_calc = data.frame(train_std$TotalRevenueGenerated,Mass_LinReg1$fitted.values)
write.csv(x = Error_calc,file = "Error_calc.csv")

