rm(list = ls())

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 20 SVM/lab activity")

#read the data

Bank <- read.csv("UniversalBank.csv",header = T,sep = ",")

Bank <- Bank[-1]
Bank <- Bank[-4]
#Bank <- Bank[-6]


library(dummies)
EduDummyVars<-dummy(Bank$Education)
head(EduDummyVars)
Bank<-data.frame(Bank,EduDummyVars)
Bank <- Bank[-6]
head(Bank)


library(vegan)
Bank <- decostand(Bank[,-Bank$Personal.Loan],"range")

# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(Bank) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- Bank[trainingRows, ] # training data
testData <- Bank[-trainingRows, ] # test data

#install.packages("e1071") 
library(e1071)
y= trainingData$Personal.Loan
unique(y)
x = subset(trainingData, select = c(-Personal.Loan))
head(x)
#buildModel
tuned <- tune.svm(Personal.Loan ~., data = trainingData, gamma = 10^(-3:1), cost = 2^(0:9))

summary(tuned) 

compareTable <- table(predict(model),);compareTable  
