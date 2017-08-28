###Author: Shah Ayub Quadri####
###Goal: to predict t

rm(list = ls())

#set working directory

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 16 Neural Networks/LAB ACTIVITY")

# Read the data
train <- read.csv("train_sample.csv")
test <- read.csv("test_sample.csv")

# convert into matrix

train <- data.matrix(train)
test <- data.matrix(test)

train.x <- train[,-1]
train.y <- train[,1]
train.x <- (train.x/255)



test.x <- test[,-1]
test.y <- test[,1]
test.x <- (test.x/255)

require(mxnet)

model1<-mx.mlp(train.x, train.y, hidden_node =c(128,64), out_node=10, 
               dropout = NULL, activation = "relu", out_activation = "softmax", 
               num.round=10, array.batch.size=100, learning.rate=0.07,  
               momentum=0.9, eval.metric=mx.metric.accuracy) 


preds <- predict(model1, test.x) 
dim(preds) 
pred.label <- max.col(t(preds)) - 1 
table(pred.label) 
pred.label 
head(pred.label) 
table(test.y,pred.label) 
sum(diag(table(test.y,pred.label)))/1000 

