rm(list = ls(all = T))
setwd("D:/ManojC/20170318_Batch24_CSE7405c_Ensembles_Lab06")

data <- read.csv("income_val.csv")
str(data)
data$income <-as.factor(data$income )
# Split data into train and test
library(caret)
inTrain <-createDataPartition(data$income,p=0.7,list=FALSE)
training <-data[inTrain,]
testing <-data[-inTrain,]
nrow(training)
nrow(testing)

# Create RF model
library(randomForest)
rf <- randomForest(income~., data=training, ntree=500)
# "mtry" is the number of variables tried at each split 
table(predict(rf,testing), testing$income)
rf



