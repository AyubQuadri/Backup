rm(list=ls(all=TRUE))
# Set the working directory
setwd("D:/AMITS_LABS/BATCH 24/Day16_NN/20170212_Batch 24_CSE 7405c_ANN_LabActivity")

#install.packages("drat", repos="https://cran.rstudio.com") 
#drat:::addRepo("dmlc") 
#install.packages("mxnet") 

# Importing "housing.csv" files's data into R dataframe using read.csv function.
housing_data = read.csv(file="housing.csv", header=TRUE, sep=",")

# Understand the structure the summary of the data using str and summary R commands
str(housing_data)
summary(housing_data)

# Convert all the variables to appropriate type
housing_data$CHAS = as.factor(housing_data$CHAS)

library(dummies)
CHAS = dummy(housing_data$CHAS)
housing_data = subset(housing_data, select=-c(CHAS)) 
housing_data = cbind(housing_data, CHAS)

# Separate Target Variable and Independent Variables.
# In this case "OwnOcc" is a target variable and all others are independent variable. 
target_Variable = housing_data$OwnOcc
independent_Variables = subset(housing_data, select = -c(OwnOcc))

# Standardization the independent variables using decostand funcion in vegan R library
library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables = decostand(independent_Variables,"range")

housing_data = data.frame(independent_Variables, OwnOcc = target_Variable)
rm(independent_Variables, target_Variable)

#Split dataset into train and test
library(caret)
set.seed(1234)
intrain = createDataPartition(y = housing_data$OwnOcc, p=0.7, list = F)
train.x = data.matrix(housing_data[intrain, -15])
train.y = housing_data[intrain, 15]
test.x = data.matrix(housing_data[-intrain, -15])
test.y = housing_data[-intrain, 15]

require(mxnet)
mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=1, activation="tanh",out_activation="rmse",
                num.round=20, array.batch.size=100, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.rmse)
Sys.time() -> end
paste(end-start)

preds = predict(model, test.x)
preds=t(preds)

## Auto detect layout of input matrix, use rowmajor..

library(DMwR)
regr.eval(preds,test.y)

