# PROBLEM 1: Target variable = "Revenue", classification problem
# Apply both C50 and RPART algorithms

#1. Remove objects from environment 
rm(list=(ls(all=TRUE)))

#2. Setting working directory path
setwd("")

#3. Reading raw data,removing,CustID,etc.
Cust_Data <-  read.csv("CustomerData.csv",sep=",",header=T,fill=T) 
names(Cust_Data)
# Remove the cutomer ID attribute
Cust_Data <-Cust_Data[,-1]
str(Cust_Data)

#4. Data Pre-processing
#(a) Handling missing values
sum(is.na(Cust_Data))

# Note: Study which attributes are inherently categorical or numeric
# categorical: FavoriteChannelOfTransaction,  FavoriteGame and City
# numeric: rest are all numeric

#(b) Converting categorical attribute "City" from integer to factor
Cust_Data$City <- as.factor(as.character(Cust_Data$City))
str(Cust_Data)

library(DMwR)
# Fill the NULL/NA values using KNN imputation technique
Cust_imputed <- knnImputation(data = Cust_Data,k=10)   
sum(is.na(Cust_imputed))
# To Omit: Cust_Data1 <- na.omit(Cust_Data)

# Rounding the decimal vaules to zero
Cust_Data$NoOfChildren <- round(Cust_Data$NoOfChildren,0)
Cust_Data$FrquncyOfPurchase <- round(Cust_Data$FrquncyOfPurchase,0)
Cust_Data$FrequencyOFPlay <- round(Cust_Data$FrequencyOFPlay,0)
Cust_Data$NoOfGamesBought <- round(Cust_Data$NoOfGamesBought,0)

#(c) Discretizing (or bin) numerical attributes 
# Subset all the numerical,categorical and Target attributes
Cust_target <- subset(Cust_imputed,select = c(Revenue))
Cust_categorical <- subset(Cust_imputed,select = c(City,FavoriteChannelOfTransaction,FavoriteGame))
Cust_numerical <- subset(Cust_imputed,select = -c(City,FavoriteChannelOfTransaction,FavoriteGame,Revenue))

# Load library to bin the data
library(infotheo)
Cust_numerical_binned <- discretize(X = Cust_numerical,disc="equalwidth")
#Cust_numerical_binned <- discretize(X = Cust_numerical,disc="equalfreq")
str(Cust_numerical_binned)

Cust_numerical_binned <- data.frame(apply(Cust_numerical_binned, 2,as.character))
str(Cust_numerical_binned)

#(d) Dicretize (or bin) the target variable "Revenue" into 2 levels (Regular, Premium)
range(Cust_target$Revenue)
Cust_target_catg <-Cust_target
Cust_target_catg$Revenue <-ifelse(Cust_target_catg$Revenue<150,"Regular","Premium")#,ifelse(Cust_target_catg$Revenue<400,"Medium","High"))
str(Cust_target_catg$Revenue)
table(Cust_target_catg$Revenue)

#(e) Convert "Revenue" into factor
Cust_target_catg$Revenue = as.factor(Cust_target_catg$Revenue)
str(Cust_target_catg)
levels(Cust_target_catg$Revenue)

#(f) Combining categorical, the discretized numerical and the target into a new dataset
Cust_final = cbind(Cust_categorical,Cust_numerical_binned,Cust_target_catg)

#5. Split the data into train and test
rows <- seq(1,nrow(Cust_final),1)
set.seed(100)
trainrows <- sample(rows, nrow(Cust_final)*0.7)
Cust_train <- Cust_final[trainrows,]
Cust_test <- Cust_final[-trainrows,]

#6. Build classification model using C50
library(C50)

#a. Build model
DT_C50 <- C5.0(Revenue~.,data=Cust_train)
summary(DT_C50)

#b. Predict "Revenue" for train and test datasets
pred_Train = predict(DT_C50,newdata=Cust_train, type="class")
pred_Test = predict(DT_C50, newdata=Cust_test, type="class")

#c.Confusion Matrix on Train Data
C50_train_Conf_Matrix = table(Cust_train$Revenue,pred_Train);C50_train_Conf_Matrix

#e. Confusion Matrix on Test Data
C50_test_Conf_Matrix = table(Cust_test$Revenue,pred_Test);C50_test_Conf_Matrix

#f. Compute the evaluation metric
accuracy_C50_train = round((sum(diag(C50_train_Conf_Matrix))/sum(C50_train_Conf_Matrix))* 100,2)
accuracy_C50_train
accuracy_C50_test = round((sum(diag(C50_test_Conf_Matrix))/sum(C50_test_Conf_Matrix))*100,2)
accuracy_C50_test

#g. Check variable importance
C5imp(DT_C50, pct=TRUE)

#7. Build classification model using RPART
library(rpart)

#a. Build model
DT_rpart_class<-rpart(Revenue~.,data=Cust_train,method="class")
printcp(DT_rpart_class)
DT_rpart_class
#summary(DT_rpart_class)

#b. Predict "Revenue" for train and test datasets
pred_Train = predict(DT_rpart_class,newdata=Cust_train, type="class")
pred_Test = predict(DT_rpart_class, newdata=Cust_test, type="class")

#c. Confusion Matrix on Train Data
Rpart_train_Conf_Matrix = table(Cust_train$Revenue,pred_Train);Rpart_train_Conf_Matrix 

#e. Confusion Matrix on Test Data
Rpart_test_Conf_Matrix = table(Cust_test$Revenue,pred_Test);Rpart_test_Conf_Matrix

#f. Compute the evaluation metric
accuracy_rpart_train = round((sum(diag(Rpart_train_Conf_Matrix))/sum(Rpart_train_Conf_Matrix))* 100,2)
accuracy_rpart_train
accuracy_rpart_test = round((sum(diag(Rpart_test_Conf_Matrix))/sum(Rpart_test_Conf_Matrix))*100,2)
accuracy_rpart_test
  
# Choosing Best CP
DT_rpart_class1<-rpart(Revenue~.,data=Cust_train,method="class",control = rpart.control(cp=0.001))
printcp(DT_rpart_class1)
plotcp(DT_rpart_class1)
plotcp(DT_rpart_class, minline=TRUE, col="blue", lwd=2, lty=1)# draw line 1 SD above minimum rel.error

DT_rpart_class1<-rpart(Revenue~.,data=Cust_train,method="class",control = rpart.control(cp=0.0050968))
printcp(DT_rpart_class1)

#b. Predict "Revenue" for train and test datasets
pred_Train1 = predict(DT_rpart_class1,newdata=Cust_train, type="class")
pred_Test1 = predict(DT_rpart_class1, newdata=Cust_test, type="class")

#c. Confusion Matrix on Train Data
Rpart_train_Conf_Matrix1 = table(Cust_train$Revenue,pred_Train1);
Rpart_train_Conf_Matrix1

#e. Confusion Matrix on Test Data
Rpart_test_Conf_Matrix1 = table(Cust_test$Revenue,pred_Test1);
Rpart_test_Conf_Matrix1

#f. Compute the evaluation metric
accuracy_rpart_train1 = round((sum(diag(Rpart_train_Conf_Matrix1))/sum(Rpart_train_Conf_Matrix1))* 100,2)
accuracy_rpart_train1
accuracy_rpart_test1 = round((sum(diag(Rpart_test_Conf_Matrix1))/sum(Rpart_test_Conf_Matrix1))*100,2)
accuracy_rpart_test1

###########################################
# Regression
#(f) Combining categorical, the discretized numerical and the target into a new dataset
Cust_finalReg = cbind(Cust_categorical,Cust_numerical_binned,Cust_target)

#5. Split the data into train and test
rows <- seq(1,nrow(Cust_finalReg),1)
set.seed(100)
trainrows <- sample(rows, nrow(Cust_finalReg)*0.7)
Cust_trainReg <- Cust_finalReg[trainrows,]
Cust_testReg <- Cust_finalReg[-trainrows,]

#7. Build classification model using RPART
library(rpart)
library(rpart.plot)

#a. Build model
DT_rpart_Reg<-rpart(Revenue~.,data=Cust_trainReg,method="anova")

DT_rpart_Reg<-rpart(Revenue~.,data=Cust_trainReg,method="anova",control = rpart.control(cp = 0.01960033))
printcp(DT_rpart_Reg)
DT_rpart_Reg

rpart.plot(DT_rpart_Reg)

predCartTrain=predict(DT_rpart_Reg, newdata=Cust_trainReg, type="vector")
predCartTest=predict(DT_rpart_Reg, newdata=Cust_testReg, type="vector")

regr.eval(Cust_trainReg[,"Revenue"], predCartTrain, train.y = Cust_trainReg[,"Revenue"])
regr.eval(Cust_testReg[,"Revenue"], predCartTest, train.y = Cust_testReg[,"Revenue"])


