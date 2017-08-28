rm(list=ls(all=TRUE))
setwd("D:/AMITS_LABS/BATCH 24/Day19_SVM")
 
bankdata=read.csv("UniversalBank.csv", header=TRUE, sep=",")

#OBJECTIVE: Will a person take a personal loan or not? 
#Response variable is "Personal.Loan" 

# Understanding the data 
names(bankdata)
summary(bankdata)
str(bankdata)
head(bankdata)

# Removing unnecessary columns ID and zipcode
bankdata_1=subset(bankdata, select=-c(ID,ZIP.Code))

# Create dummy variables for Education
table(bankdata_1$Education)

library("dummies") 
Educations = dummy(bankdata_1$Education)	
str(Educations)
bankdata_2 = subset(bankdata_1,select=-c(Education)) 
bankdata_3 = cbind(bankdata_2,Educations)
str(bankdata_3)

# Standardize the data  
# install.packages("vegan")
library(vegan)
bankdata_4 = data.frame(decostand(bankdata_3[,-c(7)],"range"),"Personal.Loan"=bankdata_1$Personal.Loan) # standardize using 'Range' method

# Separate dataset into train and test
set.seed(123)
train = sample(1:5000,0.6*5000) # to take a random sample of  60% of the records for train data 
train_bankdata = bankdata_4[train,] 
nrow(train_bankdata) #no of rows
test = (1:5000) [-train] # to take a random sample of  30% of the records for test data 
test_bankdata = bankdata_4[test,] 
nrow(test_bankdata)  #no of rows

#Alternate method to seperate dataset into train & test
# rows <- seq(1,nrow(),1)
# set.seed(100)
# trainrows <- sample(rows,0.6*nrow(bankdata_4))
# train_bankdata <- bankdata_4[trainrows,]
# test_bankdata <- bankdata_4[-trainrows,]

# Data Summary for the response variable "Personal.Loan"
table(bankdata_4$Personal.Loan)
table(train_bankdata$Personal.Loan)
table(test_bankdata$Personal.Loan)

####Classification using "e1071"####
# install.packages("e1071")
library(e1071)

# Store the independent variables and target variable separately
# (for easy use)
x = subset(train_bankdata, select = -Personal.Loan) #remove response variable
y = as.factor(train_bankdata$Personal.Loan) #convert response variable to factor

# Building the model on train data
model  =  svm(x = x, y = y, type = "C-classification", kernel = "linear", cost = 10)
summary(model)
#The "cost" parameter balances the trade-off between having a large margin and classifying
#all points correctly. It is important to choose it well to have good
#generalization.

# Predict on train data using the model
pred_train  =  predict(model, x) # x is all the input variables
table(pred_train)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
# compare actual (i.e. "y") vs. predicted (pred_train)
tb_train = table(y,pred_train)#actual is on left and predicted shown on top

# Calculate error metrics
accuracy_train = sum(diag(tb_train))/sum(tb_train); accuracy_train
recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1]));recall_train

# Predict on train data using the model
a  =  subset(test_bankdata, select = -Personal.Loan) #remove response variable
b  =  as.factor(test_bankdata$Personal.Loan) #convert response variable to factor
pred_test = predict(model, a)
table(pred_test)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
#compare actual (i.e. "b") vs. predicted (pred_test)
tb_test <- table(b,pred_test)
accuracy_test = sum(diag(tb_test))/sum(tb_test);accuracy_test
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1]));recall_test
#########END########

#######Build SVM model with RBF kernel#### 
model = svm(x,y, method = "C-classification", kernel = "radial", cost = 10,
            gamma = 0.1)
summary(model)

# Predict on train data
pred_train  =  predict(model, x)
table(pred_train)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
# compare actual (i.e. "y") vs. predicted (pred_train)
tb_train = table(y,pred_train)

# Calculate error metrics
accuracy_train = sum(diag(tb_train))/sum(tb_train);accuracy_train
recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1]));recall_train

# Predict on test data
a  =  subset(test_bankdata, select = -Personal.Loan) #remove response variable
b  =  as.factor(test_bankdata$Personal.Loan)
pred_test = predict(model, a)
table(pred_test)

#Build confusion matrix
tb_test <- table(b,pred_test)
accuracy_test = sum(diag(tb_test))/sum(tb_test);accuracy_test
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1]));recall_test

#####Classification using "KSVM"############
#install.packages("kernlab")
library(kernlab)
names(train_bankdata)
#Build model using ksvm with "rbfdot" kernel
kern_rbf <- ksvm(as.matrix(train_bankdata[,-14]),train_bankdata[,14],
                  type='C-svc',kernel="rbfdot",kpar="automatic",
                  C=10, cross=5)
kern_rbf

#Build model using ksvm with "vanilladot" kernel
kern_vanilla <- ksvm(as.matrix(train_bankdata[,-14]),train_bankdata[,14],
                     type='C-svc',kernel="vanilladot", C = 10)
kern_vanilla

#Predict model "kern_rbf" (on test data)
kpred_rbf<- predict(kern_rbf,test_bankdata[-14])
confMatrix <- table(test_bankdata$Personal.Loan, kpred_rbf)
acc_rbf = sum(diag(confMatrix))/sum(confMatrix);acc_rbf
rec_rbf = (confMatrix[2,2]/(confMatrix[2,2]+confMatrix[2,1]));rec_rbf

#Predict model "kern_vanilla" (on test data) 
kpred_vanilla<- predict(kern_vanilla,test_bankdata[-14])
confMatrix <- table(test_bankdata$Personal.Loan, kpred_vanilla)
acc_vanilla = sum(diag(confMatrix))/sum(confMatrix);acc_vanilla
rec_vanilla = (confMatrix[2,2]/(confMatrix[2,2]+confMatrix[2,1]));rec_vanilla
################################
# In order to improve the performance of SVM model we will need
# to select the best parameters for the model. Default is epsilon = 0.1
# and c = 10. The process of choosing these parameters is called
# hyperparameter tuning The standard way of doing it is by doing a
# grid search. 

#Grid Search/Hyper-parameter tuning
summary(train_bankdata)
tuneResult <- tune(svm, train.x = x, train.y = y, 
                   ranges = list(gamma = 10^(-3:-1), cost = 2^(2:3)))
print(tuneResult) 
summary(tuneResult)
#obj <- tune.svm(Personal.Loan~., data = train_bankdata, gamma = 10^(-3:-1),cost = 2^(2:3))

#Predict model and calculate errors
tunedModel <- tuneResult$best.model;tunedModel
tunedModelY <- predict(tunedModel, as.matrix(x)) 
conf <- table(y, tunedModelY);conf
acc_tune <- sum(diag(conf))/(sum(conf));acc_tune
rec_tune <-(conf[2,2]/(conf[2,2]+conf[2,1]));rec_tune

#Predict on test
tunedModelY_test <- predict(tunedModel, as.matrix(a)) 
conf_test <- table(b, tunedModelY_test);conf_test
acc_tune_test <- sum(diag(conf_test))/(sum(conf_test));acc_tune_test
rec_tune_test <-(conf_test[2,2]/(conf_test[2,2]+conf_test[2,1]));rec_tune_test




