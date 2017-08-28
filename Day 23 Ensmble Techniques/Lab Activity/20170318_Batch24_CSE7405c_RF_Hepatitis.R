rm(list = ls(all = T))
setwd("D:\\ManojC\\20170318_Batch24_CSE7405c_Ensembles_Lab06")
#Read the data into R
hepatitis <- read.table('hepatitis.txt', header=F, dec='.',
                        col.names=c('target','age','gender','steroid','antivirals',
                                    'fatigue','malaise','anorexia','liverBig',
                                    'liverFirm','spleen','spiders','ascites',
                                    'varices','bili','alk','sgot','albu','protime',
                                    'histology'), na.strings=c('?'), sep=',')
#Study dataset
str(hepatitis)
table(hepatitis$target) 
str(hepatitis$target) # 1: Die; 2: Live 

#Convert 1s and 2s into 1s and 0s 
hepatitis$target= ifelse(hepatitis$target==1,0,1 ) # 0: Die(+ve); 1: Live (-ve)

# The numerical variables are: age,bili,alk,sgot,albu and protime
# The categorical variables are: the remaining 14 variables

# Seperate numerical and categorical variables
hepatitis_num <- hepatitis[,c(2,15:19)]
hepatitis_cat <- hepatitis[,-c(2,15:19)]

# Convert subsetted categorical variables into factors 
hepatitis_cat <- data.frame(apply(hepatitis_cat,2,function(x){as.factor(x)}))
str(hepatitis_cat)

# Combine the 2 datasets
hepatitis_merged <-cbind(hepatitis_num,hepatitis_cat)
str(hepatitis_merged)

# Handle missing values using knn imputation
sum(is.na(hepatitis_merged))
library(DMwR)
hepatitis_imputed <- knnImputation(data =hepatitis_merged,k = 5)
sum(is.na(hepatitis_imputed))

# Split dataset into train and test
rows <- seq(from = 1, to = nrow(hepatitis_imputed), by = 1)
set.seed(2020)
trainrows <- sample(x = rows, size = nrow(hepatitis_imputed) * 0.65)
trainR <- hepatitis_imputed[trainrows,] #all rows in trainrows & all columns of parent dataset
testR <- hepatitis_imputed[-trainrows,]

# Build the classification model using randomForest
library(randomForest)
hepatitis_rf <- randomForest(target~., data=trainR,ntree=100,do.trace=20) 

# View results and understand important attributes
print(hepatitis_rf)
hepatitis_rf$predicted 
hepatitis_rf$importance #gives 1st col(accuracy reduces if imp var are removed)
#importance(hepatitis_rf)
round(importance(hepatitis_rf), 2)   

# Extract and store important variables obtained from the random forest model
Imp_hepatitis_rf <- data.frame(hepatitis_rf$importance)
Imp_hepatitis_rf1 <- data.frame(row.names(Imp_hepatitis_rf),Imp_hepatitis_rf[,1])
colnames(Imp_hepatitis_rf1) = c('Attributes','Importance')
Imp_hepatitis_rf2 <- Imp_hepatitis_rf1[order(Imp_hepatitis_rf1$Importance , decreasing = TRUE),]
Imp_hepatitis_rf2 <- Imp_hepatitis_rf2[1:6,]

# plot (directly prints the important attributes) 
varImpPlot(hepatitis_rf)
#importance(hepatitis_rf)

# Predict on Train data 
pred_model_train <-predict(hepatitis_rf,trainR[,-c(7)],
                           type="response")

result_train <- table("actual _values"= trainR$target,
                      pred_model_train);result_train
#OR
#table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test <-predict(hepatitis_rf,testR[,-c(7)],
                          type="response", norm.votes=TRUE)
result_test <- table("actual _values"= testR$target,
                     pred_model_test);result_test

# Accuracy,Precision and Recall on testR
test_accuracy <- sum(diag(result_test))/sum(result_test)*100;test_accuracy
test_recall <- ((result_test[2,2])/(result_test[2,2]+result_test[2,1])*100);test_recall
test_precision <-((result_test[2,2])/(result_test[2,2]+result_test[1,2])*100);test_precision
############################# 

# Optimizing "mtry" to improve model/metrics

## find optimal value of mtry for randomForest
bestmtry <- tuneRF(trainR[,c(-7)],trainR$target, ntreeTry=100,
                    stepFactor=1.5,improve=0.01, trace=F, plot=F, dobest=FALSE)
 
best.m <- bestmtry[bestmtry[, 2] == min(bestmtry[, 2]), 1]
print(bestmtry)
print(best.m)

# Use the optimal number of variables selected at each split and 
#run random forest again
set.seed(71)
nw_rf <-randomForest(target~.,data=trainR, mtry=best.m, importance=TRUE,ntree=200)
print(nw_rf)
# Predicton Test Data
pred_model_test1 <-predict(nw_rf,testR[,-c(7)],
                          type="response", norm.votes=TRUE)
result_test1 <- table("actual _values"= testR$target,
                     pred_model_test1);result_test1

# Accuracy,Precision and Recall on testR
test_accuracy1 <- sum(diag(result_test1))/sum(result_test1)*100;test_accuracy
test_recall1 <- ((result_test1[2,2])/(result_test1[2,2]+result_test1[2,1])*100);test_recall
test_precision1 <-((result_test1[2,2])/(result_test1[2,2]+result_test1[1,2])*100);test_precision

