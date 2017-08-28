rm(list=ls())

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Scholarship Test")

#read the data
rawData <- read.csv("data_train.csv", header = T, sep = ",")
test <- read.csv("data_test.csv",header = T, sep = ",")

#preprocessing
  rawData = rawData[-1]
  test <- test[-1]
  
  
  
   duplicate_col= read.csv('X_new_duplicate.csv',header = F,sep=",")


  a = subset(rawData, select = -c(duplicate_col$V2))

  
  sapply(a,function(x) sum(is.na(x)))
  
  # 3. Stratified sampling
  
  require(caTools)
  set.seed(123) 
  sample = sample.split(a$TARGET, SplitRatio = .70)
  train = subset(a, sample == TRUE)
  test = subset(a, sample == FALSE)
  
  # Model SVM
  
  glmOut = glm(TARGET ~.,data=rawData,family = binomial)
  
  pred_test = predict(glmOut,test,type="response")
  
  pred_train_test_class = ifelse(pred_test>0.5,1,0)
  
  conf_train_test = table(test$TARGET,pred_train_test_class)      
  
  Acc_test_train_GLM = sum(diag(conf_train_test))/sum(conf_train_test);Acc_test_train_GLM
  #precession
  Precession_GLM = conf_matrix[2,2]/sum(conf_matrix)
  # Recall
  Recall_GLM = conf_matrix[2,2]/sum(conf_matrix[2,]) 
  