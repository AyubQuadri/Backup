rm(list = ls())

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Scholarship Test")

    #read the data
    rawData <- read.csv("data_train.csv", header = T, sep = ",")
    
    duplicate_col= read.csv('X_new_duplicate.csv',header = F,sep=",")
    testData <- read.csv("data_test.csv",header = T,sep = ",")
    
 
    
# Remove the duplicate columns & columns which are zero
    
    rawData= rawData[-1]
    testData= testData[-1]
    duplicate_col = duplicate_col[-1]
    
    a = subset(rawData, select = -c(duplicate_col$V2))
    a_test = subset(testData, select = -c(duplicate_col$V2))
    
    b=  a[,which(!apply(a,2,FUN = function(x){all(x == 0)}))]
    
    b_test = a_test[,which(!apply(a_test,2,FUN = function(x){all(x == 0)}))]
    
    b$TARGET = as.factor(as.character(b$TARGET))

# Stratified Splitting the data
    require(caTools)
    set.seed(123) 
    sample = sample.split(a$TARGET, SplitRatio = .70)
    train = subset(a, sample == TRUE)
    test = subset(a, sample == FALSE)



    # Logestic Regression model
    lmOut <- glm(TARGET~.,  data=rawData, family=binomial)
    summary(lmOut)
    
    prob <- predict(lmOut, type ="response")
      
    pred_class <- ifelse(prob>0.5,1,0)
    
    conf_matrix = table(train$TARGET,pred_class)
    
    # Accuracy
      Acc_GLM = sum(diag(conf_matrix))/sum(conf_matrix);Acc_GLM
    #precession
      Precession_GLM = conf_matrix[2,2]/sum(conf_matrix);Precession_GLM
    # Recall
      Recall_GLM = conf_matrix[2,2]/sum(conf_matrix[2,]);Recall_GLM
      
      
    library(MASS)  
      stepAIC()
        
    # ROC  
      pred_Train_test = predict(lmOut,test,type="response")
      
      pred_train_test_class = ifelse(pred_Train_test>0.5,1,0)
      
      conf_train_test = table(test$TARGET,pred_train_test_class)      
            
      Acc_test_train_GLM = sum(diag(conf_train_test))/sum(conf_train_test);Acc_test_train_GLM
      #precession
      Precession_GLM = conf_matrix[2,2]/sum(conf_matrix)
      # Recall
      Recall_GLM = conf_matrix[2,2]/sum(conf_matrix[2,]) 
      
    
  # Prediction of Test data
      
      pred_TEST = predict(lmOut,b_test,type="response")
      