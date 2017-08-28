### Author: Shah Ayub Quadri ####
### Problem: Binary Classification problem ###
### Output: {0,1} ###

### SVM
### NeuralNet

rm(list = ls())

#set the working directory
  setwd("C:/Users/quadris/Desktop/data science/data set/Data Fest 2 April")
  
#read the data
  trainData <- read.csv("train.csv",header = T,sep = ",")
  testData <- read.csv("test.csv",header = T,sep = ",")
  
  # Remove Id values.
  
  train <- trainData[-1]
  
  
  
  # Feature Engineering 
    # 1. Check for Null Values
      sapply(train, function(x) sum(is.na(x)))
      sapply(testData, function(x) sum(is.na(x)))
      
    # 2. a Impute missing values
      library(DMwR)
      train <-centralImputation(train)
      test.raw <- centralImputation(testData)
        
    # 2. Drop ID column
      train = subset(trainData,select = -c(ID))


    # 3. Stratified sampling
      
      require(caTools)
      set.seed(123) 
      sample = sample.split(train$Outcome, SplitRatio = .70)
      train = subset(train, sample == TRUE)
      test = subset(train, sample == FALSE)
      
      
      #Model logistic regression
      
      glmOut <- glm(Outcome~., data =train,family = binomial)
      summary(glmOut)
      
      library(MASS)
      stepAIC(glmOut)
      
      finalGlm <- glm(formula = Outcome ~ timestamp + Stock_ID + Volume + Three_Day_Moving_Average + 
            Five_Day_Moving_Average + Ten_Day_Moving_Average + True_Range + 
            Average_True_Range + Positive_Directional_Movement + Negative_Directional_Movement, 
          family = binomial, data = train)

      
      # Predict the train accuracy and confusion matrix
      
      GlmPred <- predict(finalGlm,type = "response")
      pred_class_Glm <- ifelse(GlmPred>0.5,1,0)
      
      # Confusion Matrix
      ConfGLM.Mat<- table(train$Outcome,pred_class_Glm)
      
      Accuracy <- sum(diag(ConfGLM.Mat)/sum(ConfGLM.Mat))
      Accuracy
      
      Precisions <- ConfGLM.Mat[2,2]/sum(ConfGLM.Mat[,2])
      Precisions
      
      Recall <- ConfGLM.Mat[2,2]/sum(ConfGLM.Mat[2,])
      Recall          
      
      # Predict the Test Accuracy and confusion matrix
      testGlm.pred <- predict(finalGlm,test,type = "response")
      pred_test.Glm <- ifelse(testGlm.pred>0.5,1,0)
      
      # Confusion Matrix for Test set
      ConfGLM.Mat.test<- table(test$Outcome,pred_test.Glm)
      
      Accuracy.glm.test <- sum(diag(ConfGLM.Mat.test)/sum(ConfGLM.Mat.test))
      Accuracy.glm.test ##64.1%
      
      Precisions.glm.test <- ConfGLM.Mat.test[2,2]/sum(ConfGLM.Mat.test[,2])
      Precisions.glm.test ## 65.7%
      
      Recall.glm.test <- ConfGLM.Mat.test[2,2]/sum(ConfGLM.Mat.test[2,])
      Recall.glm.test  ##81.8%
      
      # test data prediction
      pred_Test.final <- predict(finalGlm,test.raw,type = "response")
      pred_Test.final <- ifelse(testGlm.pred>0.5,1,0)
      
      write.csv(pred_Test.final,"GLM55Percent.csv")
      