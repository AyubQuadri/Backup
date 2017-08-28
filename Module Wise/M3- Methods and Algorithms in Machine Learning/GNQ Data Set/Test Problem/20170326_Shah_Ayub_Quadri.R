### Author: Shah Ayub Quadri ###
### Set No: set2_B ###

### Output:  Target categorical
### Problem type: Binary Classification 
### Models to be Built
  # C5.0
  # Logestic Regression
  # SVM
  # NN
  # KNN
  # Ensemble
  # ADABOOST

rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Module Wise/M3- Methods and Algorithms in Machine Learning/GNQ Data Set/Test Problem")

# Read the data set
  data <- read.csv("Auto Insurance.csv",header = T,sep = ",")

  #a. Necessary Preprocessing
  
      # Check for null values (No Null values)
        sapply(data, function(x) sum(is.na(x)))
      
      # Dummify the categoircal values
        library(dummies)
        library(dplyr)
        categorical <- select_if(data, is.factor)
        numerical <- select_if(data,is.numeric)
        
        dumCat <- dummy.data.frame(categorical)
        
      # Scale Numeric data 
        library(vegan)
        StdNumeric <- decostand(numerical,"range")        
      
      # combine Dummy data and Scaled Numeric data
        TotalData <- cbind(StdNumeric,dumCat)
        
        TotalData$Target <-as.factor(as.character(TotalData$Target))
        
      # Split the data into test and train
        require(caTools)
        set.seed(123) 
        sample = sample.split(TotalData$Target, SplitRatio = .70)
        train = subset(TotalData, sample == TRUE)
        test = subset(TotalData, sample == FALSE)
        
        train.x <- subset(train,select = -c(Target))
        train.y <- subset(train,select = c(Target))
        
        test.x  <-subset(test,select = -c(Target))
        test.y <-subset(test,select = c(Target))
        
        
  #b. Building Models
     # Logisitc Regression
          library(MASS)
        
          glmOut <- glm(Target~., data = train, family = binomial)  
          summary(glmOut)
          
          stepAIC(glmOut)
          
          finalGlm <- glm(formula = Target ~ P4 + P8 + P10 + P6A1 + P6A2 + P6A3 + P6A4 + 
                            P7MediumRisk + P11Type2 + P11Type3 + P12L2, family = binomial, 
                          data = train)
          summary(finalGlm)
          
          # Predict the train accuracy and confusion matrix
          
          GlmPred <- predict(finalGlm,type = "response")
          pred_class_Glm <- ifelse(GlmPred>0.5,1,0)
          
          # Confusion Matrix
          ConfGLM.Mat<- table(train$Target,pred_class_Glm)
          
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
          ConfGLM.Mat.test<- table(test$Target,pred_test.Glm)
          
          Accuracy.glm.test <- sum(diag(ConfGLM.Mat.test)/sum(ConfGLM.Mat.test))
          Accuracy.glm.test ##64.1%
          
          Precisions.glm.test <- ConfGLM.Mat.test[2,2]/sum(ConfGLM.Mat.test[,2])
          Precisions.glm.test ## 65.7%
          
          Recall.glm.test <- ConfGLM.Mat.test[2,2]/sum(ConfGLM.Mat.test[2,])
          Recall.glm.test  ##81.8%
          
      # Dicision Trees
          # C5.0
          library(C50)
          Model_C50 <- C5.0(Target~.,data = train)
          Model_C50
          summary(Model_C50)
          table(train$Target)
          
          #Predicting on Train (already shown in summary)
          P1_train=predict(Model_C50,train);P1_train
          conf.Mat.C5<-table(train$Target,Predicted=P1_train)
          
          Accuracy.c5 <- sum(diag(conf.Mat.C5)/sum(conf.Mat.C5))
          Accuracy.c5
          
          Precisions.c5 <- conf.Mat.C5[2,2]/sum(conf.Mat.C5[,2])
          Precisions.c5
          
          Recall.c5 <- conf.Mat.C5[2,2]/sum(conf.Mat.C5[2,])
          Recall.c5 
          
          #Predicting on Test
          P1_test = predict(Model_C50,test);P1_test
          conf.Mat.test.C5 <-table(test$Target,Predicted=P1_test)
          
          Accuracy.c5.test <- sum(diag(conf.Mat.test.C5)/sum(conf.Mat.test.C5))
          Accuracy.c5.test ##67.1
          
          Precisions.c5.test <- conf.Mat.test.C5[2,2]/sum(conf.Mat.test.C5[,2])
          Precisions.c5.test ##65.1
          
          Recall.c5.test <- conf.Mat.test.C5[2,2]/sum(conf.Mat.test.C5[2,])
          Recall.c5.test ##95.3
          
          
    # SVM Model
          library(e1071)
          SVMmodel <- svm(Target~., data = train, kernel = "linear",cost = 10)
          
          summary(SVMmodel)
          
          compareTable <- table(predict(SVMmodel),train$Target);compareTable 
          
          Accuracy.SVM <- sum(diag(compareTable)/sum(compareTable))
          Accuracy.SVM ## 64%
          
          Precisions.SVM <- compareTable[2,2]/sum(compareTable[,2]);Precisions.SVM #91.6%
          
          Recall.SVM <- compareTable[2,2]/sum(compareTable[2,]);
          Recall.SVM
          
          #confusion matrix on test data
          
          conf.Mat.SVM <- table(test$Target, predict(SVMmodel,test));
          conf.Mat.SVM 
          
          Accuracy.SVM.test <- sum(diag(conf.Mat.SVM)/sum(conf.Mat.SVM))
          Accuracy.SVM ## 64%
          
          Precisions.SVM <- conf.Mat.SVM[2,2]/sum(conf.Mat.SVM[,2])
          Precisions.SVM ## 63%
          
          Recall.SVM <- conf.Mat.SVM[2,2]/sum(conf.Mat.SVM[2,])
          Recall.SVM  ## 90%    
          
    # Neural Netowrks
          library(mxnet)
          mx.set.seed(0)
          Sys.time() -> start
          model <- mx.mlp(train.x, train.y, hidden_node=c(10), out_node=1, activation="tanh", out_activation="logistic",
                          num.round=20, array.batch.size=100, learning.rate=0.07, momentum=0.9,
                          eval.metric=mx.metric.accuracy)
          Sys.time() -> end
          paste(end - start)
          
          preds = predict(model, test.x)
          
          preds=t(preds)
          pred.label = ifelse(preds<0.55, 0, 1)
          
          conf.mat = table(pred.label, test.y);conf.mat
          accuracy = sum(diag(conf.mat))/sum(conf.mat);accuracy
          precision = conf.mat[2,2]/sum(conf.mat[2,]);precision
          recall = conf.mat[2,2]/sum(conf.mat[,2]);recall
          
          table(test.y)
          
    # KNN
          
          library(class)
          KNNmodel <- knn(train.x, test.x, train$Target, k = 5)
          
          conf.Mat.Knn <-table(KNNmodel,test$Target)
          
          Accuracy.knn.test <- sum(diag(conf.Mat.Knn)/sum(conf.Mat.Knn))
          Accuracy.knn.test ##56.9
          
          Precisions.knn.test <- conf.Mat.Knn[2,2]/sum(conf.Mat.Knn[,2])
          Precisions.knn.test ##70.4
          
          Recall.knn.test <- conf.Mat.Knn[2,2]/sum(conf.Mat.Knn[2,])
          Recall.knn.test ##61.8
          
        
          
    # ADABOOST
          library(ada)
          
          model = ada(train.x, train.y, iter=20, loss="logistic") # 20 Iterations 
          model
          
          # predict the values using model on test data sets. 
          pred = predict(model, a);pred 
          
          # calculate precision, recall and accuracy 
          result <- table(pred, b);result # 0(-ve) and 1(+ve)
          accuracy <- sum(diag(result))/sum(result)*100;accuracy
          recall <- ((result[2,2])/(result[2,2]+result[1,2])*100);recall
          precision <-((result[2,2])/(result[2,2]+result[2,1])*100);precision
          
          
#D. Finialize the best model based on the appropriate metrix
        
        Accuracy.glm.test
        Accuracy.c5.test 
        Accuracy.knn.test
        Accuracy.SVM.test
        
        x <- rbind(Accuracy.glm.test,Accuracy.c5.test,Accuracy.knn.test,Accuracy.SVM.test)
        plot(x)
        # C.5 Decision tree give me the best result amongst all
        