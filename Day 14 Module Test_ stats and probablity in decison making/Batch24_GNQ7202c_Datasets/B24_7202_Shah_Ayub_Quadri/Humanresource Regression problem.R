### Author: Shah Ayub Quadri ####
### Date:   5th Feb 2016     ###

rm(list = ls())
setwd()
#a. Read the data set HumanResourceanaltytics.csv

  HumanRes <- read.csv("Humanresourceanalytics.csv",header = T,sep = ",")

#Feature Engineering

#b. check for null values
    sapply(HumanRes,function(x)sum(is.na(x)))
    #No Null values so no need to impute
    
#c. Convert the attributes if needed
    #Number of projects -> factor
    #Whether they have had a work accident
    #Whether they have had a promotion in the last 5 years
    #Levels
    #timespent Exp
    #left-> need to be predicted
    
    HumanRes$number_project <- as.factor(as.character(HumanRes$number_project)) 
    HumanRes$Work_accident <- as.factor(as.character(HumanRes$Work_accident)) 
    HumanRes$promotion_last_5years <- as.factor(as.character(HumanRes$promotion_last_5years)) 
    HumanRes$Level <- as.factor(as.character(HumanRes$Level)) 
    HumanRes$left<- as.factor(as.character(HumanRes$left)) 
    #HumanRes$time_spend_company <- as.integer(as.character(HumanRes$time_spend_company)) 
    
#d. Slipt the data into Test and train
    #using CAtool on left attribute
  
    library(caTools)
  
    set.seed(123)
    sample = sample.split(HumanRes$left, SplitRatio = 0.7)
    train = subset(HumanRes,sample==TRUE)
    test = subset(HumanRes,sample == FALSE)
    
#e. General Logestic Regression model GLM
    
    glmOut<- glm(left~., data = train, family="binomial")
    
    summary(glmOut)
#f. with Glm the current model has AIC of 6457.2 which can be still reduced by onther method with the help of stepAIC
    
#g. SetAIC for the Glm model
    library(MASS)
    stepAIC(glmOut)
    
    #glm model provided by Step AIC with minimum AIC value of 6453 amongst all other models.
    
    Glm_Model <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                       average_montly_hours + time_spend_company + Work_accident + 
                       promotion_last_5years + Department + salary, family = "binomial", 
                     data = train)
    
    summary(Glm_Model)
    
#Prediction based on Train & Test data and there Accuracy 
    
    #prediction for Train data at threshold 0.5
    
      pred <- predict(Glm_Model,type = "response")
      
      pred_class <- ifelse(pred>0.5,1,0)
      
      Conf.Mat1 = table(train$left,pred_class)
      
      Accuracy_train <- sum(diag(Conf.Mat1)/sum(Conf.Mat1))
      Accuracy_train
      
      Precisions_train <- Conf.Mat1[2,2]/sum(Conf.Mat1[,2])
      Precisions_train
      
      Recall_train <- Conf.Mat1[2,2]/sum(Conf.Mat1[2,])
      Recall_train
    
#h. check for best threshold value
      install.packages('ggplot2')
      library(ROCR)
      library(ggplot2)
      
      predicted <- predict(Glm_Model,type = "response")
      prob<- prediction(predicted,train$left)
      
      tprfpr <- performance(prob,"tpr","fpr")
      AUC<- performance(prob,measure = "auc")
      AUC@y.values[[1]]
      #Area under the Curve is 91%
      
      plot(tprfpr)
      
      cutoff <- data.frame(cut <- tprfpr@alpha.values[[1]],fpr<-tprfpr@x.values[[1]],tpr=tprfpr@y.values[[1]])
      
      cutoff <- cutoff[order(cutoff$tpr,decreasing = TRUE),]
      
      head(cutoff)
      
      plot(tprfpr,colorize= TRUE,print.cutoff.at=seq(0,1,by=0.1),text.adj = c(-0.2,1.7))
      
    # with TPR vs FPR graph we interpret that threshold should be at 0.8 
      tpr<-unlist(slot(tprfpr,"y.values"))
      fpr<-unlist(slot(tprfpr,"x.values"))
      
    # ROC 
      roc <-data.frame(tpr,fpr)
      
    #graph of sensitivity vs 1-specificity that give AUC
      ggplot(roc)+geom_line(aes(x=fpr,y=tpr))+
        geom_abline(intercept=0,slope=1,color="gray")+
        ylab("sensivitity")+xlab("1-specificity")
      
      #prediction for test data at threshold 0.5
      
      pred_test <- predict(Glm_Model, test, type = "response")
      
      pred_class_test <- ifelse(pred_test>0.5,1,0)
      
      Conf.Mat2 <- table(test$left,pred_class_test)
      
      Accuracy_test <- sum(diag(Conf.Mat2)/sum(Conf.Mat2))
      Accuracy_test
      
      Precisions_test <- Conf.Mat2[2,2]/sum(Conf.Mat2[,2])
      Precisions_test
      
      Recall_test <- Conf.Mat2[2,2]/sum(Conf.Mat2[2,])
      Recall_test
      
      # Model predicts pretty well on both test and train data 
      #Accuracy at Train = 86.67%
      #Accuracy at Test  = 86.68%
      
      
      
#i. finalized model is give below
      
      Glm_Model <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                         average_montly_hours + time_spend_company + Work_accident + 
                         promotion_last_5years + Department + salary, family = "binomial", 
                       data = train)