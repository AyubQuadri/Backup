rm(list = ls())

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 14 Module Test")

Train <- read.csv("income.csv",header = T,sep = ",")
Test <- read.csv("income_val.csv",header = T,sep = ",")


#Check for Null values

  sapply(Train,function(x)sum(is.na(x)))
  sapply(Test,function(x)sum(is.na(x)))
  
  Train$income <- as.factor(as.character(Train$income))
  Test$income <- as.factor(as.character(Test$income))

#logistic regression
  
  glmOut <- glm(income~.,data = Train,family = "binomial")
  glmOut
  summary(glmOut)

#SetAIC
  library(MASS)
  stepAIC(glmOut)
  Glm_Model <- glm(formula = income ~ age + type_employer + education + marital + 
                     occupation + relationship + race + sex + capital_gain + capital_loss + 
                     hr_per_week + country, family = "binomial", data = Train)
  Glm_Model
  summary(Glm_Model)

#VIF
  library(car)
  
  vif(Glm_Model)
  
  pred <- predict(Glm_Model,type = "response")
  
  pred_class <- ifelse(pred>0.5,1,0)
  
  Conf.Mat1 = table(Train$income,pred_class)
  
  Accuracy_train <- sum(diag(Conf.Mat1)/sum(Conf.Mat1))
  Accuracy_train
  
  Precisions_train <- Conf.Mat1[2,2]/sum(Conf.Mat1[,2])
  Precisions_train
  
  Recall_train <- Conf.Mat1[2,2]/sum(Conf.Mat1[2,])
  Recall_train
  
  #pridict on test data
  
  pred_test <- predict(Glm_Model, Test, type = "response")
  
  pred_class_test <- ifelse(pred_test>0.5,1,0)
  
  Conf.Mat2 <- table(Test$income,pred_class_test)
  
  Accuracy_test <- sum(diag(Conf.Mat2)/sum(Conf.Mat2))
  Accuracy_test
  
  Precisions_test <- Conf.Mat2[2,2]/sum(Conf.Mat2[,2])
  Precisions_test
  
  Recall_test <- Conf.Mat2[2,2]/sum(Conf.Mat2[2,])
  Recall_test
  
  #ROCR 
  #install.packages('ggplot2')
  library(ROCR)
  library(ggplot2)
  
  predicted <- predict(Glm_Model,type = "response")
  prob<- prediction(predicted,Train$income)
  
  tprfpr <- performance(prob,"tpr","fpr")
  AUC<- performance(prob,measure = "auc")
  AUC@y.values[[1]]
  
  plot(tprfpr)
  str(tprfpr)
  
  cutoff <- data.frame(cut <- tprfpr@alpha.values[[1]],fpr<-tprfpr@x.values[[1]],tpr=tprfpr@y.values[[1]])
  
  cutoff <- cutoff[order(cutoff$tpr,decreasing = TRUE),]
  
  head(cutoff)
  
  plot(tprfpr,colorize= TRUE,print.cutoff.at=seq(0,1,by=0.1),text.adj = c(-0.2,1.7))
  
  tpr<-unlist(slot(tprfpr,"y.values"))
  fpr<-unlist(slot(tprfpr,"x.values"))
  
  # ROC 
  roc <-data.frame(tpr,fpr)
  #Area Under Curve AUC
  
  
  ggplot(roc)+geom_line(aes(x=fpr,y=tpr))+
    geom_abline(intercept=0,slope=1,color="gray")+
    ylab("sensivitity")+xlab("1-specificity")
