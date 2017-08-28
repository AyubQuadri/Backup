###Author: Shah Ayub Quadri####

  rm(list = ls())

  setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 16 Neural Networks/LAB ACTIVITY")

#read the data
  housing <- read.csv("housing.csv",header = T,sep = ",")
  housing$CHAS<-as.factor(as.character(housing$CHAS))
  housing$RAD<-as.factor(as.character(housing$RAD))
#Feature Engineering
  #pre processing
    #check for null values
      sapply(housing, function(x)sum(is.na(x)))
    
    #convert the categorical int values to numeric
      unique(housing$RAD)
      housing_cat <- housing[,c("CHAS","RAD")]
      
      
      library(dummies)
      housing_catg_dummy <- dummy.data.frame(housing_cat,sep = ".")
      str(housing_catg_dummy)
      
      housing_num <- housing[,c("Crimerate","ResiLandZone","INDUS","NOX","Rooms","AGE","Distance","TAX","PTRATIO","Blacks","LSTAT","RAD") ]
      
      housing_num <- data.frame(apply(housing_num,2,function(x){as.character(x)}))
      housing_num <- data.frame(apply(housing_num,2,function(x){as.numeric(x)}))
      
      # Standardization the independent variables using decostand funcion in vegan R library
      library(vegan)
      # Note: To standardize the data using 'Range' method
      independent_Variables = decostand(housing_num, "range")
      

      
      # Seperate the target value from the data set     
      
      Target <-subset(housing,select = ("OwnOcc"))
      str(Target)
     
      
      #Combine all attributes into final dataframe
      Final_Data <-data.frame(housing_catg_dummy,independent_Variables,Target)
      str(Final_Data)
      sum(is.na(Final_Data)) 
      
      
      library(caret)
      set.seed(999)
      intrain = createDataPartition(y = Final_Data$OwnOcc, p=0.7, list = F)
      train.x = data.matrix(Final_Data[intrain, -14])
      train.y = Final_Data[intrain, 14]
      test.x = data.matrix(Final_Data[-intrain, -14])
      test.y = Final_Data[-intrain, 14]
      
      require(mxnet)
      mx.set.seed(0)
      Sys.time() -> start
        model <- mx.mlp(train.x, train.y, hidden_node=c(10), out_node=1, activation="tanh", out_activation="rmse",
                      num.round=100, array.batch.size=100, learning.rate=0.02, momentum=0.7,
                      eval.metric=mx.metric.rmse)
      Sys.time() -> end
      paste(end - start)
      
      preds = predict(model, test.x)
      
      preds=t(preds)
      
      library(DMwR)
      result <- regr.eval(preds,test.y)
      