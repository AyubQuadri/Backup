  rm(list=ls())
  
  setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 21 KNN Collaborative Filters/Lab activity")
  getwd()

#read the data
  data = read.csv("UniversalBank.csv",header = T,sep=",")
  str(data)
  
  #check for Null values
    sapply(data, function(x)sum(is.na(x)))
  
  #Remove Id and zip
    data = subset(data, select= -c(ID,ZIP.Code))
  
  # Convert Education to factor
    data$Education = as.factor(as.character(data$Education))
  
  #load dummy library
    library(dummies)
    Education1 = dummy(data$Education)
    BankData = subset(data, select = -c(Education))
    BankData1= cbind(BankData,Education1)
  
  # KNN Without Standardizing
    # Split the data into test train
      require(caTools)
      set.seed(123) 
      sample = sample.split(BankData1$Personal.Loan, SplitRatio = .70)
      train = subset(BankData1, sample == TRUE)
      test = subset(BankData1, sample == FALSE)
    
    #Seperate Y and X values in the data set
      X_Train = subset(train, select = -c(Personal.Loan))
      X_Test = subset(test, select = -c(Personal.Loan))
      
    # Run the KNN model on test and train with k =1,2,4,6,8,10
      library(class)
      pred = knn(X_Train,X_Test,train$Personal.Loan,k=1)
      
      a = table(pred,test$Personal.Loan)      
      a = sum(diag(a)/nrow(X_Test))
      a
    
  # KNN Without Standardizing
    #Standardise the data set 
      library(vegan)
      stadBank = decostand(BankData1,"range")
      
      # Split the data into Test and Train
        require(caTools)
        set.seed(213)
        sample = sample.split(stadBank$Personal.Loan, SplitRatio =.70)
        trainStd = subset(stadBank,sample == TRUE)
        testStd = subset(stadBank,sample == FALSE)
        
      # Seperate X and Y values in data sets Train and test
        X_Std_Train = subset(trainStd, select = -c(Personal.Loan))
        X_Std_Test = subset(testStd, select = -c(Personal.Loan))
        
      # Run the KNN model on TestStd and TrainStd with Kvalues
        
        predStd = knn(X_Std_Train,
                      X_Std_Test,
                      trainStd$Personal.Loan,
                      k=1)
        aStd = table(predStd,testStd$Personal.Loan)
        aStd = sum(diag(aStd)/nrow(X_Std_Test))
        aStd
    
  # Reduce the Complexity of the Model
        
        keep = condense(X_Std_Train,trainStd$Personal.Loan)
        
        # KNN model after Condense
        
          predCond = knn(X_Std_Train[keep,],
                         X_Std_Test,
                         trainStd$Personal.Loan[keep],
                         k=5)
          aCond = table(predCond,testStd$Personal.Loan)
          aCondAccuracy = sum(diag(aCond))/nrow(X_Std_Test)        
  
  # indices of the records that are considered for prediction
          
          install.packages("FNN")
          library(FNN)          
          
          predFNN = FNN::knn(X_Std_Train[keep,], 
                             X_Std_Test, 
                             trainStd$Personal.Loan[keep], 
                             k =5)
          aFNN <- table(predFNN,testStd$Personal.Loan)
          aFNNAccuracy = sum(diag(aFNN))/nrow(X_Std_Test)
          
          indices=knnx.index(X_Std_Train[keep,], 
                             X_Std_Test, k=5)   
          print(indices[20, ])  
          