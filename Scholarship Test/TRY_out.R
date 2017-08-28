### Author: Shah Ayub Quadri ###
### Scholarship Test Problem on banking ###

#Visualisation
# 1. Density plots of var_15, Num_var4, Var38 

#Preprocessing steps
# 1. check for null values
# 2. remove those columns which are totally null reduce to 303 from 371
# 3. 

rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Scholarship Test")

rawDataset <- read.csv("data_train.csv", header = T, sep = ",")
testData <- read.csv("data_test.csv",header = T,sep = ",")




# 1. Data visualisation using density plot

    # visualizing data 
    plot(density(rawDataset$var15))  #distribution of var 15, Normal distribution
    plot(density(rawDataset$num_var4)) # distribution of num_var4 
    plot(density(rawDataset$var38)) # distribution of Mortgae value.
    
    plot(cor(rawDataset, use = "everything"))

# 2. Data pre processing

  #a. Remove id Col
    rawDataset = rawDataset[-1]
    testData = testData[-1]
    
    
    duplicated.columns <- duplicated(t(rawDataset))
    df <- rawDataset[,!duplicated.columns]
    
    
    NZV <-nearZeroVar(df)
    df <- df[,-NZV]
    
    # Logst
    
    lmOut <- glm(TARGET ~.,data = df,family = binomial)

    summary(lmOut)
    
    library(MASS)
    stepAIC(lmOut)
    
    testData<-subset(testData,select=c(var15 , imp_op_var39_ult1 , 
                       ind_var30 , ind_var37_cte , num_var4 , num_var5 , 
                       num_var12_0 , num_var30_0 , num_var39_0 , num_var42_0 , num_var42 , 
                       saldo_var42 , ind_var43_recib_ult1 , 
                       num_var22_hace2 , num_var22_hace3 , num_var22_ult1 , num_med_var45_ult3 , 
                       num_meses_var5_ult3 , num_meses_var39_vig_ult3 , num_var45_hace2 , 
                       num_var45_hace3 , num_var45_ult1 ,  
                       saldo_medio_var5_ult3 , var38))
    finalOutput<-glm(formula = TARGET ~ var15 + 
          imp_op_var39_ult1 + 
          ind_var30 + ind_var37_cte + num_var4 + num_var5 + 
          num_var12_0 + num_var30_0 + num_var39_0 + num_var42_0 + num_var42 + 
           saldo_var42 + ind_var43_recib_ult1 + 
          num_var22_hace2 + num_var22_hace3 + num_var22_ult1 + num_med_var45_ult3 + 
          num_meses_var5_ult3 + num_meses_var39_vig_ult3 + num_var45_hace2 + 
          num_var45_hace3 + num_var45_ult1 +  
          saldo_medio_var5_ult3 + var38, family = binomial, data = df)
    
    summary(finalOutput)
    
    #
    predicted <- predict(finalOutput,testData,type = "response")
    
    pred<- ifelse(predicted > 0.5,1,0)
    pred<- as.data.frame(pred)
    
    testData$TARGET <- pred
    write.csv(pred,"pred.csv")
    
    str(predicted)
    predicted
    library(ROCR)
    prid <- prediction()
    
    # PCA 
    
    prin_comp <-prcomp(df,scale. = T)
    dim(prin_comp$x)
    biplot(prin_comp, scale = 0)
    
    std_dev <- prin_comp$sdev
    #  std_dev_test <- prin_comp_test$sdev
    
    #variance
    pr_var <- std_dev^2
    #pr_var_test <- std_dev_test^2
    
    #proportion of variance explained
    prop_varex <- pr_var/sum(pr_var)
    # prop_var_test_ex <- pr_var_test/sum(pr_var_test)  
    
    #check variance of first 10 components
    prop_varex[1:10]
    
    
    #scree plot
    plot(prop_varex, xlab = "Principal Component of train data set",
         ylab = "Proportion of Variance Explained",
         type = "b")
    
    
    #cumulative scree plot
    plot(cumsum(prop_varex), xlab = "Principal Component of train data set",
         ylab = "Cumulative Proportion of Variance Explained",
         type = "b")
    
    
    df.train <- df[]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  #b. Remove all columns that have same variance
    
    library(caret)
    trainData <- nearZeroVar(rawDataset)
    testData <- nearZeroVar(testData)
    NewData = totalData[, apply(totalData,2,var) !=0]
  
    #NewTestData = testData[,apply(testData,2,var)!=0]
    
    #b =names(NewData)
    
  #  TestData.x = subset(testData, select = )
    
   # names(NewTestData)
  #  NewData.X = subset(NewData, select=-c(TARGET)) 
     
   # setdiff(names(NewData.X), names(NewTestData)) !=  setdiff(names(NewTestData), names(NewData.X))
    
    #names(NewData) != names(NewTestData)
    
  #c. Apply PCA for data set
    
    prin_comp <-prcomp(NewData,scale. = T)
    
    #prin_comp_test <-prcomp(NewTestData,scale. = T)
    
        #names of the variance
        names(prin_comp)
        #names(prin_comp_test)
        
        #output of means
        prin_comp$center
     #   prin_comp_test$center
        
        #rotations
        prin_comp$rotation
      #  prin_comp_test$rotation
        
        #pca X values
        dim(prin_comp$x)
       # dim(prin_comp_test$x)
        #plot the PCA
        
        biplot(prin_comp, scale = 0)
        #biplot(prin_comp_test, scale =0)
        
        #Std deviation
        std_dev <- prin_comp$sdev
      #  std_dev_test <- prin_comp_test$sdev
        
        #variance
        pr_var <- std_dev^2
        #pr_var_test <- std_dev_test^2
        
        #proportion of variance explained
         prop_varex <- pr_var/sum(pr_var)
        # prop_var_test_ex <- pr_var_test/sum(pr_var_test)  
         
         #check variance of first 10 components
         prop_varex[1:10]
         
         prop_var_test_ex[1:20]
         
         #scree plot
         plot(prop_varex, xlab = "Principal Component of train data set",
                ylab = "Proportion of Variance Explained",
                type = "b")
         
         #plot(prop_var_test_ex, xlab = "Principal Component test data set",
          #    ylab = "Proportion of Variance Explained",
           #   type = "b")
         
         #conformation check 
         
         #cumulative scree plot
         plot(cumsum(prop_varex), xlab = "Principal Component of train data set",
                ylab = "Cumulative Proportion of Variance Explained",
                type = "b")
         
         plot(cumsum(prop_var_test_ex), xlab = "Principal Component of test data set",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")
         
         train.data = data.frame(prin_comp$x[1:19805,1:100])
         test.data = data.frame(prin_comp$x[19806:33008,1:100])
         
         train.data = data.frame(TARGET=rawDataset$TARGET,train.data)
         
         #trainSet = cbind(train.data.x,train.data.y)
         
         ##########
         #train.data.x = data.frame(prin_comp$x)
         #train.data.y = data.frame(rawDataset$TARGET)
         
         #t#est.data = data.frame()
        # train.data <- train.data[1:19805,1:100]
     
         
#3.Model building
         
  #a.Run a decision tree
     install.packages("rpart")
     library(rpart)
     rpart.model <- rpart(train.data$TARGET ~ .,data = train.data, method = "anova")
     rpart.model 
     
     
     test.data <- predict(prin_comp, data = test.data)
     test.data <- as.data.frame(test.data)
     predDecisionTree <- predict(rpart.model, test.data)
     
     conf_DT = table()
 
  #b.GLM
     library(MASS)
      
     glmOut <- glm(TARGET~., data=train.data, family="binomial") 
       
       summary(glmOut)
     
       
      test.data = predict(prin_comp, newdata = test.data)
      GLM_Predict <- predict(glmOut,test.data)
      
      GLM_Predict <- as.data.frame(GLM_Predict)
      
      GLM_Predict <- GLM_Predict[,1:100]
  #c. 