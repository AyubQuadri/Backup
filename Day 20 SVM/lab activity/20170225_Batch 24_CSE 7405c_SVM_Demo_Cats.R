library(MASS)
#?cats
head(cats)

#Standardize the numeric variables
library(vegan) 
cats1 <- decostand(cats[,-1],"range")

#Add target variable "Sex" to the standardized dataset
cats1['Sex'] <- cats$Sex

# Re-arranging the variables
cats1<-subset(cats1,select=c(Sex,Bwt,Hwt))

library(e1071)
#Build SVM Model
model <- svm(Sex~., data = cats1, kernel = "linear",cost = 10)
print(model)
summary(model)

compareTable <- table(predict(model),cats1$Sex);compareTable  

model_kernel <- svm(Sex~., data = cats1, kernel = "radial",cost = 10)
print(model_kernel)
summary(model_kernel)

compareTable <- table(predict(model_kernel),cats1$Sex);compareTable 


# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(cats1) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- cats1[trainingRows, ] # training data
testData <- cats1[-trainingRows, ] # test data

#Tuning
tuned <- tune.svm(Sex ~., data = trainingData, gamma = 10^(-3:1), cost = 2^(0:9)) # tune

summary(tuned) # to select best gamma and cost

svmfit <- svm (Sex ~ ., data = trainingData, kernel = "radial",
               cost = 100, gamma=0.01, probability = TRUE) # radial svm, scaling turned OFF
print(svmfit)
probvalues<- predict(svmfit, testData, probability = TRUE)

compareTable <- table(testData$Sex, predict(svmfit, testData))  # comparison table


# prediction <- predict(svmfit, testData[,-3])
# tab1 <- table(pred = prediction, true = testData[,3]);tab1
w = t(svmfit$coefs) %*% svmfit$SV


