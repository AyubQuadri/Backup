# Remove all the variables from workspace
rm(list = ls())

#Predict flower species(classify)
iris
head(iris)
dim(iris)
names(iris) 
str(iris)
table(iris$Species)

#split train and test
set.seed(500)
# s = sample(150,105)
# iris_train = iris[s,]
# iris_test = iris[-s,]
# table(iris_train$Species)
library(caTools)
splitRec <- sample.split(iris$Species,SplitRatio = 0.7)
iris_train <- iris[splitRec,]
iris_test <- iris[!splitRec,]
dim(iris_train)
dim(iris_test)
#################
#C5.0 good classification technique but does not 
#provide good plots. For that rpart is better.
library(C50)
Model_C50 <- C5.0(iris_train[,-5],iris_train[,5],rules = TRUE)
Model_C50
summary(Model_C50)
table(iris_train$Species)

#Predicting on Train (already shown in summary)
P1_train=predict(Model_C50,iris_train);P1_train
table(iris_train[,5],Predicted=P1_train)

#Predicting on Test
P1_test = predict(Model_C50,iris_test);P1_test
table(iris_test[,5],Predicted=P1_test)

#################
#rpart
library(rpart)
library(rpart.plot)

Model_rpart= rpart(Species~.,data=iris_train, method="class")
Model_rpart
plot(Model_rpart)
rpart.plot(Model_rpart,type=3,extra=103,fallen.leaves = FALSE)
#percentages are based on predictions(i.e. 36/105,32/105,37/105)
#extras=101:shows classifications/misclassifications for each class  
#extras=102:shows correct classification/total prediction for each class
#extras=102:shows misclassification/total prediction for each class

table(iris_train$Species)

#Predicting on Train
P1_train_rpart=predict(Model_rpart,iris_train,type="class")
table(iris_train[,5],predicted=P1_train_rpart)

#Predicting on Test
P1_test_rpart=predict(Model_rpart,iris_test,type="class")
table(iris_test[,5],predicted=P1_test_rpart)

