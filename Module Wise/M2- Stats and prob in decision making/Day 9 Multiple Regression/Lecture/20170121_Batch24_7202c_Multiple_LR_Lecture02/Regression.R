setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\RegressionTimeSeries\\2017-01-14\\Day2")

# MULTIPLE LINEAR REGRESSION MODEL BUILDING

# Read in World Crude Oil Output data
CrudeOilOutput <- read.csv("CrudeOilOutput.csv", header = T, sep = ",")
CrudeOilOutput

# Check for correlations
correlation <- cor(CrudeOilOutput)
correlation
plot(CrudeOilOutput)

CrudeOilOutputlm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
                       + CrudeOilOutput$USAutoFuelRate
                       + CrudeOilOutput$USNuclear + CrudeOilOutput$USCoal
                       + CrudeOilOutput$USDryGas, CrudeOilOutput)
summary(CrudeOilOutputlm)
par(mfrow=c(2,2))
plot(CrudeOilOutputlm)

# Load required libraries
library(MASS)
library(car)

# Use stepAIC to build model based on AIC
stepAIC(CrudeOilOutputlm, direction = "both")



 # LOGISTIC REGRESSION

# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)

flierresponseglm <- glm(Response~1, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)

#-------Term Deposit Example -----

subscribetermdeposit <- read.csv("bank-full.csv", header = T, sep = ";")
subscribetermdeposit
str(subscribetermdeposit)
subscribetermdepositglm <- glm(y ~ age + job + marital + education + default
                               + balance + housing + loan + contact + day
                               + month + duration + campaign + pdays
                               + previous + poutcome, data = subscribetermdeposit,
                               family = "binomial")
subscribetermdepositglm
summary(subscribetermdepositglm)

stepAIC(subscribetermdepositglm, direction = "both")
car::vif(subscribetermdepositglm)

# CASE STUDY - The Framingham Heart Study

# Read in the Framingham dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

#Accuracy on the training set
predictTrain = predict(framinghamLog, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)

# Accuracy on Train Set
(2170+30)/(2170+30+357+9)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy on Test Set
(915+12)/(915+12+158+7)

# Confusion matrix with threshold of 0.9
table(test$TenYearCHD, predictTest > 0.9)
# Confusion matrix with threshold of 0.7
table(test$TenYearCHD, predictTest > 0.7)
# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
# Confusion matrix with threshold of 0.3
table(test$TenYearCHD, predictTest > 0.3)
# Confusion matrix with threshold of 0.1
table(test$TenYearCHD, predictTest > 0.1)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# # RECODE AGE AND REBUILD MODEL
# 
# # Read in the Framingham dataset
# framingham = read.csv("framingham.csv")
# 
# # Look at structure
# str(framingham)
# 
# # Recode Age
# lowRisk <- which(framingham$age < 40)
# medRisk <- which(framingham$age >= 40 & framingham$age < 50)
# highRisk <- which(framingham$age >= 50)
# framingham[lowRisk,"age"] <- "1"
# framingham[medRisk,"age"] <- "2"
# framingham[highRisk, "age"] <- "3"
# framingham$age <- as.factor(framingham$age)
#   
# # Load the library caTools
# library(caTools)
# 
# # Randomly split the data into training and testing sets
# set.seed(1000)
# split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)
# 
# # Split up the data using subset
# train = subset(framingham, split==TRUE)
# test = subset(framingham, split==FALSE)
# 
# # Logistic Regression Model
# framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
# summary(framinghamLog)
# 
# #Accuracy on the training set
# predictTrain = predict(framinghamLog, type="response", newdata=train)
# 
# # Confusion matrix with threshold of 0.5
# table(train$TenYearCHD, predictTrain > 0.5)
# 
# # Accuracy
# (2170+30)/(2170+30+357+9)
# 
# # Predictions on the test set
# predictTest = predict(framinghamLog, type="response", newdata=test)
# 
# # Confusion matrix with threshold of 0.5
# table(test$TenYearCHD, predictTest > 0.5)
# 
# # Accuracy
# (915+12)/(915+12+158+7)
# 
# # Test set AUC 
# library(ROCR)
# ROCRpred = prediction(predictTest, test$TenYearCHD)
# as.numeric(performance(ROCRpred, "auc")@y.values)
