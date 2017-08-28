rm(list = ls())
setwd("D:/AMITS_LABS/BATCH 24/Day16_NN/20170212_Batch 24_CSE 7405c_ANN_LabActivity")

# Load train and test datasets
train <- read.csv("train_sample.csv")
test <- read.csv("test_sample.csv") 

# Convert to matrix
train_mat<-data.matrix(train)

# ## Color ramp def
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

## Plot the first 12 images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
sm = sample(nrow(train_mat), 12)
for(di in sm)
{
  print(di)

  z <- array(train_mat[di,-1],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=train_mat[di,1],col=cus_col(256))
}

# dataset into train and test
train<-data.matrix(train)
test<-data.matrix(test)

train.x<-train[,-1]
train.y<-train[,1]
train.x<-(train.x/255)
table(train.y)

test.x<-test[,-1]
test.y<-test[,1]
test.x<-(test.x/255)
table(test.y)
library(mxnet)
#Build model
model1 <-mx.mlp(train.x, train.y, hidden_node =c(128,64), out_node=10, dropout = NULL,
               activation = "relu", out_activation = "softmax",
               num.round=10, 
               array.batch.size=100,
               learning.rate=0.07, 
               momentum=0.9,  
               eval.metric=mx.metric.accuracy)

# Predict on test
preds <- predict(model1, test.x)
dim(preds)

pred.label <- max.col(t(preds)) - 1 
pred.label
head(pred.label)
table(test.y,pred.label)
sum(diag(table(test.y,pred.label)))/1000
