#

setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\RegressionTimeSeries\\2017-01-14\\Day1")

wordRecall <- read.csv("wordRecall.csv")

plot(wordRecall)

#Plot the transformed variables
plot(log(wordRecall$time),wordRecall$prop)

#Fit a linear model

lmRecall <- lm(prop~log(time),wordRecall)
summary(lmRecall)

