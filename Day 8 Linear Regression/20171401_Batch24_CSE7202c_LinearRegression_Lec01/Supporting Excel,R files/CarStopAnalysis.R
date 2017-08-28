#

setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 8 Linear Regression/20171401_Batch24_CSE7202c_LinearRegression_Lec01/Supporting Excel,R files")

carstop <- read.csv("SpeedVsStopNADA.csv", header = T, sep = ",")

#Build a linear Model
lmstop <- lm(StopDist.ft~ Speed.mph, data=carstop )

summary(lmstop)

shapiro.test(lmstop$residuals)  # Check if the residuals are Normally distributed
library(ggplot2)
# Try drawing a smoothed line using LOcally Weighted RegrESSion (loess)
# If you have ggplot2 installed, the line below will work
ggplot(carstop,aes(x=Speed.mph, y=StopDist.ft)) + geom_point()+geom_smooth(method="loess")

#or

with(carstop, scatter.smooth(Speed.mph,StopDist.ft,family="gaussian"))




