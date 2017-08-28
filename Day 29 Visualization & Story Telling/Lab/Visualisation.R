rm(list=ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 29 Visualization & Story Telling/Lab")

#####Read the data
data <- read.csv("EconomistData.csv",header = T,sep = ",")
attach(data)

########## Scatter plot with CPI on x-axis and HDI on y-axis
library(ggplot2)

ggplot(data=data,aes(x=CPI, y=HDI))+
  geom_point()+theme_bw()+xlab("CPI")+ylab("HDI")

############## add Region to this above plot ####

ggplot(data=data,
       aes(x=CPI,
           y=HDI, 
           color=as.factor(Region)))+
  geom_point()+theme_bw()+xlab("CPI")+ylab("HDI")

############ Add a tend line ###########

ggplot(data=data,
       aes(x=CPI, 
           y=HDI ,
           color = Region))+
  geom_point()+
  geom_smooth(aes(group=1))+
  theme_bw()+

  xlab("Corruption Perception Index")+ylab("Human development Index")
  
############# Add Country label ###########
ggplot(data=data,
       aes(x=CPI, 
           y=HDI ,
           color = Region))+
  geom_point()+
  geom_smooth(aes(group=1))+
  theme_bw()+
  
  xlab("Corruption Perception Index")+ylab("Human development Index")
  
  
  