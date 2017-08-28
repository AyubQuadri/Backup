rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 11/Activity Sheet")
getwd()


data <- read.csv("FlightDelays.csv",header = T,sep=",")

summary(data)
str(data)



data$Weather <- as.factor(as.character(data$Weather))
data$DAY_WEEK <- as.factor(as.character(data$DAY_WEEK))
data$Flight.Status <- as.factor(as.character(data$Flight.Status))

str(data)
head(data)

#Bin data 

data$CRS_DEP_TIME <- cut(data$CRS_DEP_TIME, c(0,600,1200,1800,2400), right=FALSE, labels=c(0:3))

data$CRS_DEP_TIME




library("arules");

data("data");

Trans = as(data, "transactions");

inspect(Trans)



Rule <- apriori(Trans,parameter = list(sup=0.5,conf=0.6,target="Rule"))

summary(Rule)
inspect(Rule)

Rule.classfilter1 <- as(subset(Rule, subset = rhs %in%
                                 "Flight.Status=0"),"data.frame")

Rule.classfilter1
str(Rule.classfilter1)

write.csv(Rule.classfilter1,"FlightResult.csv",row.names = T)
