rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 11/Activity Sheet")
getwd()


data <- read.csv("Pharmacovigilance_audit_Data.csv",header = T,sep=",")

data <-data[-6]
data$Age <- as.factor(as.character(data$Age))
str(data)

trans <-data

library("arules");

data("data");

Trans = as(data, "transactions");

inspect(Trans)



Rule <- apriori(Trans,parameter = list(sup=0.06,conf=0.6,target="Rule"))

summary(Rule)
inspect(Rule)

Rule.classfilter1 <- as(subset(Rule, subset = rhs %in%
                                  "LocationID=Location2"),"data.frame")

str(Rule.classfilter1)

write.csv(Rule.classfilter1,"Result.csv",row.names = T)
