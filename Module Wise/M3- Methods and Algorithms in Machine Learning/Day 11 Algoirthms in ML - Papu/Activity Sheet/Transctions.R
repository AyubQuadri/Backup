rm(list = ls())
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 11/Activity Sheet")
getwd()

install.packages('arules')
library('arules')
trans <- read.transactions(file = "Transactions.csv", rm.duplicates = F,format = "single",sep = ",",cols = c(1,2))
inspect(trans)
trans

Rule <- apriori(trans,parameter = list(sup=0.5,conf=0.6,target="Rule"))

summary(Rule)
inspect(Rule)
