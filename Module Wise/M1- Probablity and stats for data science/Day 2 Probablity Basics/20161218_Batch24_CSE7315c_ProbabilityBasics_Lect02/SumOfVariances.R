
# Lets start by creating two large datasets
data1  <- rnorm(50000,sd=1)  #Random data with Standard deviation 1
data2 <- rnorm(50000,sd=2)   #Random data with Standard deviation 2

#Lets compute variance
var(data1)  
var(data2)

hist(data1,200)

data3 <- data1+data2

#Lets compute standard deviation of the new dataset
var3 <- var(data3)
hist(data3,200)

x11()
data4 <- data1 - data2
var4 <- var(data4)
hist(data4,200)

