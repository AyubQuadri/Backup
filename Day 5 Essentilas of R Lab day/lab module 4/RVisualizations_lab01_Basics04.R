rm(list=ls(all=TRUE))
setwd()

#######Creating dummy variables and adding to original table#############

datamerged <- read.csv(file="dataMerged.csv",header=TRUE, sep=",")

str(datamerged)
head(datamerged)
summary(datamerged)

datamerged<-na.omit(datamerged)

datamerged$family <- as.factor(datamerged$family)
datamerged$edu <- as.factor(datamerged$edu)
datamerged$cc <- as.factor(datamerged$cc)
datamerged$cd <- as.factor(datamerged$cd)
datamerged$securities <- as.factor(datamerged$securities)
datamerged$online <- as.factor(datamerged$online)
datamerged$loan <- as.factor(datamerged$loan)

install.packages("dummies")
library(dummies)
EduDummyVars<-dummy(datamerged$edu)
head(EduDummyVars)
datamerged<-data.frame(datamerged,EduDummyVars)
head(datamerged)


####################################Visualizations#################################################

data <- datamerged
attach(data)
names(data)

#Histogram
hist(inc, col="green", xlab="Income",main="Histogram of Income")

par(mfrow=c(1,2))
hist(age, col="green", xlab="Age",main="Histogram of Age")
hist(exp, col="blue", xlab="Experience",main="Histogram of Experience")

#BoxPlot
par(mfrow=c(1,2))
boxplot(inc, main="Income distribution")
abline(h = mean(inc), lty=2)

boxplot(inc~edu, main="Variation of income as a function of education", 
        xlab="Education", ylab="Income")
abline(h = mean(inc))

#BarPlot
par(mfrow=c(1,1))
barplot(table(data$loan), main="Number of customer in each class")

detach(data)


#######Visualisations using ggplot###

library(ggplot2)
attach(mtcars)
###Group bar charts
c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c+geom_bar(width=0.5)
c + geom_bar() +coord_flip()
#c + scale_fill_brewer()
c+geom_bar(fill="green", colour="red", width=0.5)
c + geom_bar(aes(fill=factor(gear)), position="dodge") 
c + geom_bar(aes(fill=factor(gear)), position="dodge") + facet_grid(~vs)

##Stacked bar plots
qplot(factor(cyl), data=mtcars, geom="bar")
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))


##Histograms 
hist(mpg, breaks = c(10, 12, 14,18, 20, 22, 25, 28, 30,35 ), main ="distribution of mpg")
#using ggplot
m <- ggplot(mtcars, aes(x=mpg))
m + geom_histogram(bins=10)
m + geom_histogram(bins=10,aes(fill=..count..)) ##adjusting the value of binwidth

###Boxplots
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter()
p + geom_boxplot(outlier.colour = "green", outlier.size = 3) + geom_jitter()
p + geom_boxplot(aes(fill=cyl))
p + geom_boxplot(aes(fill=factor(cyl)))


###Scatterplot in R
plot(mtcars$mpg,mtcars$disp)
qplot(mtcars$mpg,mtcars$disp,data=mtcars,colour=as.factor(mtcars$cyl),geom=c("point","line"))
qplot(mtcars$mpg,mtcars$disp,data=mtcars,colour=as.factor(mtcars$gear),geom=c("point","line"), facets = ~cyl)

#install.packages("car")
library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data=mtcars, main="Scatter Plot Matrix")
scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars, main="Three Cylinder Options")


#connect points with line, #add regression line
p1 <- ggplot(mtcars, aes(x = hp, y = mpg))
p1 + geom_point(color="blue")
p1 + geom_point(color="blue") + geom_line()                           
p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)



