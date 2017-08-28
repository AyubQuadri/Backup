setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\RegressionTimeSeries\\2017-01-14\\Day4")


aa <- read.csv("SeasonalSales.csv")
ggplot(aa)+geom_point(aes(x=Quarter,y=Sales),size=2)+ geom_line(aes(x=Quarter,y=Sales),linetype=2) + labs(y="Sales( in $1000s)", x="Number of Quarters")

#First Attempt: Linear fit
saleslm <- lm(Sales ~Quarter , data=aa)
ggplot(aa)+geom_point(aes(x=Quarter,y=Sales),size=2)+ geom_line(aes(x=Quarter,y=Sales),linetype=2) +geom_line(aes(x=Quarter,y=saleslm$fitted.values),col="red")

#Quadratic fit
saleslm2 <- lm(Sales~ poly(Quarter,2),data=aa)
ggplot(aa)+geom_point(aes(x=Quarter,y=Sales),size=2)+ geom_line(aes(x=Quarter,y=Sales),linetype=2) +geom_line(aes(x=Quarter,y=saleslm2$fitted.values),col="red") + labs(y="Sales( in $1000s)", x="Number of Quarters")


Qtr <- (aa$Quarter-1)%%4 + 1 
aa$Qtr <- as.factor(Qtr)  #Add the specific quarter in the year as a factor.

#Quadratic fit with Seasonality built-in
saleslmQ <- lm(Sales~ poly(Quarter,2) + Qtr,data=aa)
ggplot(aa)+geom_point(aes(x=Quarter,y=Sales),size=2)+ geom_line(aes(x=Quarter,y=Sales),linetype=2) +geom_line(aes(x=Quarter,y=saleslmQ$fitted.values),col="red") + labs(y="Sales( in $1000s)", x="Number of Quarters")

#Lets try to understand how the quadratic fit is fitting the seasonality so well
qt1 <- aa$Qtr==1
qt2 <- aa$Qtr==2
qt3 <- aa$Qtr==3
qt4 <- aa$Qtr==4

p <- ggplot()+geom_point(data=aa, aes(x=Quarter,y=Sales),size=2)+ geom_line(data=aa, aes(x=Quarter,y=Sales),linetype=2) + labs(y="Sales( in $1000s)", x="Number of Quarters")
p +geom_line(aes(x=aa$Quarter[qt1],y=saleslmQ$fitted.values[qt1]),col="red") +geom_line(aes(x=aa$Quarter[qt2],y=saleslmQ$fitted.values[qt2]),col="pink")+geom_line(aes(x=aa$Quarter[qt3],y=saleslmQ$fitted.values[qt3]),col="green")+geom_line(aes(x=aa$Quarter[qt4],y=saleslmQ$fitted.values[qt4]),col="blue")

#The above plot shows, what adding the factor Qtr does. It allows R to fit, separate quadratic fit to each quarter.


#Now lets use Holt-Winters to fit the data
SalesTs <- ts(aa$Sales,frequency = 4)
HWSalesAdd <- HoltWinters(SalesTs)
plot(HWSalesAdd)
print(paste("SSE=",HWSalesAdd$SSE))
ggplot()+geom_line(aes(x=aa$Quarter,y=aa$Sales)) + geom_line(aes(x=aa$Quarter[-(1:4)],y=HWSalesAdd$fitted[,1]),col="red") + labs(y="Sales( in $1000s)", x="Number of Quarters")
  
  
HWSalesMul <- HoltWinters(SalesTs,seasonal = "multiplicative")
plot(HWSalesMul)
print(paste("SSE=",HWSalesMul$SSE))
