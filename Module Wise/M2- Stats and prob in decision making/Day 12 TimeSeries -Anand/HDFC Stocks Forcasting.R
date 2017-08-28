rm(list = ls())

#
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 12")

hdfc<- read.csv("hdfc.csv",header = T,sep = ",")

#check for Null value
sapply(hdfc, function(x) sum(is.na(x)))

#
head(hdfc)

#hdfc[order(as.Date(hdfc$day, format ="%m/%d/%y")),]

#data sorting desending 
#hdfc1 <- hdfc[nrow(hdfc):1,]

#Read date into a new variable and extract month day year

install.packages('lubridate')
library('lubridate')

Stock_date <- mdy(hdfc$day)

mth <- month(Stock_date)

yr <- year(Stock_date)

#group data based on month year and price

hdfc2 <- data.frame("Price"=hdfc$Close, yr, mth)
head(hdfc2)

hdfc2 <-aggregate(Price~ mth+yr,hdfc2,mean)


priceTimeSeries <- ts(hdfc2$Price, start = c(2011,1),frequency = 12)

plot(priceTimeSeries)

#Decompose the priceTimeSeries into Trend Randomness, Seasonal, observed values
decompose_priceTimeSeries <- decompose(priceTimeSeries)

par(mfrow = c(1,1))

plot(decompose_priceTimeSeries)

#study ACF PACF

par(mfrow= c(1,2))

acf(priceTimeSeries)

pacf(priceTimeSeries)

#SMA/WMA/EMA

library(TTR)
library(forecast)

par(mfrow=c(1,1))

sma_hdfc <- SMA(priceTimeSeries, n=7)
wma_hdfc <- WMA(priceTimeSeries, n=7)
ema_hdfc <- EMA(priceTimeSeries, n=7)

par(mfrow = c(1,1))

plot(priceTimeSeries, type='l', col='blue')

lines(sma_hdfc, col='black')
lines(wma_hdfc,col='red')
lines(ema_hdfc,col='green')

#Errors on average models

errorSma <- mean(abs(priceTimeSeries[7:56]-sma_hdfc[7:56] ) )

errorWma <- mean(abs(priceTimeSeries[7:56]-wma_hdfc[7:56] ) )

errorEma <- mean(abs(priceTimeSeries[7:56]-ema_hdfc[7:56] ) )


errorSma

errorWma

errorEma

# Holt-Winters model

#Train data to build model

priceTimeSeries = ts(hdfc2$Price, start = c(2011,1), frequency = 12)

price_HW = HoltWinters(priceTimeSeries, gamma = FALSE)

# forecast for last 5 

library(forecast)
price_hw_forecasts <- forecast(price_HW, h=4)

hw_preds <- data.frame(price_hw_forecasts)$Point.Forecast

actuals <- hdfc2$Price[53:56]

#compute the error

mean(abs(hw_preds-actuals))

library(DMwR)
regr.eval(trues = actuals,preds = hw_preds)


#Applying ARIMA

#Method 1
par(mfrow=c(1,4))
plot.ts(priceTimeSeries, main="Actual Data")

timeSeriesDiff1 <- diff(priceTimeSeries,differences = 1)
plot.ts(timeSeriesDiff1,main="Data with one Difference")

timeSeriesDiff2 <- diff(priceTimeSeries,differences = 2)
plot.ts(timeSeriesDiff1,main="Data with two Difference")

timeSeriesDiff3 <- diff(priceTimeSeries,differences = 3)
plot.ts(timeSeriesDiff1,main="Data with three Difference")

# Method 2 for ARIMA

ndiffs(priceTimeSeries)

price_stationary_ts <- diff(priceTimeSeries,differences = 1)
par(mfrow=c(1,1))
plot.ts(price_stationary_ts)

#inspect ACF PACF

par(mfrow=c(1,2))

acf(price_stationary_ts)

pacf(price_stationary_ts)

#From the PACF plot of differenced data, we infer that p = 0  
#From the ACF plot of differenced data, we infer that q = 0  
#Number of diffs we used to difference d=1  
#There fore our ARIMA model is: ARIMA(0,1,0)  

#Build ARIMA Model

price_ARIMA <- arima(priceTimeSeries,order = c(0,1,0))

price_arima_forecasts <- forecast.Arima(price_ARIMA, h=4)

preds <- data.frame(price_arima_forecasts)$Point.Forecast

actuals <- hdfc2$Price[53:56]

#Check Error

library(DMwR)
regr.eval(trues = actuals,preds = preds)

#Compare the Holt-Winters and ARIMA Model which ever perfrom better choose that
regr.eval(trues = actuals,preds = hw_preds)


