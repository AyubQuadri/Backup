rm(list = ls())

#read data
birth<- read.csv("nybirths.dat",header = F,sep = "\t")

#check of null value
sum(is.na(birth))

plot.ts(birth)

#Time Series model

TimeSeries <- ts(birth ,start = c(2013,1),frequency = 12)
TimeSeries
plot(TimeSeries)

#Decompose to check the Seasonality, ramdomness, Trend

decmoposedTimeSeries <- decompose(TimeSeries)

par(mfrow=c(1,1))

plot(decmoposedTimeSeries)

# Study ACF and PACF

par(mfrow =c (1,2))

acf(TimeSeries)
pacf(TimeSeries)

#SMA,WMA,EMA

library(TTR)
library(forecast)

Sma_Delivery <-SMA(TimeSeries, n=5)
Wma_Delivery <-WMA(TimeSeries,n =5)
Ema_Delivery <-EMA(TimeSeries,n =5)

par(mfrow = c(1,1))

plot(TimeSeries, type='l', col='blue')

lines(Sma_Delivery, col='black')
lines(Wma_Delivery,col='red')
lines(Ema_Delivery,col='green')

#Error Terms


ErrorSMA<- mean(abs(TimeSeries[5:168] - Sma_Delivery[5:168]))
ErrorWMA<- mean(abs(TimeSeries[5:168] - Wma_Delivery[5:168]))
ErrorEMA<- mean(abs(TimeSeries[5:168] - Ema_Delivery[5:168]))

ErrorSMA
ErrorWMA
ErrorEMA

# Holt-Winters model

DelTimeSeries = ts(birth, start = c(2013,1), frequency = 12)
Delivery_HW = HoltWinters(DelTimeSeries)
Delivery_HW = HoltWinters(DelTimeSeries, gamma = FALSE)

# forecast for last 5 based on Holt-Windters Model

library(forecast)
Delivery_hw_forecasts <- forecast(Delivery_HW, h=5)
hw_preds <- data.frame(Delivery_hw_forecasts)$Point.Forecast
actuals <- birth$V1[164:168]

#ARIMA Model diff value 

# find how many difference needed for Manual ARIMA Model

ndiffs(TimeSeries)
DelStationary_ts <- diff(TimeSeries,differences = 1)


par(mfrow=c(1,1))
plot.ts(DelStationary_ts)

#now inspect ACF and PACF

par(mfrow=c(1,2))
acf(DelStationary_ts)
pacf(DelStationary_ts)


#From the PACF plot of differenced data, we infer that p = 0  

#From the ACF plot of differenced data, we infer that q = 1  

#Number of diffs we used to difference d=1  

#There fore our ARIMA model is: ARIMA(1,1,0)


#ARIMA Model

price_ARIMA <- arima(TimeSeries),order = c(2,1,2))

Del_arima_forecasts <- forecast.Arima(price_ARIMA, h=4)

preds <- data.frame(Del_arima_forecasts)$Point.Forecast

actuals <- birth$V1[164:168]

#AUTO ARIMA
library(DMwR)
autoArimaModel <- auto.arima(TimeSeries) 
# Forecasting for the last 4 months using auto arima model
autoarima_forecast <- data.frame(forecast(autoArimaModel,h=5))$Point.Forecast
regr.eval(trues = actuals, preds = autoarima_forecast)  




#Check Error

library(DMwR)


#Compare the Holt-Winters and ARIMA Model which ever perfrom better choose that

#Holt-Winter
regr.eval(trues = actuals,preds = hw_preds)

#Manual ARIMA
regr.eval(trues = actuals,preds = preds)

#Auto ARIMA
regr.eval(trues = actuals, preds = autoarima_forecast)  

#Auto ARIMA seems to be better as it has less RMSE value comparativily
