### Author: Shah Ayub Quadri ####
### Date:   5th Feb 2016     ###

rm(list = ls())

setwd()

#a. read data
  LabourSurvey <- read.csv("LabourForceSurveyData.csv", header = T, sep=",")

  #check for null values 
    sapply(LabourSurvey, function(x)sum(is.na(x)))
    # no null values so need not to impute

#b.Time series 

  TimeSeries <- ts(LabourSurvey$TotalAllRegionalCouncilsTotalLabourForce ,start = c(1986,1),frequency = 4)
  # time series data from 1986 Quarter wise 
  TimeSeries
  plot(TimeSeries)
  
#c. Decompose to check the Seasonality, ramdomness, Trend
  
    decmoposedTimeSeries <- decompose(TimeSeries)
    par(mfrow=c(1,1))
    plot(decmoposedTimeSeries)
    
#d.Study ACF and PACF
    
    par(mfrow =c (1,2))
    acf(TimeSeries)
    pacf(TimeSeries)
    
    
    #SMA,WMA,EMA
    
    library(TTR)
    library(forecast)
    
    Sma_Labour <-SMA(TimeSeries, n=5)
    Wma_Labour <-WMA(TimeSeries,n =5)
    Ema_Labour <-EMA(TimeSeries,n =5)
    
    par(mfrow = c(1,1))
    
    plot(TimeSeries, type='l', col='blue')
    
    lines(Sma_Labour, col='black')
    lines(Wma_Labour,col='red')
    lines(Ema_Labour,col='green')
    
    #Error Terms
    
    ErrorSMA<- mean(abs(TimeSeries[5:104] - Sma_Labour[5:104]))
    ErrorWMA<- mean(abs(TimeSeries[5:104] - Wma_Labour[5:104]))
    ErrorEMA<- mean(abs(TimeSeries[5:104] - Ema_Labour[5:104]))
    
    ErrorSMA # Error is highest with SMA 17.3
    ErrorWMA #Error has significantly low value in weighted Moving Average 12.59
    ErrorEMA # Error with EMA 16.46

#e. Different Models Tests
    
  #1.Manual ARIMA Model diff value 
  
      # find how many difference needed for Manual ARIMA Model
      
      ndiffs(TimeSeries)
      LabourStationary_ts <- diff(TimeSeries,differences = 2)
      par(mfrow=c(1,1))
      plot.ts(LabourStationary_ts)
      
      #now inspect ACF and PACF
      
      par(mfrow=c(1,2))
      acf(LabourStationary_ts)
      pacf(LabourStationary_ts)
      
      
      #From the PACF plot of differenced data, we infer that p = 2  
      
      #Number of diffs we used to difference d=2 
      
      #From the ACF plot of differenced data, we infer that q = 1  
    
      #There fore our ARIMA model is: ARIMA(p,d,q) ARIMA(2,2,1)
      
      #ARIMA Model
      
      price_ARIMA <- arima(TimeSeries,order = c(2,2,1))
      
      Del_arima_forecasts <- forecast.Arima(price_ARIMA, h=5)
      
      preds <- data.frame(Del_arima_forecasts)$Point.Forecast
      
      actuals <- LabourSurvey$TotalAllRegionalCouncilsTotalLabourForce[100:104]
      
  #2.HoltWinters Model
      
      HW_TimeSeries = ts(LabourSurvey$TotalAllRegionalCouncilsTotalLabourForce, start = c(1986,1), frequency = 4)
      
      Labour_HW = HoltWinters(HW_TimeSeries)
      
      # forecast for last 5 based on Holt-Windters Model
      
      library(forecast)
      Labour_HW_forecasts <- forecast(Labour_HW, h=5)
      hw_preds <- data.frame(Labour_HW_forecasts)$Point.Forecast
      actuals <- LabourSurvey$TotalAllRegionalCouncilsTotalLabourForce[100:104]
      

  #3.Auto Arima model
  
      autoArimaModel <- auto.arima(TimeSeries) 
      # Forecasting for the last 4 months using auto arima model
      autoarima_forecast <- data.frame(forecast(autoArimaModel,h=5))$Point.Forecast
      actuals <- LabourSurvey$TotalAllRegionalCouncilsTotalLabourForce[100:104]
     
      

#g.Check Error and select best model
  
  library(DMwR)
  
  
  #Compare the Holt-Winters and ARIMA Model which ever perfrom better choose that
  
  #Holt-Winter
  regr.eval(trues = actuals,preds = hw_preds)
  
  #Manual ARIMA
  regr.eval(trues = actuals,preds = preds)
  
  #Auto ARIMA
  regr.eval(trues = actuals, preds = autoarima_forecast)  
  
  
  
  #mape from least to highest
  # Holt-Winter < Auto Arima < Manual Arima(p,d,q)
  # 1.626765e-02 < 1.719076e-02 < 1.786208e-02 
  
  #Holt-Winter ARIMA seems to be better as it has less RMSE & mape value comparativily 
  #so better to opt for Holt-Winter Model
  