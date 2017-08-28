rm(list=ls(all=TRUE))
setwd("C:\\Users\\Classroom2\\Desktop\\Batch24\\TimeSeries-Day4")

#TIME SERIES FORECASTING

library("forecast")
library("stats")

#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).

# Read in the US Air Carrier Traffic - Revenue Passenger Miles dataset
#   Unit: Thousand Miles
#   Data Source: http://www.bts.gov/xml/air_traffic/src/index.xml
#   and https://datamarket.com/data/set/281x/us-air-carrier-traffic-statistics-revenue-passenger-miles 

miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles

milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries

plot(milestimeseries)

#a data set of the number of births per 
#month in New York city, 
#from January 1946 to December 1959

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births

birthstimeseries <- ts(births, 
                       frequency=12, 
                       start=c(1946,1))
birthstimeseries

plot(birthstimeseries)

#GE stock price over 910 days
GEStock = read.csv("GEstock-3year.csv")
GEStock

getimeseries <- ts(GEStock)
getimeseries

plot(getimeseries)

#Simulated data to understand ACF & PACF

par(mfrow=c(1,1))
time <- c(1:100)
growth <- time
plot(growth~time)

par(mfrow=c(1,2))

growth <- ts(growth)
acf(growth)
pacf(growth)

par(mfrow=c(1,1))
time <- ts(c(1:100))
growth <- sin(0.25*pi*time)
plot(growth~time, type="l")

par(mfrow=c(1,2))

#growth <- ts(sin(growth))
acf(growth)
pacf(growth)

par(mfrow=c(1,1))
time <- c(1:100)
growth <- runif(100, min=0, max=1)
plot(growth~time, type="l")

par(mfrow=c(1,2))

growth <- ts(runif(growth))
acf(growth)
pacf(growth)

#Decomposition

milestimeseriescomponents <- 
  decompose(milestimeseries)

plot(milestimeseriescomponents)
milestimeseriescomponents$seasonal
milestimeseriescomponents$trend

milestimeseriesSeasonally <- 
  milestimeseries - milestimeseriescomponents$seasonal
milestimeseriesSeasonally

birthstimeseriescomponents <- 
  decompose(birthstimeseries)

plot(birthstimeseriescomponents)
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$trend

birthstimeseriesSeasonally <- 
  birthstimeseries - birthstimeseriescomponents$seasonal
birthstimeseriesSeasonally

#ACF and PACF of real world data

par(mfrow=c(1,3))
plot.ts(milestimeseries)
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)
#acf(milestimeseries, lag.max=20, ci.type="ma")

plot(birthstimeseries)
acf(birthstimeseries, lag.max=20)
pacf(birthstimeseries, lag.max=20)
#acf(birthstimeseries, lag.max=20, ci.type="ma")

# Differencing and ACF, PACF on
# Stationary and Non-Stationary Data

par(mfrow=c(1,2))
acf(getimeseries, lag.max=20)
pacf(getimeseries, lag.max=20)

getimeseriesdiff1 <- diff(getimeseries, differences=1)
getimeseriesdiff1

acf(getimeseriesdiff1, lag.max=20)
pacf(getimeseriesdiff1, lag.max=20)

#ndiffs(getimeseries)
#ndiffs(milestimeseries)

#nsdiffs(milestimeseries)

#ma <- birthstimeseriescomponents$random
#trend <- birthstimeseriescomponents$trend
#seas <- birthstimeseriescomponents$seasonal

#par(mfrow=c(2,2))
#plot(birthstimeseries)
#plot(trend)
#plot(seas)
#plot(ma)

#par(mfrow=c(3,2))
#acf(ma, na.action=na.pass)
#pacf(ma, lag.max=20, na.action=na.pass)
#acf(trend, na.action=na.pass)
#pacf(trend, lag.max=20, na.action=na.pass)
#acf(seas, na.action=na.pass)
#pacf(seas, lag.max=20, na.action=na.pass)

#Regression on time

par(mfrow=c(1,1))
births <- data.frame(births)
births$time <- seq(1:168)
edit(births)
plot(births$births, type="l")
lm1 <- lm(births$births ~ births$time)
lm2 <- lm(births$births ~ 
            poly(births$time, 2, raw=TRUE))
lm3 <- lm(births$births ~ 
            poly(births$time, 3, raw=TRUE))

points(births$time, predict(lm1), 
       type="l", col="red", lwd=2)
points(births$time, predict(lm2), 
       type="l", col="green", lwd=2)
points(births$time, predict(lm3), 
       type="l", col="blue", lwd=2)

births$seasonal <- as.factor(rep(c(1:12),14))
View(births)

lm1s <- lm(births ~ ., data=births)
lm2s <- lm(births ~ poly(time, 2, raw=TRUE)+
            seasonal, data=births)
lm3s <- lm(births ~ poly(time, 3, raw=TRUE)+
             seasonal, data=births)

plot(births$births, type="l")
points(births$time, predict(lm1s), 
       type="l", col="red", lwd=2)
points(births$time, predict(lm2s), 
       type="l", col="blue", lwd=2)

plot(births$births, type="l")
points(births$time, predict(lm3s), 
       type="l", col="green", lwd=2)

#Another crude approach

births$mae <- births$births/predict(lm1)
##births$month <- rep(seq(1:12),14)
View(births)
head(births)

seasonal <- tapply(births$mae, 
                   births$seasonal, mean)
seasonal

birthspr <- predict(lm1)*rep(seasonal,14)

plot(births$births, type="l")
points(births$time, birthspr, 
       type="l", col="red", lwd=2)

births$mae <- births$births-predict(lm1)
edit(births)
head(births)

seasonalAdd <- tapply(births$mae, 
                   births$seasonal, mean)
seasonalAdd

birthspr <- predict(lm1)+rep(seasonalAdd,14)

plot(births$births, type="l")
points(births$time, birthspr, 
       type="l", col="green", lwd=2)


#Moving averages

library(TTR)

par(mfrow=c(1,1))
milestimeseries
plot(milestimeseries)

smamiles <- SMA(milestimeseries, n=2)
smamiles

wmamiles <- WMA(milestimeseries, n=2)
wmamiles

emamiles <- EMA(milestimeseries, n=2)
emamiles

par(mfrow=c(1,1))
plot(milestimeseries, type="l", col="red")
lines(smamiles, col="black", lwd=2)
lines(wmamiles, col="blue")
lines(emamiles, col="brown")

MAPESMA <- mean(abs(milestimeseries[2:200]-smamiles[2:200])/abs(milestimeseries[2:200]))*100
MAPEWMA <- mean(abs(milestimeseries[2:200]-wmamiles[2:200])/abs(milestimeseries[2:200]))*100
MAPEEMA <- mean(abs(milestimeseries[2:200]-emamiles[2:200])/abs(milestimeseries[2:200]))*100

MAPESMA
MAPEWMA
MAPEEMA

#Effect of K

milestimeseriesSMA3 <- 
  SMA(milestimeseries,n=3)

milestimeseriesSMA8 <- SMA(milestimeseries,
                           n=8)

par(mfrow = c(1, 2))
plot.ts(milestimeseriesSMA3)
plot.ts(milestimeseriesSMA8)

par(mfrow = c(1, 1))

#Moving average without trend and seasonality
plot(birthstimeseries)

birthsforecast <- 
  HoltWinters(birthstimeseries, 
              beta=FALSE, 
              gamma=FALSE)

birthsforecast
birthsforecast$fitted

plot(birthsforecast)
birthsforecast$SSE

plot(milestimeseries)

milesforecast <- 
  HoltWinters(milestimeseries, 
              beta=FALSE, 
              gamma=FALSE)

milesforecast
milesforecast$fitted

plot(milesforecast)
milesforecast$SSE

#Let us now 
#assume there is no 
#seasonality, but there 
#is trend

#We can specify the first 
#value and slope

#Additive, trend and seasonality models

birthsforecast <- 
  HoltWinters(birthstimeseries)
birthsforecast
birthsforecast$fitted

plot(birthsforecast)
birthsforecast$SSE

milesforecast <- 
  HoltWinters(milestimeseries)
milesforecast
milesforecast$fitted

plot(milesforecast)
milesforecast$SSE
milesresiduals <- residuals(milesforecast)
milesresiduals
plot(milesresiduals)
acf(milesresiduals)
pacf(milesresiduals)

library("forecast")

#it predicts seasonal peaks well

birthsforecast2 <- 
  forecast.HoltWinters(birthsforecast, 
                       h=8)

birthsforecast2

plot.forecast(birthsforecast2)
plot.forecast(birthsforecast2, 
              shadecols=terrain.colors(3))
plot.forecast(birthsforecast2,
              shadecols="oldstyle")

milesforecast2 <- 
  forecast.HoltWinters(milesforecast, 
                       h=40)

milesforecast2

plot.forecast(milesforecast2,
              shadecols="oldstyle")

# forecast with NO trend and seasonality

birthsforecast <- 
  HoltWinters(birthstimeseries, 
              beta=FALSE, 
              gamma=FALSE)
birthsforecast <- 
  forecast.HoltWinters(birthsforecast,
                       h=8)
birthsforecast

plot.forecast(birthsforecast,
              shadecols="oldstyle")

milesforecast <- 
  HoltWinters(milestimeseries, 
              beta=FALSE, 
              gamma=FALSE)
milesforecast <- 
  forecast.HoltWinters(milesforecast,
                       h=8)
milesforecast

plot.forecast(milesforecast,
              shadecols="oldstyle")

#ARIMA
plot(birthstimeseries)
birthstimeseriesdiff1 <- 
  diff(birthstimeseries, 
       differences=1)
plot.ts(birthstimeseriesdiff1)

birthstimeseriesdiff2 <- 
  diff(birthstimeseries, 
       differences=2)
plot.ts(birthstimeseriesdiff2)

auto.arima(birthstimeseries)

plot(milestimeseries)
milestimeseriesdiff1 <- 
  diff(milestimeseries, 
       differences=1)
plot.ts(milestimeseriesdiff1)

milestimeseriesdiff2 <- 
  diff(milestimeseries, 
       differences=2)
plot.ts(milestimeseriesdiff2)

milestimeseries <- auto.arima(milestimeseries,ic='aic')
milestimeseries

par(mfrow = c(1, 2))
acf(milestimeseries$residuals)
pacf(milestimeseries$residuals)
Box.test(milestimeseries$residuals, lag=20, type="Ljung-Box")

par(mfrow = c(1, 1))
milestimeseriesforecasts <- forecast.Arima(milestimeseries, 
                  h=40)
plot.forecast(milestimeseriesforecasts)
milestimeseriesforecasts

#Parsimonious models
birthstimeseries <- 
  auto.arima(birthstimeseries,
             ic='aic')
birthstimeseries
birthstimeseriesforecasts <- 
  forecast.Arima(birthstimeseries, 
                 h=5)
plot.forecast(birthstimeseriesforecasts)

# GDP forecast
# India's GDP growth rate
#GDP <- read.csv("GDP_AnnualGrowthRate_India.csv")
#GDP
#GDPtimeseries <- ts(GDP)
#GDPtimeseries

#plot(GDPtimeseries)

#GDPtimeseriescomponents <- 
#  decompose(GDPtimeseries)

#plot(GDPtimeseriescomponents)
# GDPtimeseriescomponents$seasonal
# GDPtimeseriescomponents$trend
# 
# GDPtimeseriesSeasonally <- 
#   GDPtimeseries - GDPtimeseriescomponents$seasonal
# 
# plot(GDPtimeseries)
# acf(GDPtimeseries, lag.max=20)
# pacf(GDPtimeseries, lag.max=20)
# acf(GDPtimeseries, lag.max=20, ci.type="ma")
# 
# auto.arima(GDPtimeseries)
# GDPtimeseries <- 
#   auto.arima(GDPtimeseries,
#              ic='aic')
# GDPtimeseries
# GDPtimeseriesforecasts <- 
#   forecast.Arima(GDPtimeseries, 
#                  h=5)
# plot.forecast(GDPtimeseriesforecasts)
# GDPtimeseriesforecasts
# 
# GDPtimeseriesarima <- arima(GDPtimeseries, order=c(2,1,2))
# class(GDPtimeseries)
# 
# GDPtimeseriesforecasts1 <-
#   forecast.Arima(GDPtimeseriesarima, h=5)
# plot.forecast(GDPtimeseriesforecasts1)
# GDPtimeseriesforecasts1

#acf(GDPtimeseriesarima$residuals)
#pacf(GDPtimeseriesarima$residuals)
#Box.test(GDPtimeseriesarima$residuals, lag=20, type="Ljung-Box")
