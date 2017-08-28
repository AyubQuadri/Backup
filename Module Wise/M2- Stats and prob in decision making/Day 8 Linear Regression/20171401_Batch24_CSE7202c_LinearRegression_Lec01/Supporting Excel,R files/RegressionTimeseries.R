setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\RegressionTimeSeries\\2017-01-14\\Day1")

#Regression MTcars data

mtcarslm <- lm(mpg~wt,data=mtcars)
summary(mtcarslm)
par(mfrow=c(1,1))
plot(mtcars$wt,mtcars$mpg)
abline(mtcarslm)
par(mfrow=c(2,2))
plot(mtcarslm)

# Regression

bigmac <- read.csv("BigMac-NetHourlyWage.csv", header = T, sep = ",")
bigmac
lm(bigmac$NetHourlyWage~bigmac$BigMacPrice,bigmac)
bigmaclm <- lm(NetHourlyWage~BigMacPrice,data=bigmac)
summary(bigmaclm)
par(mfrow=c(1,1))
plot(bigmac$BigMacPrice,bigmac$NetHourlyWage)
abline(bigmaclm)
par(mfrow=c(2,2))
plot(bigmaclm)


# Linear Regression Model Building
CrudeOilOutput <- read.csv("CrudeOilOutput.csv", header = T, sep = ",")
CrudeOilOutput
correlation <- cor(CrudeOilOutput)
plot(CrudeOilOutput)
CrudeOilOutputlm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
                       + CrudeOilOutput$USAutoFuelRate
                       + CrudeOilOutput$USNuclear + CrudeOilOutput$USCoal
                       + CrudeOilOutput$USDryGas, CrudeOilOutput)
summary(CrudeOilOutputlm)
par(mfrow=c(2,2))
plot(CrudeOilOutputlm)

Step1lm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USAutoFuelRate)
summary(Step1lm)

Step2lm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
              +CrudeOilOutput$USDryGas)
summary(Step2lm)

Step3lm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
              +CrudeOilOutput$USAutoFuelRate+CrudeOilOutput$USNuclear)
summary(Step3lm)

# All Subsets Regression
library(leaps)
attach(CrudeOilOutput)
leapOut<-regsubsets(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
                  + CrudeOilOutput$USAutoFuelRate
                  + CrudeOilOutput$USNuclear + CrudeOilOutput$USCoal
                  + CrudeOilOutput$USDryGas, CrudeOilOutput, nbest=5)
# view results
#summary(leapOut)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
#plot(leapOut,scale="r2")
# plot statistic by subset size
#library(car)
#subsets(leapOut, statistic="rsq") 

library(MASS)
library(car)
library(VIF)

stepAIC(CrudeOilOutputlm, direction = "both")
car::vif(CrudeOilOutputlm)

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include Moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Scatterplot of teams and their wins
plot(moneyball$W, moneyball$Team, 
     col=ifelse(moneyball$Playoffs < 1, 'black','red'), pch=20, cex=2,
     abline(v=95, col='blue', lwd=3))

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# install.packages("lattice")
library(lattice)

# Scatterplot to check for linear relationship
xyplot(moneyball$W~moneyball$RD, type=c('p', 'smooth'), col='steelblue2',
       cex=1.5, col.line='red', pch=19, lwd=2,
       ylab=list(label='Wins', cex=2), xlab=list(label='Run Difference',cex=2),
       scales=list(x=list(cex=2), y=list(cex=2)))

# Correlation between Wins and Run Difference
cor(moneyball$W, moneyball$RD)

# Model Building for Wins
MoneyballlmStep1a <- lm(moneyball$W ~ moneyball$RD)
summary(MoneyballlmStep1a)
MoneyballlmStep1b <- lm(moneyball$W ~ moneyball$OBP)
summary(MoneyballlmStep1b)
MoneyballlmStep1c <- lm(moneyball$W ~ moneyball$SLG)
summary(MoneyballlmStep1c)
MoneyballlmStep1d <- lm(moneyball$W ~ moneyball$BA)
summary(MoneyballlmStep1d)
MoneyballlmStep1e <- lm(moneyball$W ~ moneyball$OOBP)
summary(MoneyballlmStep1e)
MoneyballlmStep1f <- lm(moneyball$W ~ moneyball$OSLG)
summary(MoneyballlmStep1f)

MoneyballlmStep2a <- lm(moneyball$W ~ moneyball$RD + moneyball$OBP)
summary(MoneyballlmStep2a)
stepAIC(MoneyballlmStep2a)
MoneyballlmStep2b <- lm(moneyball$W ~ moneyball$RD + moneyball$SLG)
summary(MoneyballlmStep2b)
MoneyballlmStep2c <- lm(moneyball$W ~ moneyball$RD + moneyball$BA)
summary(MoneyballlmStep2c)
MoneyballlmStep2d <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP)
summary(MoneyballlmStep2d)
MoneyballlmStep2e <- lm(moneyball$W ~ moneyball$RD + moneyball$OSLG)
summary(MoneyballlmStep2e)

MoneyballlmStep3a <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$OBP)
summary(MoneyballlmStep3a)
MoneyballlmStep3b <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$SLG)
summary(MoneyballlmStep3b)
MoneyballlmStep3c <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$BA)
summary(MoneyballlmStep3c)
MoneyballlmStep3d <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$OSLG)
summary(MoneyballlmStep3d)

moneyballlm <- lm(W ~ RD + SLG + OBP + BA + OOBP + OSLG, data = moneyball)
moneyballlm

stepAIC(moneyballlm, direction = "both")
vif(stepAIC(moneyballlm, direction = "both"))

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# Regression model to predict runs scored
RunsScoredReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsScoredReg)

RunsScoredReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsScoredReg)

# Regression model to predict runs allowed
RunsAllowedReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAllowedReg)
