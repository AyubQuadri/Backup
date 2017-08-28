rm(list = ls(all=T))
setwd("D:/AMITS_LABS/BATCH 24/Day19_SVM")
dat <- read.csv('BostonHousing.csv')
dat$chas <- as.numeric(dat$chas)
str(dat)

tr.rows <- sample(1:nrow(dat), size = round(nrow(dat)*0.75), replace = F)
tr <- dat[tr.rows,]
te <- dat[-tr.rows,]

#Build SVM model
library(e1071)
mod_reg <- svm(x = tr[, 1:13], y = tr[,14],
               type = 'nu-regression', kernel = 'linear', cost = 1e-7)
summary(mod_reg)

#Calculate errors
library(DMwR)
regr.eval(te$medv, predict(mod_reg, te[,1:13]))

#Tune model
tuned <- tune.svm(medv ~., data = tr, type = 'nu-regression', 
                  gamma = c(10^(-6:-1), seq(0.1, 0.5, 0.1)), cost = 10^(1:2))
plot(tuned)
summary(tuned)
# Build model with tuned parameters
tuned_model <-svm(x = tr[, 1:13], y = tr[,14],
                  type = 'nu-regression', kernel = 'linear', gamma =0.1,
                  cost = 10)

regr.eval(te$medv, predict(tuned_model, te[,1:13]))

############
#Build Lin Reg Model
mod_lm <- lm(medv~., tr)
regr.eval(te$medv, predict(mod_lm, te))


