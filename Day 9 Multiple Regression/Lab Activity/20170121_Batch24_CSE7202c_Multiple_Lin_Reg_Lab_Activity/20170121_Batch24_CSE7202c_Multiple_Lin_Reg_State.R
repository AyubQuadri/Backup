rm(list=ls(all=TRUE))
setwd("D:/AMITS_LABS/BATCH 24/Day10_Mulp_Lin_Reg")

state.x77 # output not shown
str(state.x77) # clearly not a data frame! (it's a matrix)
st = as.data.frame(state.x77,row.names = FALSE) 

#Add a population density variable. Note that population density 
#is a linear function of Population and Area, so these three vectors
#will not be linearly independent. 
colnames(st)[4] = "Life.Exp"                   
colnames(st)[6] = "HS.Grad"                    
st$Density = st$Population * 1000 / st$Area    
summary(st)
round(cor(st),3)
pairs(st) # same as plot(st)

# Life expectancy appears to be moderately to 
# strongly related to "Income" (positively), 
# "Illiteracy" (negatively), "Murder" (negatively),
# "HS.Grad" (positively), and "Frost" (no. of days per year
# with frost, positively), 

# par(mfrow=c(3,3))
# for(i in 1:9) qqnorm(st[,i], main=colnames(st)[i])
# #Model
model1 = lm(Life.Exp ~ ., data=st)
summary(model1)

model2 = update(model1, .~. -Illiteracy)
summary(model2)
model3 = update(model2, .~. -Area)
summary(model3)
model4 = update(model3, .~. -Income)
summary(model4)
model5 = update(model4, .~. -Density)
summary(model5)

#AIC
step(model1, direction="backward")
model_AIC= lm(formula = Life.Exp ~ Population + Murder + 
              HS.Grad + Frost, data = st)
summary(model_AIC)

par(mfrow=c(2,2))                    
plot(model_AIC)
par(mfrow=c(1,1))

#Beta Coefficients
# Beta or standardized coefficients are the slopes we would get if
# all the variables were on the same scale. They allow comparisons
# of the approximate relative importance of the predictors.
# Beta Coeff: coefficient times the SD of that variable divided by 
# the SD of the response variable gives the beta coefficient for
# the variable. 

# Standardization of the coefficient is usually done to answer
# the question of which of the independent variables have a greater
# effect on the dependent variable in a multiple regression analysis,
# when the variables are measured in different units of measurement
# (for example, income measured in dollars and family size measured 
# in number of individuals).

coef(model_AIC)

sdexpl = c(sd(st$Population), sd(st$Murder), sd(st$HS.Grad), sd(st$Frost))
sdresp = sd(st$Life.Exp)

coef(model_AIC)[2:5] * sdexpl / sdresp

# So an increase of one SD in the murder rate is associated with a 
# drop of 0.82 SD in life expectancy, if all the other variables are
# held constant. 

# We can see that HS.Grad and Frost are of roughly the same 
#importance in predicting Life.Exp. 
# Clearly, in our model, Murder is the most important predictor of
# Life.Exp.

######################
library(DMwR)
#Error verification
regr.eval(st$Life.Exp, model_AIC$fitted.values) 
summary(model_AIC)

