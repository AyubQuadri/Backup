#set working Directory
setwd()

#a. read data
data <- read.csv("housing.csv",header = T,sep = ",")

#b(i). str and summary
str(data)
summarise(data)

#b(ii). Missing values and measures to deal with
sum(is.na(data)) # 14 null values
#imputation technique

library(DMwR)
data2<-centralImputation(data) #Cenral Imputation
sum(is.na(data2))


#b(iii) Bin into 5 bins using 
#(a) equal frequency 
library(infotheo)
IncomeBin <- discretize(data2$Distance, disc="equalfreq",nbins=5)
table(IncomeBin)


#(b) width method
IncomeBin <- discretize(data2$Distance, disc="equalwidth",nbins=5)
table(IncomeBin)

#(C) Slpit data into train and test 
library(caTools)
require(caTools)
set.seed(123)
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE) # train 362 records
test = subset(data, sample == FALSE) # test 144 records


#(D) Standardize 
Data_NumAtr<-subset(data2,select=c(AGE))
Data_NumAtr
library(vegan)
#Using Z score method
dataStd <- decostand(Data_NumAtr,"standardize")
summary(dataStd)


#(E) Create Dummy variable for RAD by changing it into factor 
library(dummies)
RADDummyVars<-dummy(data2$RAD)
head(RADDummyVars)
as.factor(RADDummyVars)
str(RADDummyVars)
class(RADDummyVars)

#(F) IR for all the numeric values
All_Numeric = subset(data2, select=-c(CHAS,RAD,TAX)) #created sub set with all numeric values
summary(All_Numeric)
IQR(All_Numeric$Crimerate)
IQR(All_Numeric$ResiLandZone)
IQR(All_Numeric$INDUS)
IQR(All_Numeric$NOX)
IQR(All_Numeric$Rooms)
IQR(All_Numeric$AGE)
IQR(All_Numeric$Distance)
IQR(All_Numeric$PTRATIO)
IQR(All_Numeric$Blacks)
IQR(All_Numeric$LSTAT)
IQR(All_Numeric$OwnOcc)




  
#G Box plot of Ownocc vs RAD using ggplot
library(ggplot2)
p <- ggplot(data2, aes(factor(data2$RAD), data2$OwnOcc))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter()
p + geom_boxplot(outlier.colour = "green", outlier.size = 3) + geom_jitter()
p + geom_boxplot(aes(fill=RAD))
p + geom_boxplot(aes(fill=factor(RAD)))


#H Histogram of TAX  
hist(data2$TAX, col="green", xlab="TAX",main="Histogram of TAX")

library(ggplot2)
m <- ggplot(data2, aes(x=TAX))
m + geom_histogram(bins=10)
m + geom_histogram(bins=10,aes(fill=..count..))


