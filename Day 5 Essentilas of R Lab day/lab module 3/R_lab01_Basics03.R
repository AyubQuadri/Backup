setwd()

###### Merging Data frames ############################################

Grade1<-read.csv("Grade1.csv",header=T,sep=",")
Grade2<-read.csv("Grade2.csv",header=T,sep=",")

#To get the dimensions of the data (number of records and number of variables)
dim(Grade1)
#To get the structure and summary of the data
str(Grade1)
summary(Grade1)
head(Grade1) #outputs the first 6 records
tail(Grade1) #outputs the last 6 records

#Calling the elements using the column names
Grade1$Student.id[1] # first element in the name column
Grade1$English1[2:3] #second and third elements in the Marks column

#Referring the elements by position using the row and column indices

Grade1[,2] #all elements in second column are displayed
Grade1[1:2,3] # first and second values from the third column
Grade1[c(1,7,5),] #the first, seventh and 5th records from all columns of data

#We observed above that the structure for student.id is numeric.Does it make sense to have statistical summary of it
#When data is loaded in R, by default R assigns a data type to it. If we find it inappropriate we manually change it

Grade1$Student.id<-as.character(Grade1$Student.id) # converts into a character variable

##Similarly if we want to convert a numeric value to factor(ordinal) we use as.factor as we have used above.
#Converting student.id to numeric again

Grade1$Student.id<-as.numeric(Grade1$Student.id)

##We have two data sets of Grades for students. We observe that student.id is common in both
##We want to merge these two data sets into one.

mergeddata <- merge(Grade1,Grade2) ##There are other forms of merging like left join/outer join,inner join etc. 
## Explore the possible joins on other data sets


################################Data Exploration and Data Aggregation Methods#######################
##These form an important aspect especially for data exploration, data understanding and to processing
## the data for model building
detach(mtcars)
attach(mtcars)
##A data frame can have multiple datatypes in it like numeric, factor and logical.
library(plyr)
attach(baseball)
data<-baseball
str(data) ##outputs what to which type each variable belong to.
summary(data) ## gives the overall summary of the data,we observe that the stats are given for numerical
## attributes, if characters then class and mode are mentioned.

##Conversion of variable types if necessary
##We can consider "teams" as a factor ao that we can compare runs batted and home runs for teams
data$team<-as.factor(data$team)
str(data$team)
##We do this appropriate conversions first

##Missing Values
##To count the number of missing values
sum(is.na(data)) ##Gives the number of missing values in the data. What to do with the missing values

#option1. Omit all records with NA values
data1<-na.omit(data)  ##it omits all the records which has atleast one NA value in it
data2<-data[complete.cases(data),]  ##another way

#Option2. If the missing values are a few, then we can impute these missing values
library(DMwR)
data3<-centralImputation(data) #Cenral Imputation
sum(is.na(data3))

data4<-knnImputation(data[,-c(1,4,5)],scale=T,k=5) #KNN Imputation
sum(is.na(data4))

temp <- data.frame(x = 1:10, Y = c(T, T, F, F,F,F,NA,NA,T,T), Z=c(NA,NA,7,8,9,5,11,9,9,4) )
sum(is.na(temp))

temp1<-centralImputation(temp) #Cenral Imputation
sum(is.na(temp1))

temp2<-knnImputation(temp,scale=T,k=1) #KNN Imputation
sum(is.na(temp2))

##several aggegation functions

##Using tapply, aggragate function and ddply
##tapply function is very flexible function for summerizing a vector x. 
tapply(data$rbi,data$team,FUN= sum,na.rm=T)

#aggregate(x,by,FUN,...)
aggregate(data$rbi,by=list(data$team),FUN=sum,na.rm=T)

##ddply
ddply(data,.(team),summarize, runs=sum(rbi,na.rm=T))

##Please use R resources for exploring the above functions

##Randomly split  the data into two
rows<-seq(1,nrow(data),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(data))
Train<-data[trainrows,]
Test<-data[-trainrows,]
##By using a package caTools
require(caTools)
set.seed(123) 
sample = sample.split(data$team, SplitRatio = .70)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

##############Working with the data ##########################
##There are several steps that we would follow for data preprocessing steps. These are not exhaustive 
##but according to the need we may use only some of these or sometimes we need to do a bit extra processing

##Understanding the data variables-- what are their types
##Data type conversions, if while loading the data the type taken is not appropriate
##Looking at the missing values  --either removing or imputing them
### Descriptive stats for distribution of data and for outlier detection
## Standardizing the data-- why scaling is important
#a. Using Standardization
#b. Using range
## Converting the variables 
#From Categorical to numeric  --Dummy
#From Numeric to categorical  -- Dicretizing (Equal Width, Equal Freq), Manual Coding

data <- read.csv(file="dataMerged.csv",header=TRUE, sep=",")

#Undertanding data structure
str(data)
head(data)
tail(data)
edit(data)
names(data)
summary(data)

#Missing values handling

sum(is.na(data))#To check null values in data

#Dropping the recrods with missing values
data2<-na.omit(data)
rm(data2)
#To identify rows where more than 20% attributes are missing
library(DMwR)
length(manyNAs(data, 0.2) )
data[74,]

#Imputing missing values
data$family <- as.factor(data$family)
data$edu <- as.factor(data$edu)
data$cc <- as.factor(data$cc)
data$cd <- as.factor(data$cd)
data$securities <- as.factor(data$securities)
data$online <- as.factor(data$online)
data$loan <- as.factor(data$loan)
str(data)
library(DMwR)
data2<-centralImputation(data) #Cenral Imputation
sum(is.na(data2))
data3<-knnImputation(data,scale=T,k=5) #KNN Imputation
sum(is.na(data3))
write.csv(data2, "data_imputed.csv", row.names=FALSE)

#Standardizing the data
#Subsetting data
Data_NumAtr<-subset(data2,select=c(age,exp,inc,mortgage,ccAvg))
Data_CatAtr<-subset(data2,select=-c(age,exp,inc,mortgage,ccAvg))
library(vegan)
#Using range method
dataStd. <- decostand(Data_NumAtr,"range") 
summary(dataStd.)
#Using Z score method
dataStd. <- decostand(Data_NumAtr,"standardize")
summary(dataStd.)

#Discretizing the variable
summary(data2)
library(infotheo)
IncomeBin <- discretize(data2$inc, disc="equalfreq",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data2$inc,IncomeBin,min)
tapply(data2$inc,IncomeBin,max)

IncomeBin <- discretize(data2$inc, disc="equalwidth",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data2$inc,IncomeBin,min)
tapply(data2$inc,IncomeBin,max)

#Manual recoding 
summary(data2$age)
data2$ageNew<-0
for (i in 1:nrow(data)){
  if (data2$age[i]>=45){ 
    data2$ageNew[i]=2
  }
  else {
    data2$ageNew[i]=1
  }
}
table(data2$ageNew)
tapply(data2$age,data2$ageNew,min)
tapply(data2$age,data2$ageNew,max)

#Creating dummy variables and adding to original table
library(dummies)
EduDummyVars<-dummy(data2$edu)
head(EduDummyVars)
data<-data.frame(data,EduDummyVars)
head(data)


#####################################Reshape############################
##Load in the CustTransDat.csv and Grade1.csv
Cust<-read.csv("CustTransDat.csv",header=T,sep=",")
##Sometimes it may be useful to change the data the way it looks. Forexample we have an 
## a transaction data of customers for a store. We would like to know the revenue generated 
##by year and by quarter. Then reshaphing would give a good representation
names(Cust)
library(reshape2)
data2<-dcast(Cust,Quarter+Month~Year,fun.aggregate = sum,value.var="Cost")


##Another Example
meltdata<-melt(data=Grade1,id="Student.id")
head(meltdata)

#aggregating the data based on subject and gender. This is also called the wide format.
data2<-cast(data=meltdata,Student.id~variable,value="value") 
data3<-cast(data=meltdata,variable~Student.id,value="value",fill=0) #Observe the difference in data2 and data3

###Resources for Reshape- http://seananderson.ca/2013/10/19/reshape.html




