# Removing all the variables from the workspace
rm(list = ls(all.names = T))

##PCA in R###
# Creating a data frame
d = data.frame(x=c(2.5,0.5,2.2,1.9,3.1,2.3,2.0,1.0,1.5,1.1), 
               y=c(2.4,0.7,2.9,2.2,3.0,2.7,1.6,1.1,1.6,0.9))

# compute the PCs
pca = princomp(d)
names(pca)
summary(pca)

# use loadings and scores to reproduce original data (Loadings=Eigenvectors???Eigenvalues?????????????????????????????????.)
### Observe that loadings correspond to eigen vectors
loadings = t(pca$loadings)
### Correspond to values in the new basis
scores = pca$scores 

# Reproducing the original data
reproduce = scores %*% loadings  + colMeans(d)

## only with 1PC
loadings_1 = t(pca$loadings[,1])
scores_1 = pca$scores[,1]
reproduce = scores_1 %*% loadings_1  + colMeans(d)


# Removing all the variables from the workspace
rm(list = ls(all.names = T))

# R code to demonstrate Principal component analysis
# Attaching a specific library and understanding the data

# "attitude" Data Description
# From a survey of the clerical employees of a large financial organization, 
# the data are aggregated from the questionnaires of the approximately 35
# employees for each of 30 (randomly selected) departments. 
# The numbers give the percent proportion of favourable responses to seven
# questions in each department.

data(attitude)
names(attitude)
summary(attitude)
str(attitude)
attach(attitude)

#constructing a linear regression model
model1= lm(rating ~ complaints+
             privileges+
             learning+raises+
             critical+
             advance)

# Check the summary of the model
summary(model1)

# Removing the target attribute
data <- attitude[,-c(1)]

library(vegan)
# Normalizing the data
data = decostand(data,method = "range")

# Running a PCA and analyzing the values
pca_data <- princomp(data)
# Summary of the PCA model
summary(pca_data)
plot(pca_data)
print(pca_data)

# Getting the eigen vectors
pca_data$loadings[,]

# creating the data set with four components and target, Considering the components that explains 90% variance in the data
data2 <- data.frame(rating,pca_data$scores[,1:4])

# Building the linear regression model using the PCA components
model2 = lm(rating ~ .,data=data2)
summary(model2)

# Running a PCA using prcomp function and analyzing the values
prcomp_data <- prcomp(data)
predict(prcomp_data)
# Summary of the PCA model
summary(prcomp_data)

# Getting the cumiliative proportions of the components
cum_Prop =  summary(prcomp_data)$importance[3,]
# Setting the threshold
CUT_OFF = 0.92
comp_Names =  names(cum_Prop[cum_Prop <= CUT_OFF])
plot(prcomp_data)
print(prcomp_data)

# Getting the eigen vector
prcomp_data$rotation[,]
# Getting the principle componets
prcomp_data$x
# Getting the principle componets values
predict(prcomp_data)

#creating the data set with four components and target, Considering the components that explains 90% variance in the data
data2 <- data.frame(rating,prcomp_data$x[,1:4])
# Above and below lines are same
#creating the data set with four components and target, Considering the components that explains 90% variance in the data
data2 <- data.frame(rating,prcomp_data$x[,c(comp_Names)])
# Building the linear regression model using the PCA components
model3 = lm(rating ~ .,data=data2)
summary(model2)