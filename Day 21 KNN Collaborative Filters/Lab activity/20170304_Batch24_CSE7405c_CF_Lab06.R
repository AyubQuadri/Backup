#https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
rm(list=ls(all=TRUE))

#install.packages("recommenderlab")
#install.packages("lsa")

library(recommenderlab) 
library(lsa)

#simulating a user-item rating matrix
set.seed(5643)
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))  
m
sum(is.na(m))  

#converting into a realRatingMatrix (the object stores data in sparse format)
r <- as(m, "realRatingMatrix")
getRatingMatrix(r)

#Normalization
r_m <- normalize(r)
getRatingMatrix(r_m)
(as(r_m, "data.frame")) #converts dataframe

#Small portions of rating matrices can be visually inspected using image().
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")
r_b <- binarize(r, minRating=4)
image(r_b)

#creating recommender
getRatingMatrix(r)
r1 <- Recommender(r, method = "UBCF") #input is un-normalized  data 
r1
getModel(r1)

# Recommendations
recom <- predict(r1, r[1]) #get recommendations for user1
as(recom, "list")
as(recom, "matrix")
getRatingMatrix(r[1])

recom1 <- predict(r1, r[1], type="ratings")
recom1
as(recom1, "list")
as(recom1, "matrix")
getRatingMatrix(r)
##############################
cosine(t(as(r@data[1:5,],"matrix")))#of un-normalized matrix
getRatingMatrix(r)
recom2 <- predict(r1, r[4], type="ratings")
recom2
as(recom2, "list")
as(recom2, "matrix")
##############################

#split the data into train and evaluation sets
e <- evaluationScheme(r, method="split", train=0.7,
                      given=3)

# We create two recommenders (user-based and item-based collaborative
#filtering) using the training data.
getRatingMatrix(getData(e, "train"))

#UBCF
r2 <- Recommender(getData(e, "train"), "UBCF")
r2

#IBCF
r3 <- Recommender(getData(e, "train"), "IBCF")
r3

# Compute predictions for the known part of 
#the test data (3 items for each user) using the two algorithms.
getRatingMatrix(getData(e, "known"))

#Predict UBCF
p1 <- predict(r2, getData(e, "known"), type="ratings")
p1
as(p1, "list")
#as(p1, "matrix")
getRatingMatrix(r)
getRatingMatrix(getData(e, "known")) #retains 3 rankings from each record of test-set
getRatingMatrix(getData(e, "unknown"))#removes the 3 rankings from the test-set

#Predict IBCF
p2 <- predict(r3, getData(e, "known"), type="ratings")
p2
as(p2, "list")
#as(p2, "matrix")
getRatingMatrix(getData(e, "known"))
getRatingMatrix(getData(e, "unknown"))

# Finally, we can calculate the error between the prediction and the
#unknown part of the test data.

error <- rbind(
  calcPredictionAccuracy(p1, getData(e, "unknown")),
  calcPredictionAccuracy(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error

#In this example user-based collaborative filtering produces a 
#smaller prediction error.
