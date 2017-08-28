
#Compute the leverage (h value)

x=c(1,2,3,4,8)
xbar=mean(x)

 sdx = sd(x)
 
 #compute the standardized scores
 z <- (x-xbar)/sdx
 
 #compute leverage
 h <- (z^2 + 1)/length(x)
 