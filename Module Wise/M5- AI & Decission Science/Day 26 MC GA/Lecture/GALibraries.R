rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
setwd("C:/Users/admin/Desktop/RegularBatch")

library(rgenoud)

fn=function(x){
  y=4*x^3-3*x^2
  }

minima<- genoud(fn, nvars=1,
                pop.size=100,
                max=FALSE)

fn=function(x){
  y=8*x-4*x^2 
  }

maxima<- genoud(fn, nvars=1,
                pop.size=100,
                max=TRUE)

fn=function(x){
  y=3*x^3-9*x
  return(y)
  }

minima = genoud(fn, nvars=1, 
                pop.size=100, 
                max=FALSE)

#Knapsack problem 
#(ref: http://fishyoperations.com/r/genetic-algorithms-a-simple-r-example/)

library(genalg)

dataset <- data.frame(item = 
                        c("pocketknife", 
                          "beans", 
                          "potatoes",
                          "onions", 
                          "sleeping bag", 
                          "rope", 
                          "compass"), 
                      survivalpoints = c(10, 20, 
                                         15, 2, 
                                         30, 10, 30), 
                      weight = c(1, 5, 
                                 10, 1, 7, 5, 1))
weightlimit <- 20

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) 
  else return(-current_solution_survivalpoints)
}

GAmodel <- rbga.bin(size = 7, 
                    popSize = 200, 
                    iters = 100, 
                    mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))

#Using Linear programming

library(lpSolve)
f.obj <- c(10, 20,15, 2, 30, 10, 30)
f.con <- matrix(c(1, 5, 10, 1, 7, 5, 1),1,
                7,byrow=TRUE)
f.dir <- c("<=")
f.rhs <- c(20)
s1 <- lp ("max", f.obj, f.con, f.dir,
          f.rhs, binary.vec=1:7)
lp ("max", f.obj, f.con, f.dir,f.rhs,binary.vec=1:7)$solution