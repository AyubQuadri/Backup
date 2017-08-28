rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

library(lpSolve)

obj=c(350,300)
con=rbind(c(1,1), 
          c(9,6), 
          c(12, 16),
          c(1,0), 
          c(0, 1))
dir=c("<=", "<=", "<=", ">=", ">=")
rhs=c(200, 1566, 2880, 0, 0)

res=lp("max", obj, con, dir, rhs,
       compute.sens=1)

#Analyzing the results

res$solution
res
res$sens.coef.from
res$sens.coef.to

#Sensitivity and duality

obj=c(15,10)
con=rbind(c(1,0), c(0,1), c(1, 1))
dir=c("<=", "<=", "<=")
rhs=c(2, 3, 4)
res=lp("max", obj, con, dir, rhs, 
       compute.sens=1)

res
res$solution
res$sens.coef.from
res$sens.coef.to

res$duals
res$duals.from
res$duals.to

#Class room problem

obj=c(10,15)
con=rbind(c(10,3), c(7,6))
dir=c("<=", "<=")
rhs=c(60, 20)
res=lp("max", obj, con, dir, rhs, 
       compute.sens=1)

res
res$solution
res$sens.coef.from
res$sens.coef.to

obj=c(60,20)
con=rbind(c(10,7), c(3,6))
dir=c(">=", ">=")
rhs=c(10, 15)
res=lp("min", obj, con, dir, rhs, 
       compute.sens=1)

res
res$solution
res$sens.coef.from
res$sens.coef.to

#Integer solutions

obj=c(350,300)
con=rbind(c(1,1), c(9,6), c(12, 16), c(1,0), c(0, 1))
dir=c("<=", "<=", "<=", ">=", ">=")
rhs=c(200, 1520, 2650, 0, 0)
res=lp("max", obj, con, dir, 
       rhs, compute.sens=1)

resi=lp("max", obj, con, dir, 
        rhs, int.vec=1:2, 
        compute.sens=1)

#Analyzing the results

res$solution
resi$solution

#Budget allocation

obj= c(141, 187, 121, 83, 265, 127)

con= rbind(c(75,90,60,30,100,50), 
           c(25,35,15,20,25,20), 
           c(20,0,15,10,20,10), 
           c(15,0,15,5,20,30), 
           c(10,30,15,5,20,40))

dir=c("<=", "<=", "<=", "<=","<=")

rhs=c(250,75,50,50,50)

res=lp("max", obj, con, 
       dir, rhs, 
       binary.vec=1:6,
       compute.sens=1)
res
res$solution
-----
res$sens.coef.from
res$sens.coef.to

#Staff allocation problem

obj=c(680, 705, 705, 705, 
      705, 680, 655)
con=rbind(c(0,1,1,1,1,1,0), 
          c(0,0,1,1,1,1,1), 
          c(1,0,0,1,1,1,1), 
          c(1,1,0,0,1,1,1), 
          c(1,1,1,0,0,1,1), 
          c(1,1,1,1,0,0,1), 
          c(1,1,1,1,1,0,0), 
          c(1,0,0,0,0,0,0), 
          c(0,1,0,0,0,0,0), 
          c(0,0,1,0,0,0,0), 
          c(0,0,0,1,0,0,0), 
          c(0,0,0,0,1,0,0), 
          c(0,0,0,0,0,1,0), 
          c(0,0,0,0,0,0,1))

dir=c(">=",">=",">=",">=",
      ">=",">=",">=",">=",
      ">=",">=",">=",">=",
      ">=",">=")
rhs=c(18,27,22,26,25,21,19,
      0,0,0,0,0,0,0)

res=lp("min", obj, con, dir, 
       rhs, int.vec=1:7)
res
res$solution

#Stock selection

obj=c(0.0865, 0.095, 0.1, 0.0875, 
      0.0925, 0.09)

con=rbind(c(1,0,0,0,0,0), 
          c(0,1,0,0,0,0), 
          c(0,0,1,0,0,0), 
          c(0,0,0,1,0,0), 
          c(0,0,0,0,1,0), 
          c(0,0,0,0,0,1), 
          c(1,1,1,1,1,1), 
          c(1,1,0,1,0,1), 
          c(0,1,1,0,1,0))

dir=c("<=", "<=", "<=", 
      "<=", "<=", "<=", 
      "==", ">=", "<=")

rhs=c(187500, 187500, 187500, 
      187500, 187500, 187500, 
      750000, 375000, 262500)

res1=lp("max", obj, con, 
        dir, rhs, 
        compute.sens=1)

res1$solution

#Analyzing the results

res1$sens.coef.from
res1$sens.coef.to

#Product mix problem

obj  =  c(200,100,400,300) 
con = rbind(c(1,0,1,0), c(0,1,0,1),
            c(1,1,0,0),c(1,1,0,0),c(0,0,1,1),
            c(0,0,1,1),c(1,-2,0,0),c(0,0,1,-2),
            c(-1,10,0,0),c(0,0,-6,5), c(1,0,0,0), 
            c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))

#Coefficients for a total of 14 constraints

dir=c( "<=", "<=", ">=", "<=", ">=", 
       "<=", "<=", "<=", ">=", ">=", 
       ">=", ">=", ">=", ">=")

#All directions
rhs =  c(40000,60000,50000,100000, 
         5000,20000,0,0,0,0,0,0,0,0) 

res = lp("max", obj, con, dir, rhs) 
res$solution


#Transportation problem

#Transportation as linear programming

obj=c(21, 50, 40, 35, 30, 
      22, 55, 20, 25)
con=rbind(c(1,1,1,0,0,0,0,0,0), 
          c(0,0,0,1,1,1,0,0,0), 
          c(0,0,0,0,0,0,1,1,1), 
          c(1,0,0,1,0,0,1,0,0), 
          c(0,1,0,0,1,0,0,1,0), 
          c(0,0,1,0,0,1,0,0,1))

dir=c("==", "==", "==", 
      "<=", "<=", "<=")

rhs=c(275000, 400000, 300000, 
      200000, 600000, 225000)

res=lp("min", obj, con, dir, 
       rhs, compute.sens=1)

res
res$solution

#Method 2
  
costs=matrix(c(21, 50, 40, 35, 
               30, 22, 55, 20, 
               25, 0,0,0),
             byrow=TRUE, nrow=4)

row.signs = rep ("==", 4)
col.signs = rep ("<=", 3)

row.rhs = c(275000,400000,300000,
            50000)

col.rhs=c(200000, 600000, 225000)

res=lp.transport (costs, "min", 
                  row.signs, 
                  row.rhs, 
                  col.signs, 
                  col.rhs)
res$solution 

#Another transport problem

# Set up cost matrix

costs = matrix (10000, 8, 5); 
costs

costs[4,1] = costs[-4,5] = 0
costs

costs[1,2] = costs[2,3] = costs[3,4] = 7; 
costs[1,3] = costs[2,4] = 7.7
costs[5,1] = costs[7,3] = 8; 
costs[1,4] = 8.4; 
costs[6,2] = 9
costs[8,4] = 10; 
costs[4,2:4] = c(.7, 1.4, 2.1)
costs

# Set up constraint signs and right-hand sides.
row.signs <- rep ("<=", 8)
row.rhs <- c(200, 300, 350, 200, 100, 50, 100, 150)
col.signs <- rep (">=", 5)
col.rhs <- c(250, 100, 400, 500, 200)

res=lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)
res$solution
