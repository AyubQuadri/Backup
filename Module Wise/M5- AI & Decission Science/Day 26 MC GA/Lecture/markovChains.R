rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

transit=matrix(c(0.9,0.1,0.5,0.5),nrow=2)
transit

nxt=c(100,0)
nxt=c(0,100)

for (i in 1:10){
    nxt=transit%*%nxt
    cat("Step is", i, "distrib =", nxt, "\n")
}

for(j in 1:10){
  
  a=runif(1,0,1)
  nxt=c(a, 1-a)
  cat("Sim is", j, "Start =", nxt, " ")
  
  for (i in 1:100){
    nxt=transit%*%nxt
  }
  cat("Final =", nxt, "\n")
}

t <-transit%*%transit
t <-t%*%t
t

#Toothpaste

transit=matrix(c(0.9,0.1,0.4,0.6),
               nrow=2)
#nxt=c(1,0)
nxt=c(0.2,0.8)


for (i in 1:20){
  nxt=transit%*%nxt
  cat("Step is", i, "distrib =", nxt, "\n")
}
