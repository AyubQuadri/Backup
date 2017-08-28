rm(list=ls(all=TRUE))
setwd("C:/Users/admin/Desktop/RegBatch/Class3")

source("genAlgFun.R")

#Complexity
gamma(11)/(3*10^9)
gamma(51)/(3*10^9*86400*365)

# Create symmetric distance matrix

cities=50

distMat=matrix(rep(0,cities^2), nrow=cities)

distGen=function(x){
  distance=sample(100:1000,1)
  return(distance)
}

distMat=apply(distMat,1:2,distGen)
distMat=distMat+t(distMat)
diag(distMat)=0

initPop=data.frame()
for(i in 1:100){
  chr=sample(seq(1:cities))
  initPop=rbind(initPop,chr)
}
colnames(initPop)=seq(1:ncol(initPop))

eval<-function(individual){
  
  fitness=0
  
  for(i in 1:(length(individual)-1)){
    fitness=fitness+distMat[individual[i],individual[i+1]]
  }
  
  return(fitness)
}

mutate<-function(individual){
  #Interchange the values of two of the attributes
  a=sample(1:cities,2)
  k=individual[a[1]]
  individual[a[1]]=individual[a[2]]
  individual[a[2]]=k
  return(individual)
}

crossOver=function(p1,p2){
  swap=floor(length(p1)*0.5)
  pos=sample(1:length(p1),swap, replace=F)
  
  for(l in pos){
    v=p2[l]
    for(i in 1:length(p1)){
      if(p1[i]==v) { 
        a=sort(c(l,i))
        k=p1[a[1]]
        p1[a[1]]=p1[a[2]]
        p1[a[2]]=k
        i=length(p1)
      }
    }  
  }
  return(p1)
}

geneticAlgo(initPop, eval, mutate, 
            crossOver,0.25,0.5,10)
geneticAlgo(initPop, eval, mutate, 
            crossOver,0.05,0.1,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.1,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.005,0.1,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.2,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.3,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.3,20)