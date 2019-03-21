
#load the data
library(faraway)
data(savings)
head(savings)

sr=savings$sr
pop15=savings$pop15
pop75=savings$pop75
dpi=savings$dpi
ddpi=savings$ddpi

OriginalModel=lm(sr~pop15+pop75+dpi+ddpi)

EBeta=OriginalModel$coefficients


#the residuals of the original model
eHat=OriginalModel$residuals

for(s in 1:5){
#BetaHats is the vector that stores the estimate of betas in each test
BetaHats=vector(1000,mode="numeric")
beta=EBeta[s]

#take N=1000
for(i in 1:1000){
  
  #resample with replacement to obtain the jth ehat
  ejHat=vector(50,mode="numeric")
  ejHat=sample(1:50,50,1)
  for(k in 1:50){
    ejHat[k]=eHat[ejHat[k]]
  }
  
  #generate y
  yHat=vector(50,mode="numeric")
  for(j in 1:50){
    yHat[j]=EBeta[1]+pop15[j]*EBeta[2]+pop75[j]*EBeta[3]+dpi[j]*EBeta[4]+ddpi[j]*EBeta[5]+ejHat[j]
  }
  
  #run the regression
  model=lm(yHat~pop15+pop75+dpi+ddpi)
  BetaHats[i]=model$coefficients[s]
}

#generate the empirical distribution
Distribution=vector(1000,mode="numeric")
for(i in 1:1000){
  Distribution[i]=BetaHats[i]-beta
}

SortedDistribution=sort(Distribution)

t25=SortedDistribution[25]
t975=SortedDistribution[975]

#resample with replacement to obtain the jth ehat
ejHat=vector(50,mode="numeric")
ejHat=sample(1:50,50,1)
for(k in 1:50){
  ejHat[k]=eHat[ejHat[k]]
}

#generate y
yHat=vector(50,mode="numeric")
for(j in 1:50){
  yHat[j]=EBeta[1]+pop15[j]*EBeta[2]+pop75[j]*EBeta[3]+dpi[j]*EBeta[4]+ddpi[j]*EBeta[5]
}

#run the regression to estimate beta
testmodel=lm(yHat~pop15+pop75+dpi+ddpi)
ebeta=testmodel$coefficients[s]

#construct the confidence interval
interval=c(ebeta-t975,ebeta-t25)
print(interval)

}
