#C:\Users\nahki\Documents\R\win-library\3.0\rstan\include\stansrc\models\bugs_examples\vol1\rats
setwd("D:/RW/stan/rats/")
library(rstan)
set_cppo("fast") #set_cppo('debug') 
library(doSNOW)
library(foreach)
cl<-makeCluster(4)

STAN_HOME<-'C:/Users/nahki/Documents/R/win-library/3.0/rstan'
dirpath <- paste0(STAN_HOME, '/include/stansrc/models/bugs_examples/vol1/rats')
#load data to list:dat
source(paste0(dirpath, "/rats.data.R"))
dat<-list(y=y, x=x, xbar=xbar,N=N,T=T)

## parallel processing each chain of stan
# do not sampling. just create model.
fit0<-stan(file=paste0(dirpath,"/rats.stan"), data=dat, chains=0)
sflist1<-foreach(i=1:4,.packages="rstan") %dopar% {
  stan(fit=fit0, data=dat, chains=1, chain_id=i, refresh=-1)
}

# merging the chains
f3<-sflist2stanfit(sflist1)

apply(extract(f3)$alpha,2,median)
plot(f3)
traceplot(f3)