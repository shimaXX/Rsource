#C:\Users\nahki\Documents\R\win-library\3.0\rstan\include\stansrc\models\bugs_examples\vol1\rats
setwd("D:/RW/stan/rats/")
library(rstan)
set_cppo("fast") #set_cppo('debug') 

STAN_HOME<-'C:/Users/nahki/Documents/R/win-library/3.0/rstan'
dirpath <- paste0(STAN_HOME, '/include/stansrc/models/bugs_examples/vol1/rats')
#load data to list:dat
source(paste0(dirpath, "/rats.data.R"))
dat<-list(y=y, x=x, xbar=xbar,N=N,T=T)
fit1 <- stan(file=paste0(dirpath,"/rats.stan"), data=dat,
             iter=1000, chains=4)
apply(extract(fit1)$alpha,2,median)
plot(fit1)
traceplot(fit1)

#fit again using the previous fit result
fit3<-stan(fit=fit1, data=dat, iter=400, chains=4)
traceplot(fit3)