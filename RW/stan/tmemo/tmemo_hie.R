setwd("D:/RW/stan/tmemo")
library(rstan)
set_cppo("fast")
library(doSNOW)
library(foreach)
cl<-makeCluster(4)

# read data
d <- read.csv("tmemo_test.csv")

N<-nrow(d)
N_col<-ncol(d)

Y<-d[,1]
X<-d[,4:N_col]
minutes<-d[,2]
X_M<-d[,3]

N_col<-ncol(X)
v0 <- matrix(0,nrow=N_col, ncol=N_col)
for(i in 1:N_col){ v0[i,i]<-100}
sigma0 <- matrix(0,nrow=N_col, ncol=N_col)
for(i in 1:N_col){ sigma0[i,i]<-100}

dat<-list(Y=Y, X=X, minutes=minutes, N=N, N_col=N_col, X_M=X_M, v0=v0, sigma0=sigma0)

## parallel processing each chain of stan
# do not sampling. just create model.
fit0<-stan(file='tmemo_hie.stan', data=dat, chains=0)
sflist2<-foreach(i=1:4,.packages="rstan") %dopar% {
  stan(fit=fit0, data=dat, iter=1000, thin=3,
       #pars = c("beta"),
       chains=1, chain_id=i, refresh=-1)
}

# merging the chains
f3<-sflist2stanfit(sflist2)
stopCluster(cl)

apply(extract(f3)$alpha,2,median)
plot(f3)
traceplot(f3)