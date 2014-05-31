setwd("D:/RW/stan/test")
library(rstan)
set_cppo("fast")
library(doSNOW)
library(foreach)
cl<-makeCluster(4)

# read data
#d <- read.csv("testdata2.csv")
data(Nile)
dat <- list(y = as.vector(Nile), n = length(Nile), y1 = Nile[1])

#N<-nrow(d)
#y<-d$y
#x<-d$x
#dat<-list(N=N, y=y, x=x)

## parallel processing each chain of stan
# do not sampling. just create model.
fit0<-stan(file='test2.stan', data=dat, chains=0)
sflist2<-foreach(i=1:4,.packages="rstan") %dopar% {
  stan(fit=fit0, data=dat, iter=4000, thin=3,
       pars = c("x", "sigma_x", "sigma_y"),
       chains=1, chain_id=i, refresh=-1)
}

# merging the chains
f3<-sflist2stanfit(sflist2)
stopCluster(cl)

#apply(extract(f3)$alpha,2,median)
plot(f3)
traceplot(f3)