setwd("D:/RW/stan/kubo_examples")
library(rstan)
#set_cppo("fast")
library(doSNOW)
library(foreach)
cl<-makeCluster(4)

# read data
d <- read.csv(url("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv"))

N<-nrow(d)
Y<-d$y

dat<-list(y=Y, N=N)

## parallel processing each chain of stan
# do not sampling. just create model.
fit0<-stan(file='kubo10.stan', data=dat, chains=0)
sflist<-foreach(i=1:4,.packages="rstan") %dopar% {
  stan(fit=fit0, data=dat, iter=1000, chains=1, chain_id=i, refresh=-1)
}
#sflist<-stan(file='kubo10.stan',data=dat,iter=1000,chains=4)

# merging the chains
f3<-sflist2stanfit(sflist)
stopCluster(cl)

#apply(extract(f3)$alpha,2,median)
plot(f3)
traceplot(f3)