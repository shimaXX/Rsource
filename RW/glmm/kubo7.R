setwd("D:/RW/stan/kubo_examples")
#install.packages("glmmML")
library(glmmML)

# read data
d <- read.csv(url("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv"))
N<-rep(8,nrow(d))
x<-rep(2:6,each=20) # iterate 20times
data <- data.frame(N=N, id=d$id, y=d$y, x=x)

res <- glmmML(cbind(y, N-y)~x, data=data, family=binomial, cluster=id,method="ghq")

logistic <- function(z){ 1/(1+exp(-z))}
dd <- data.frame(i=rep(1,length(data$x)),x=data$x)
q <- as.matrix(dd)%*%matrix(res$coefficients,2,1)
logit_q <- logistic(q)
(pred <- logit_q*N)