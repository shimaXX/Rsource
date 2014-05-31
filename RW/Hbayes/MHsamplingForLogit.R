## MHlogitmodel.r ####
options(encoding="UTF-8")
rm(list=ls())
setwd("C:/RW")
#install.packages("bayesm")
#install.packages("mlogit")
library(bayesm)
library(mlogit)

set.seed(555)
##第5章のデータと結果を利用
load("logitdata.Rdata")
BUY <- logitdata$BUY
PRICE <- logitdata$PRICE
DISP <- logitdata$DISP
betas0 <- logitdata$betas

load("res-logit.Rdata")
Hessian <- res$hessian
invH <- solve(-Hessian)

loglike <- function(betas, X1, X2, Y){
  b1 <- betas[1]
  b2 <- betas[2]
  b01 <- betas[3]
  b02 <- betas[4]
  b03 <- betas[5]
  
  U1 <- b1*X1[,1] + b2 * X2[,1] + b01
  U2 <- b1*X1[,2] + b2 * X2[,2] + b02
  U3 <- b1*X1[,3] + b2 * X2[,3] + b03
  U4 <- b1*X1[,4] + b2 * X2[,4]
  
  d <- exp(U1) + exp(U2) + exp(U3) + exp(U4)
  LLL <- Y[,1]*U1 + Y[,2]*U2 + Y[,3]*U3 + Y[,4]*U4 - log(d)
  LL <-sum(LLL)
  return(LL)
}

nvar <- 5

## prior
beta0 <- rep(0,nvar)
B0 <- 0.01*diag(nvar)

sbeta <- 0.2
root <- t(chol(sbeta*invH))
rootBi <- t(chol(B0))

## 初期値の設定
oldbetas <- rep(0, nvar)

R <- 12000
keep <- 10

betadraw <- array(0,c(floor(R/keep), nvar))
rej <- 0

for(nd in 1:R){
  #betaのサンプリング
  betad <- oldbetas
  betan <- betad + root %*% rnorm(nvar)
  
  #対数尤度の計算
  lognew <- loglike(betan, PRICE, DISP, BUY)
  logold <- loglike(betad, PRICE, DISP, BUY)
  logknew <- lndMvn(betan, beta0, rootBi)
  logkold <- lndMvn(betad, beta0, rootBi)
  
  #MHステップ
  alpha <- min(1, exp(lognew + logknew - logold - logkold))
  if(alpha == "NaN") alpha <- -1
  u <- runif(1)
  if(u < alpha){
    oldbetas <- betan
    logl <- lognew
  } else {
    logl <- logold
    rej <- rej + 1
  }
  if(nd%%keep==0){
    mkeep <- nd/keep
    betadraw[mkeep,] <- oldbetas
  }  
}

ns <- nrow(betadraw)
bmean <- apply(betadraw[(floor(ns/2)+1):ns, ], 2, mean)
bQB <- apply(betadraw[(floor(ns/2)+1):ns, ], 2, quantile,
             probs = c(0.025, 0.5, 0.975))
matplot(betadraw, type="l", xlab="iterations/10")
for(i in 1:nvar){
  abline(a=betas0[i], b=0, col=i)
}