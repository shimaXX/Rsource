## logit_model.R
options(encoding="UTF-8")
rm(list=ls())
setwd("C:/RW")
load("logitdata.Rdata")
hh <- logitdata$hh
pt<-  logitdata$pt
PRICE <- logitdata$PRICE
DISP <- logitdata$DISP
BUY <- logitdata$BUY

## Logit model の対数尤度関数の定義
fr <- function(x, BUY ,PRICE, DISP){
  b1 <- x[1]
  b2 <- x[2]
  b01 <- x[3]
  b02 <- x[4]
  b03 <- x[5]
  
  U1 <- b1*PRICE[,1] + b2*DISP[,1] + b01
  U2 <- b1*PRICE[,2] + b2*DISP[,2] + b02
  U3 <- b1*PRICE[,3] + b2*DISP[,3] + b03
  U4 <- b1*PRICE[,4] + b2*DISP[,4]
  
  d <- exp(U1) + exp(U2) + exp(U3) + exp(U4)
  
  LLI <- BUY[,1]*U1 + BUY[,2]*U2 + BUY[,3]*U3 + BUY[,4]*U4 - log(d)
  
  LL <- sum(LLI)
  return(LL)
}

b0 <- c(0,0,0,0,0) #パラメータ初期値の設定

### 対数尤度関数frの最大化
res <- optim(b0, fr, gr=NULL, BUY, PRICE, DISP, method = "BFGS",
             hessian = TRUE, control=list(fnscale=-1))

### パラメータ推定
b <- res$par
### t値と情報量基準の計算
tval <- b/sqrt(-diag(solve(res$hessian)))
AIC <- -2*res$value + 3*length(res$par)
BIC <- -2*res$value + log(nrow(BUY))*length(res$par)