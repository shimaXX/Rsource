## マーケティングモデルの断片
options(encoding="UTF-8")
rm(list=ls())

##--定数の定義-------
hh <-200
SEASONALYTY <- 7
##-------------------

## 前回来店からの日数の対数,イベントの有無（ダミー変数）------
## 値引きの有無（ダミー変数）
Zorg <- cbind(as.integer(runif(hh)*30),
           ifelse(runif(hh)>0.5,1,0), ifelse(runif(hh)>0.5,0,1))

## Zの値を基準化する関数
standardization <- function(z){
  log(0.001 + (z - min(z))/(max(z) - min(z))*(0.999 - 0.001))
}

tmp <- c(1, 1, rep(0,SEASONALYTY-2))
Ztld <- cbind(t(matrix(tmp, length(tmp), hh)), Z)
##------------------------------------------------------------


##-- 2項プロビットモデルのベイズ推定
rtnorm <- function(mu, sigma, a, b){
  FA <- pnorm(a, mu, sigma)
  FB <- pnorm(b, mu, sigma)
  print(FA)
  print(FB)
  return(qnorm(runif(length(mu)) * (FB - FA) + FA, mu, sigma))
}
##-----------------------------------


##-- 計算に必要なデータの定義
nobs <- nrow(Ztld)
nvar <- ncol(Ztld)
##-----------------------------------


## 事前分布のパラメタ-----------------
A <- 0.01 * diag(nvar) ## AはB_0の逆数
b0 <- matrix(0, nrow=nvar, ncol=1)  ## カルマンフィルタで算出し、渡す必要あるかも
ZpZ <- crossprod(Ztld)
ndraw <- 11000
##------------------------------------


## 初期値の設定------------------------------
beta <- matrix(0, nrow=nvar, ncol=1)
sigma <- 1.0
u <- runif(nobs, min=-1, max=1)

## 切断範囲の指定
a <- ifelse(u<0, -100, 0)
b <- ifelse(u<0, 0, 100)
betadraw <- array(double(ndraw*nvar), dim=c(ndraw,nvar)) #描画用のマトリクス


for(nd in 1:6){
  old.beta <- beta
  
  ## uのサンプリング
  u <- rtnorm(Ztld%*%beta, sigma, a, b)
  
  ## betaのサンプリング
  Zpu <- crossprod(Ztld,u)
  IB <- solve(ZpZ + A)
  btilde <- IB %*% (Zpu + A%*% b0)
  beta <- btilde + chol(IB) %*% rnorm(nvar)
  betadraw[nd,] <- t(beta)
}
##-------------------------------------------
