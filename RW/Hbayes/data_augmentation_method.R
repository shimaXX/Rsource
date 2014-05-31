#setwd("X:/RMM")
options(encoding="UTF-8")
rm(list=ls())
set.seed(555)
## 2項プロビットモデルのデータの発生

simbprobit = function(X, beta){
  y=ifelse((X%*%beta + rnorm(nrow(X))) < 0, 0, 1)
  list(X=X, y=y, beta=beta)
}

nobs <-200
X <- cbind(rep(1,nobs), runif(nobs), runif(nobs))
beta <- c(0, 1, -1)
nvar <- ncol(X)
simout <- simbprobit(X, beta)

X <- simout$X
y <- simout$y


## 2項プロビットモデルのベイズ推定
rtnorm <- function(mu, sigma, a, b){
  FA <- pnorm(a, mu, sigma)
  FB <- pnorm(b, mu, sigma)
  return(qnorm(runif(length(mu)) * (FB - FA) + FA, mu, sigma))
}

nobs <- length(y)
nvar <- ncol(X)
a <- ifelse(y==0, -100, 0)
b <- ifelse(y==0, 0, 100)

## 事前分布のパラメタ
A <- 0.01 * diag(nvar) ## AはB_0の逆数
b0 <- matrix(0, nrow=nvar, ncol=1)

XpX <- crossprod(X)

ndraw <- 11000

## 初期値の設定
beta <- matrix(0, nrow=nvar, ncol=1)
sigma <- 1.0
betadraw <- array(double(ndraw*nvar), dim=c(ndraw,nvar))

for(nd in 1:ndraw){
  old.beta <- beta
  ## zのサンプリング
  z <- rtnorm(X%*%beta, sigma, a, b)
  
  ## betaのサンプリング
  Xpz <- crossprod(X,z)
  IB <- solve(XpX + A)
  btilde <- IB %*% (Xpz + A%*% b0)
  beta <- btilde + chol(IB) %*% rnorm(nvar)
  betadraw[nd,] <- t(beta)
}

matplot(betadraw, type="l", main="beltadraw")
abline(a=0, b=0, col=1)
abline(a=1, b=0, col=2)
abline(a=-1, b=0, col=3)