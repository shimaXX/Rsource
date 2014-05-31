## step3：θ_i_sysの発生
options(encoding="UTF-8")
rm(list=ls())
setwd("C:/RW")
library(bayesm)
set.seed(555)

##--定数の定義-------
hh <-200            ## 人間の数
SEASONALYTY <- 7    ## 季節変動の周期
nz <- 3             ## 説明変数Zの次元
nsita <- 2+nz       ## θ_i_sysの次元（トレンド+季節変動+βの次元(=Zの次元)）
R <- 3000           ## サンプリング回数
##-------------------


## 逆ガンマ分布のサンプリング関数------
irgamma <- function(n, shp, rt){
  return(1/rgamma(n, shape=shp, rate=rt))
}
## ------------------------------------

## 事後分布サンプリングに必要な値の算出
dift <- rep(0,hh)
difw <- rep(0,hh)
sw <- rep(0,hh)
difbeta <- array(rep(0,nz*hh), dim=c(nz, hh))

for(tm in 2:T){
  dift <- dift + (t[,tm] - t[,tm-1])^2
  
  ## 季節周期の取り扱いよくわからん（下のやり方だと次元が変わってまう）
  if(tm >= SEASONALY - 1){
    for(trm in 0:SEASONALY-1){
      sw <- sw + w[,tm - trm]
    }
  }
  difw <- difw + sw^2
  
  for(d in 1:nz){
    difbeta[d,] <- difbeta[d,] + (beta[d,,tm] - beta[d,,tm-1])^2
  }
}

#--行列計算で書き換える
#t <- rnorm(1000, 10, 20)
#t_t <- t
#t_t_1 <- t[-length(t)]
#t_t_1 <- append(t_t_1, 0, after=0)

#dift <- t_t - t_t_1
#sdift2 <- t(dift)%*%dift

## ------------------------------------

## 事前分布の設定-----------------
mu0 <- 0; kaps0 <- 25;
nu0 <- 0.02; s0 <- 0.02
##---------------------------------

## パラメータ初期値の設定-----------
mu <- 0
sigs <- 1
##----------------------------------


## 事後分布からのサンプリング----
mX <- 0
betadraw <- matrix(0, nrow=R, ncol=1)

for(rep in 1:R){
  ## tのsigma2のサンプリング
  nu1 <- nu0 + T
  s1t <- s0 + dift
  sigst <- 1/(rgamma(n=1, shape=nu1/2, scale = 2/s1t))
  
  ## wのsigma2のサンプリング
  nu1 <- nu0 + T
  s1w <- s0 + difw
  sigsw <- 1/(rgamma(n=1, shape=nu1/2, scale = 2/s1w))
  
  
  ## サンプリング結果の保存
  betadraw[rep] <- sigs
}


tau2t_t <- irgamma(1, (0.1+T)/2, (0.01+dift)/2)
tau2w_t <- irgamma(1, (0.1+T)/2, (0.01+difw)/2)
tau2beta_t <- irgamma(1, (0.1+T)/2, (0.01+difbeta)/2)
## ---------------------




Z <- matrix(c(rep(1,hh), runif(hh, min=-1, max=1)), hh, nz)
Delta <- matrix(as.integer(runif(nz*nvar)*10), nz, nvar)
iota <- matrix(1, nvar, 1)
Vbeta <- diag(nvar) + .5*iota%*%t(iota)

## シミュレーションデータの発生
hhdata <- NULL

for(i in 1:hh){
  beta <- t(Delta)%*%Z[i,] + as.vector(t(chol(Vbeta)) %*% rnorm(nvar))
  X <- matrix(runif(nobs*nvar), nobs, nvar)
  prob <- exp(X%*%beta)/(1+exp(X%*%beta))
  unif <- runif(nobs, 0, 1)
  y <- ifelse(unif < prob, 1, 0)
  hhdata[[i]] <- list(y=y, X=X, beta=beta)
}

## 2項ロジットモデルの対数尤度関数の定義
loglike <- function(y, X, beta){
  p1 <- exp(X %*% beta)/(1+exp(X %*% beta))
  ll <- y*log(p1) + (1 - y)*log(1-p1)
  sum(ll)
}

## ベイズ推定のための設定
R <- 12000
sbeta <- 0.2
keep <- 10

nhh <- length(hhdata)
nz <- ncol(Z)

nvar <- ncol(X)
## 事前分布のパラメータ
nu <- nvar + 3
V <- nu*diag(rep(1, nvar))
ADelta <- 0.01*diag(nz)
Deltabar <- matrix(rep(0, nz*nvar), nz, nvar)

## サンプリング結果の保存スペースの作成
Vbetadraw <- matrix(double(floor(R/keep) * nvar * nvar),
                    floor(R/keep), nvar * nvar)
betadraw <- array(double(floor(R/keep) * nhh * nvar), 
                  dim = c(nhh, nvar, floor(R/keep)))
Deltadraw <- matrix(double(floor(R/keep) * nvar * nz), 
                    floor(R/keep), nvar * nz)

## 初期値の設定
oldbetas <- matrix(double(nhh * nvar), nhh, nvar)
oldVbeta <- diag(nvar)
oldVbetai <- diag(nvar)
oldDelta <- matrix(double(nvar * nz), nz, nvar)
betad <- array(0, dim=c(nvar))
betan <- array(0, dim=c(nvar))