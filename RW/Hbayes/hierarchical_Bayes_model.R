## Hb_Bin_Logit.R
options(encoding="UTF-8")
rm(list=ls())
setwd("C:/RW/Hbayes/")
library(bayesm)
set.seed(555)

nvar <- 1  ## ロジ�?トモ�?ルの説明変数の数
hh <-200   ## 個人数
nobs <- 10 ## 個人当たり�?�選択回数
nz <- 5    ## 個人属性の説明変数の数

## �K����Z�̒l�𔭐�
Zorg <- matrix(c(rep(1,hh), (as.integer(runif(hh)*30)),
              (as.integer(abs(rnorm(hh,3000,1000)/1000))),
              (as.integer(abs(rnorm(hh,400,100)/100))),
              (as.integer(abs(rnorm(hh,30,10)/10)))), hh, nz)

## Z�̒l���������֐�
standardization <- function(z){
  log(0.001 + (z - min(z))/(max(z) - min(z))*(0.999 - 0.001))
}

## Z�̊��
Z <- matrix(c(rep(1,hh), 
              standardization(Zorg[,2]),
              standardization(Zorg[,3]),
              standardization(Zorg[,4]),
              standardization(Zorg[,5])), hh, nz)
Delta <- matrix(as.integer(runif(nz*nvar)*10), nz, nvar)
iota <- matrix(1, nvar, 1)
Vbeta <- diag(nvar) + .5*iota%*%t(iota)

## シミュレーション�?ータの発�?
hhdata <- NULL

for(i in 1:hh){
  beta <- t(Delta)%*%Z[i,] + as.vector(t(chol(Vbeta)) %*% rnorm(nvar))
  X <- matrix(runif(nobs*nvar), nobs, nvar)
  prob <- exp(X%*%beta)/(1+exp(X%*%beta))
  unif <- runif(nobs, 0, 1)
  y <- ifelse(unif < prob, 1, 0)
  hhdata[[i]] <- list(y=y, X=X, beta=beta)
}

## 2�?ロジ�?トモ�?ルの対数尤度関数の定義
loglike <- function(y, X, beta){
  p1 <- exp(X %*% beta)/(1+exp(X %*% beta))
  ll <- y*log(p1) + (1 - y)*log(1-p1)
  sum(ll)
}

## ベイズ推定�?�ための設�?
R <- 12000
sbeta <- 0.2
keep <- 10

nhh <- length(hhdata)
nz <- ncol(Z)

nvar <- ncol(X)
## 事前�?�?のパラメータ
nu <- nvar + 3
V <- nu*diag(rep(1, nvar))
ADelta <- 0.01*diag(nz)
Deltabar <- matrix(rep(0, nz*nvar), nz, nvar)

## サンプリング結果の保存スペ�?�スの作�??
Vbetadraw <- matrix(double(floor(R/keep) * nvar * nvar),
                    floor(R/keep), nvar * nvar)
betadraw <- array(double(floor(R/keep) * nhh * nvar), 
                  dim = c(nhh, nvar, floor(R/keep)))
Deltadraw <- matrix(double(floor(R/keep) * nvar * nz), 
                    floor(R/keep), nvar * nz)

## 初期値の設�?
oldbetas <- matrix(double(nhh * nvar), nhh, nvar)
oldVbeta <- diag(nvar)
oldVbetai <- diag(nvar)
oldDelta <- matrix(double(nvar * nz), nz, nvar)
betad <- array(0, dim=c(nvar))
betan <- array(0, dim=c(nvar))

## �?却�?と対数尤度
reject <- array(0, dim=c(R/keep))
llike <- array(0, dim=c(R/keep))

## 階層ベイズ2�?ロジ�?トモ�?ルによる推�?
for(iter in 1:R){
  rej <- 0
  logl <- 0
  sV <- sbeta*oldVbeta
  root <- t(chol(sV))
  
  ## MH法による個人別betaのサンプリング
  for(i in 1:nhh){
    betad <- oldbetas[i, ]
    betan <- betad + root %*% rnorm(nvar)
    lognew <- loglike(hhdata[[i]]$y, hhdata[[i]]$X,betan)
    logold <- loglike(hhdata[[i]]$y, hhdata[[i]]$X,betad)
    logknew <- -0.5*(t(betan) - Z[i, ] %*% oldDelta) %*% 
      oldVbetai %*% (betan - t(Z[i, ] %*% oldDelta))
    logkold <- -0.5*(t(betad) - Z[i, ] %*% oldDelta) %*% 
      oldVbetai %*% (betad - t(Z[i, ] %*% oldDelta))
    alpha <- exp(lognew + logknew - logold - logkold)
    if(alpha == "NaN") alpha = -1
    u <- runif(1)
    if(u < alpha){
      oldbetas[i, ] <- betan
      logl <- logl + lognew
    }else{
      logl <- logl + logold
      rej <- rej +1
    }
  }
  
  ## 多変量回帰によるDeltaのギブスサンプリング?�?bayesmのrmultiregを利用?�?
  out <- rmultireg(oldbetas, Z, Deltabar, ADelta, nu, V)
  oldDelta <- out$B
  oldVbeta <- out$Sigma
  oldVbetai <- solve(oldVbeta)
  
  ## 現在までのサンプリング数の表示
  if((iter%%100) == 0){
    cat("繰り返し数", iter, fill = TRUE)
  }
  
  ## keep回ごとにサンプリング結果を保�?
  mkeep <- iter/keep
  if(iter%%keep == 0){
    Deltadraw[mkeep, ] <- as.vector(oldDelta)
    Vbetadraw[mkeep, ] <- as.vector(oldVbeta)
    betadraw[, , mkeep] <- oldbetas
    llike[mkeep] <- logl
    reject[mkeep] <- rej/nhh
  }
}


## log-likelihoodのプロ�?�?
plot(llike, type="l", xlab="Iterations/10", ylab="", main="Log Likelihood")

## rejectのプロ�?�?
plot(reject, type="l", xlab="Iterations/10", ylab="",
     main="Rejection Rate of MH Algorithm")

## 平�?パラメータのプロ�?�?
Deltas <- as.vector(Delta)
matplot(Deltadraw, type="l", xlab="Iteratins/10",
        ylab="", main="Draw of Delta")

## burn-in 期間
R0 <- floor(2000/10) + 1
R1 <- R/10

## Deltaの統計値
apply(Deltadraw[R0:R1, ], 2, mean)
rbind(Deltas, apply(Deltadraw[R0:R1, ],
                    2, quantile, probs=c(0.025, 0.5, 0.975)))