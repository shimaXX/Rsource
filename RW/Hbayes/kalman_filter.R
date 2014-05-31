# 時系列データの時変平均の推定（2次階差の平滑化事前分布を使用）
options(encoding="UTF-8")
rm(list=ls())

# データ数 N を求め，データを y に渡す
N <- nrow(data)
y <- data[,1]
# カルマンフィルタの関数 ------------------------------------------
KF <- function(y, XF0, VF0, F, H, G, Q, R, limy, ISW, OSW, m, N)  
{
  if (OSW == 1)
  {
    XPS <- matrix(0, m, N); XFS <- matrix(0, m, N)
    VPS <- array(dim = c(m, m, N)); VFS <- array(dim = c(m, m, N))
  }
  XF <- XF0; VF <- VF0; NSUM <- 0; SIG2 <- 0; LDET <- 0
  for (n in 1:N)
  {
    # 1期先予測
    XP <- F %*% XF
    VP <- F %*% VF %*% t(F) +  G %*% Q %*% t(G)
    # フィルタ
    if (y[n] < limy) 
    {
      NSUM <- NSUM + 1
      B <- H %*% VP %*% t(H) + R
      B1 <- solve(B)
      K <- VP %*% t(H) %*% B1
      e <- y[n] - H %*% XP
      XF <- XP + K %*% e
      VF <- VP - K %*% H %*% VP
      SIG2 <- SIG2 + t(e) %*% B1 %*% e
      LDET <- LDET + log(det(B))
    }
    else
    {
      XF <- XP; VF <- VP
    }
    if (OSW == 1)
    {
      XPS[,n] <- XP; XFS[,n] <- XF; VPS[,,n] <- VP; VFS[,,n] <- VF
    }   
  }
  SIG2 <- SIG2 / NSUM
  if (ISW == 0)
  {
    FF <- -0.5 * (NSUM * (log(2 * pi * SIG2) + 1) + LDET)
  }
  else
  {
    FF <- -0.5 * (NSUM * (log(2 * pi) + SIG2) + LDET)
  }
  if (OSW == 0)
  {
    return(list(LLF=FF, Ovar=SIG2))
  }
  if (OSW == 1)
  {
    return(list(XPS=XPS, XFS=XFS, VPS=VPS, VFS=VFS, LLF=FF, Ovar=SIG2))
  }
}
# 平滑化の関数 ----------------------------------------------------
SMO1 <- function(XPS, XFS, VPS, VFS, F, GSIG2, m, q, N)
{
  XSS <- matrix(0, m, N); VSS <- array(dim = c(m, m, N))
  XS1 <- XFS[,N]; VS1 <- VFS[,,N]
  XSS[,N] <- XS1; VSS[,,N] <- VS1
  for (n1 in 1:(N-1))
  {
    n <- N - n1; XP <- XPS[,n+1]; XF <- XFS[,n]
    VP <- VPS[,,n+1]; VF <- VFS[,,n]; VPI <- solve(VP)
    A <- VF %*% t(F) %*% VPI
    XS2 <- XF + A %*% (XS1 - XP)
    VS2 <- VF + A %*% (VS1 - VP) %*% t(A)
    XS1 <- XS2; VS1 <- VS2
    XSS[,n] <- XS1; VSS[,,n] <- VS1
  }
  arc <- matrix(0, q, N); arv <- array(dim = c(q, q, N))
  for (n in 1:N)
  {
    arc[,n] <- XSS[1:q,n]; arv[,,n] <- GSIG2 * VSS[1:q,1:q,n]
  }
  return(list(arc=arc, arv=arv))
}
# TAU2の対数尤度関数の定義 ----------------------------------------
LogL <- function(theta, y, F, H, G, R, limy, ISW, k, m, N)  
{
  TAU2 <- theta; Q <- TAU2 * diag(m)
  XF0 <- numeric(k); VF0 <- 10 * diag(k); OSW <- 0
  LLF <- KF(y, XF0, VF0, F, H, G, Q, R, limy, ISW, OSW, k, N)
  LL <- LLF$LLF
  return(LL)
}
# カルマンフィルタ計算のための行列およびパラメータの設定-----------
limy <- 1e20; ISW <- 0; L <- 1; k <- 2; m <- 1
F <- matrix(0, k, k); G <- matrix(0, k, m)
H <- matrix(0, L, k); R <- diag(L)
F[1,1] <- 2; F[1,2] <- -1; F[2,1] <- 1
G[1,1] <- 1; H[1,1] <- 1
# TAU2の最尤推定を求める数値計算-----------------------------------
LLF1 <- optimize(LogL, lower=0.0005, upper=0.1, maximum=TRUE, y=y, 
                 F=F, H=H, G=G, R=R, limy=limy, ISW=ISW, k=k, m=m, N=N)
# TAU2の最尤推定
TAU2 <- LLF1$maximum; MLL <- LLF1$objective
# カルマンフィルタ-------------------------------------------------
Q <- TAU2 * diag(m); XF0 <- numeric(k)
VF0 <- 10 * diag(k); OSW <- 1
LLF2 <- KF(y, XF0, VF0, F, H, G, Q, R, limy, ISW, OSW, k, N)
XPS <- LLF2$XPS; XFS <- LLF2$XFS
VPS <- LLF2$VPS; VFS <- LLF2$VFS
SIG2 <- LLF2$Ovar; GSIG2 <- 1
# 平滑化 ----------------------------------------------------------
LLF3 <- SMO1(XPS, XFS, VPS, VFS, F, GSIG2, k, 1, N)
sx <- LLF3$arc
# AICの計算と主要な結果の出力--------------------------------------
TAU2 <- TAU2 * SIG2
AIC <- -2 * MLL + 2 * 2

#===================================================================