## �}�[�P�e�B���O���f���̒f��
options(encoding="UTF-8")
rm(list=ls())
setwd("C:/RW")

random.seed(555)

#install.packages("mvtnorm")
#install.packages("MCMCpack")
#install.packages("FKF")
#install.packages("sspir")

### package�ǂݍ���
library(bayesm)
library(mvtnorm)
library(MCMCpack)
library(FKF)
#--------------------

##--�萔�̒�`-------
TIMES <- 500
nhh <-15
SEASONALYTY <- 7 #�G�ߕϓ��̎���
RP <- 11000  # �T���v�����O��
keep <- 100
nz <- 3
nD <- 5
zc <- 1+1+ SEASONALYTY-2 + nz
m <- 1 + 1 + nz
nvar <- 1 # �l�����ŋ��߂�Ώۂ̐��i���N�H�i�Ƃ���ȊO�̓��j�i�Ƃ�����2�j
limy <- 1e20 # �����Ƃ݂Ȃ����l�̋��E
k <- 1 # �g�����h�������f���̎���
##-------------------

### data�����v���Z�X
## �O�񗈓X����̓����̑ΐ�,�C�x���g�̗L���i�_�~�[�ϐ��j------
## �l�����̗L���i�_�~�[�ϐ��j
Z <- NULL

# ���n��p�����^����
tmp <- c(1, 1, rep(0,SEASONALYTY-2))

# Z�S�̂Ƀ}�[�W
for(t in 1:TIMES){
  Z[[t]] <- list(
          Z0 = t(matrix(tmp, length(tmp), nhh)),
          Z1 = as.integer(runif(nhh)*30),
          Z2 = rep(ifelse(runif(1)>0.5,1,0), nhh),
          Z3 = rep(ifelse(runif(1)>0.5,0,1), nhh))
}

## �ڋq���ʕ�����data
D <- matrix(
    c(
      rep(1,nhh),
      log( rnorm(nhh, 7, 2) ),
      log( rnorm(nhh, 3000, 800)/1000 ),
      log( rnorm(nhh, 400, 100)/100 ),
      log( rnorm(nhh, 30, 5)/10 )
    ),
    nhh, nD
  )
##------------------------------------------------------------

### �֐��̒�`------------------------------------
# 2���v���r�b�g���f���̃x�C�Y����(step1�p)
rtnorm <- function(mu, sigma, a, b){
  FA <- pnorm(a, mu, sigma)
  FB <- pnorm(b, mu, sigma)
  result <- qnorm(runif(length(mu)) * (FB - FA) + FA, mu, sigma)
  if(result == -Inf ) {result <- -100}
  if(result == Inf ) {result <- 100}
  return(result)
}

# Z�̒l���������֐�(step1�p)
standardization <- function(z){
  return(log(0.001 + (z - min(z))/(max(z) - min(z))*(0.999 - 0.001)))
}

# �t�K���}���z�̃T���v�����O�֐�(step3�p)
irgamma <- function(n, shp, rt){
  return(1/rgamma(n, shape=shp, scale=1/rt))
}

# �P�ϗʐ��K���z�̃J�[�l�������̏搔�̕����̌v�Z(step4�p)
Nkernel <- function(sita, H, D, Vsita){
  return( ((sita - H%*%D)^2)/Vsita )
}

# ���ϗʐ��K���z�̃J�[�l�������̏搔�̕����̌v�Z(step4�p)
NMkernel <- function(sita, H, D, Vsita){
  return(
    t(sita - t(H)%*%D) %*% solve(Vsita) %*% (sita - t(H)%*%D)
  )
}

### �G�ߒ������f���̏�ԋ�ԕ\���̍s��ݒ�
FGHset <- function(al, k, p, q, nz){  #al��AR���f���̃��x�N�g���Ak;p;q�̓g�����h�A�G�߁AAR
  m <- k + p + q + nz -1
  if(q>0){G <- matrix(0,m,3+nz)} # ��ԃ��f���Ńg�����h�A�G�߁AAR��3���܂ޏꍇ
  else{G <- matrix(0,m,2+nz)}    #AR�������܂܂Ȃ��ꍇ(q=0)
  F <- matrix(0,m,m)
  #H <- matrix(0,1,m)          #H�̑����Ztld���g���̂ŁAH�͕s�v
  
  ## �g�����h���f���̃u���b�N�s��̍\�z
  G[1,1] <- 1
  #H[1,1] <- 1
  if(k==1) {F[1,1] <- 1}
  if(k==2) {F[1,1] <- 2; F[1,2] <- -1; F[2,1] <- 1}
  if(k==3) {F[1,1] <- 3; F[1,2] <- -3; F[1,3] <- 1
            F[2,1] <- 1; F[3,2] <- 1}
  LS <- k
  NS <- 2;
  
  ## �G�ߒ��������̃u���b�N�s��̍\�z
  G[LS+1, NS] <- 1
  #H[1,LS+1] <- 1
  for(i in 1:(p-1)) {F[LS+1, LS+i] <- -1}
  for(i in 1:(p-2)) {F[LS+i+1, LS+i] <- 1}

  ## Z�����̃u���b�N�s��̍\�z
  LS <- LS + p -1
  NS <- 2
  for(i in 1:(nz)) {F[LS+i, LS+i] <- 1}
  for(i in 1:(nz)) {G[LS+i, NS+i] <- 1}
  
  if(q>0){
    NS <- NS +1
    G[LS+1, NS] <- 1
    #H[1,LS+1] <- 1
    for(i in 1:q) {F[LS+1, LS+i] <- al[i]}
    if(q>1) {
      for(i in 1:(q-1)) {F[LS+i+1, LS+i] <- 1}
    }
  }
  
  ## �V�X�����f���̕��U�����U�s��Q�̘g�̎Z�o
  Q <- diag(NS+nz)
  
  return(list(m=m, MatF=F, MatG=G, MatQ=Q))
}

# ��ԋ�ԕ\���ɂ�����s��Q�̐ݒ�------------------------
Qset <- function(Q0 ,parm){
  NS <- ncol(Q0)
  Q <- Q0
  # �V�X�����f���̕��U�����U�s��Q�̘g�̎Z�o
  for(i in 1:NS) {
    Q[i,i] <- parm[i]
  }
  return(Q)
}

# �J���}���t�B���^�̊֐� ------------------------------------------
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
    # 1����\��
    XP <- F %*% XF
    VP <- F %*% VF %*% t(F) +  G %*% Q %*% t(G)
    # �t�B���^
    if (y[n] < limy) 
    {
      NSUM <- NSUM + 1
      B <- matrix(H[,n],1,) %*% VP %*% t(matrix(H[,n],1,)) + R  # H�͐��w�I�ɂ͉��x�N�g���Ȃ̂ŉ�]������
      B1 <- solve(B)
      K <- VP %*% t(matrix(H[,n],1,)) %*% B1
      e <- y[n] - matrix(H[,n],1,) %*% XP
      XF <- XP + K %*% e
      VF <- VP - K %*% matrix(H[,n],1,) %*% VP
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

# �������̊֐� ----------------------------------------------------
SMO <- function(XPS, XFS, VPS, VFS, F, GSIG2, k, p, q, m, N)
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
  return(list(XSS=XSS, VSS=VSS))
}

# TAU2x�̑ΐ��ޓx�֐��̒�` ----------------------------------------
LogL <- function(parm, y, F, H, G, R, limy, ISW, k, m, N, Q0,...) 
{
  Q <- Qset(Q0 ,parm)
  XF0 <- numeric(k); VF0 <- 10 * diag(k); OSW <- 0
  LLF <- KF(y, XF0, VF0, F, H, G, Q, R, limy, ISW, OSW, k, N)
  LL <- LLF$LLF
  return(LL)
}
# -------------------------------------------------------
##-----------------------------------


## ���O���z�̃p�����^-----------------
# step1�F���݌��p�T���v�����O�p
A <- 0.01 * diag(zc) ## A��B_0�̋t��
b0 <- matrix(0, nrow=zc, ncol=1)  ## �J���}���t�B���^�ŎZ�o���A�n���K�v���邩��

# step3�F�V�X�e���m�C�Y�̕��U�T���v�����O�p
mu0 <- 0; kaps0 <- 25;
nu0 <- 0.02; s0 <- 0.02

# step5�F����҈َ����̉�A�p�����^H�̃f�[�^�g
m0 <- matrix(rep(0,nD*nvar), nD, nvar)
A0 <- 0.01*diag(nD)            #����{�ł�A

# step6�F����҈َ����̉�A�p�����^V�̃f�[�^�g
f0 <- nvar�@+3
V0 <- f0 * diag(nvar)
##------------------------------------


##��R�̃��[�v�̒����Ɏd���܂Ȃ��Ƒʖڂł́H
## ���㕪�z�T���v�����O�ɕK�v�ȃf�[�^�̘g���쐬-----------
# step1�F���݌��p�T���v�����O�̃f�[�^�g 
ZpZ <- array(double(zc*zc*TIMES),dim=c(zc, zc, TIMES))
Ztld <- array(double(nhh*zc*TIMES),dim=c(nhh, zc, TIMES))
for(t in 1:TIMES){
  Ztld[,,t] <- cbind(Z[[t]]$Z0, Z[[t]]$Z1, Z[[t]]$Z2, Z[[t]]$Z3)
  ZpZ[,,t] <- crossprod(Ztld[,,t])
}
u <- array(runif(nhh*TIMES, min=-1, max=1), dim=c(nhh, TIMES))
Zpu <- matrix(double(zc*TIMES), zc, TIMES)

# step2:��ԃx�N�g���̎Z�o�̃f�[�^�g
# �����̓s����A�����l�̐ݒ蕔���ŏ���

# step3�F�V�X�e���m�C�Y�̕��U�T���v�����O�̃f�[�^�g
Sita.sys <- array(rep(10*diag(m),nhh), dim=c(m,m,nhh)) 

# step4�F�[���ƒ���݌ɂ��K�肷�邽�߂̃p�����^�T���v�����O�̂��߂̃f�[�^�g
# �Ƃ�2�ϐ��������̓x�N�g���ł͂Ȃ�matrix�ɂ��邱��
Lsita.dlt <- rep(0,nhh)
Lsita.lmbd <- rep(0,nhh)
Hdlt <- matrix(rep(0,nD),nvar,)
Hlmbd <- matrix(rep(0,nD),nvar,)
Vsita.dlt <- 0.01*diag(nvar)
Vsita.lmbd <- 0.01*diag(nvar)
Sita.dlt <- rep(0,nhh)
Sita.lmbd <- rep(0,nhh) 
sigma.dlt <- 0.01*diag(nvar)
sigma.lmbd <- 0.01*diag(nvar)
rej.dlt <-rep(0,nhh)
rej.lmbd <-rep(0,nhh)
##---------------------------------------------------------


## �����l�̐ݒ�------------------------------
# step1�p
Xs <- array(double(nhh*zc*TIMES), dim=c(nhh, zc, TIMES))
sigma <- 1.0
# ut��at,bt�𔭐������邽�߂ɍ�����[���f�[�^.�{���͕K�v�Ȃ�
ut <- array(runif(nhh*TIMES, min=-1, max=1), dim=c(nhh, TIMES))

# step2�p
param <- FGHset(0, 1, SEASONALYTY, 0, nz)
L <- 1
R <- diag(L)
F <- param$MatF
G <- param$MatG
 #�V�X�e�����f���̕��U���l���ƂɊi�[����g
 Q0 <-param$MatQ %o% rep(1,nhh)
 Q <- Q0

# step3�p
mu <- 0
sigs <- 1
##-------------------------------------------

## �ؒf�͈͂̎w��
at <- ifelse(ut<0, -100, 0)
bt <- ifelse(ut<0, 0, 100)


##-------------------
udraw <- array(double(nhh*TIMES*RP),dim=c(nhh,TIMES,RP))

## �T���v�����O�̃��[�v
for(nd in 1:RP){
  for(hh in 1:nhh){
    # step3�̊K���v�Z�̘a�̌v�Z�Ŏg�p����ϐ��̏�����
    dift <- 0
    difw <- 0
    difbeta <- rep(0,nz)
    
    # step4�̃Ƃ̎��㕪�z�J�[�l���̑�ꍀ�̘a�v�Z���g�p����ϐ��̏�����
    Lsita <- 0
    
    for(t in 1:TIMES){
      # step1--------------------------------------------------
      u[hh,t] <- rtnorm(Ztld[hh,,t]%*%Xs[hh,,t], sigma, at[hh,t], bt[hh,t])
      
      udraw[hh,t,nd] <- u[hh,t]
      
      # beta�̃T���v�����O
      Zpu[,t] <- crossprod(t(Ztld[hh,,t]),u[hh,t])
      IB <- solve(ZpZ[,,t] + A)
      btilde <- IB %*% (Zpu[,t] + A%*% b0)
      Xs[hh,,t] <- btilde + chol(IB) %*% rnorm(zc)

      #------------------------------------------------------------
    }
    
    ## step2�̃V�X�e�����f���p�����[�^�̌v�Z----------------------    
    # TAU2�̍Ŗސ�������߂鐔�l�v�Z------------------------------
    ISW <- 0
    tau0 <- c(TAU21=Sita.sys[1,1,hh],TAU22=Sita.sys[2,2,hh],
              TAU23=Sita.sys[3,3,hh],TAU24=Sita.sys[4,4,hh],
              TAU25=Sita.sys[5,5,hh])
    LLF1 <- optim(tau0, fn=LogL, y=u[hh,], F=F, H=Ztld[hh,,], G=G, R=R,
                  limy=limy, ISW=ISW, k=zc, m=m , N=TIMES , Q0=Q0[,,hh],
                  method ="L-BFGS-B",
                  lower = 1e-4, upper = 1e2,
                  control=list(fnscale=-1))
    # TAU2�̍Ŗސ���
    TAU2 <- LLF1$par
    
    # �J���}���t�B���^
    Q <- Qset(Q0[,,hh] ,TAU2); XF0 <- numeric(zc)
    VF0 <- 10 * diag(zc); OSW <- 1
    LLF2 <- KF(u[hh,], XF0, VF0, F, Ztld[hh,,], G, Q, R, limy, ISW, OSW, zc, TIMES)
    XPS <- LLF2$XPS; XFS <- LLF2$XFS
    VPS <- LLF2$VPS; VFS <- LLF2$VFS
    SIG2 <- LLF2$Ovar; GSIG2 <- 1
    # ������ ----------------------------------------------------------
    LLF3 <- SMO(XPS, XFS, VPS, VFS, F, GSIG2, 1, SEASONALYTY, 1, zc, TIMES)
    Xs[hh,,] <- LLF3$XSS
    #------------------------------------------------------------
    
    for(t in 1:TIMES){  
      # step3�̊K���̌v�Z--------------------------------------
      if(t>1){
        dift <- dift + (Xs[hh,1,t] - Xs[hh,1,t-1])^2
        difw <- difw + (Xs[hh, 2, t]+sum(Xs[hh, 2:SEASONALYTY, t-1]))^2
        
        for(d in 1:nz){
          difbeta[d] <- difbeta[d] + (Xs[hh,SEASONALYTY+d,t] 
                                        - Xs[hh,SEASONALYTY+d,t-1])^2
        }
      }
      #--------------------------------------------------------
      
      # step4�̌��p�l�̌덷�v�Z(step4�̃Ƃ̖ޓx�v�Z)------------
      Lsita <- Lsita + (u[hh,t] - t(Ztld[hh,,t])%*%Xs[hh,,t])^2
      
      #--------------------------------------------------------
    }
    
    # step3--------------------------------------
    Sita.sys[1,1,hh] <- irgamma(1, (nu0+TIMES)/2, (s0+dift)/2)
    Sita.sys[2,2,hh] <- irgamma(1, (nu0+TIMES)/2, (s0+difw)/2)
    for(d in 1:nz){
      Sita.sys[2+d,2+d,hh] <- irgamma(1, (nu0+TIMES)/2, (s0+difbeta[d])/2)
    }
    #--------------------------------------------
    
    ### step4--------------------------------------
    ## dlt���̌v�Z
    # ����̃Ƃ��m�ۂ���
    old.sita.dlt <- Sita.dlt[hh]
    # �V�����Ƃ��T���v�����O�i�����T���v�����O�j
    new.sita.dlt <- Sita.dlt[hh] + rnorm(1, 0, sigma.dlt)

    # �ޓx�̌v�Z�i�ΐ��ޓx�̏ꍇ�̓��R�r�A���Œ����j
    new.Lsita.dlt <- Lsita + Nkernel(new.sita.dlt, Hdlt, D[hh,], Vsita.dlt)
    new.Lsita.dlt <- exp(-0.5*new.Lsita.dlt)
    old.Lsita.dlt <- Lsita + Nkernel(old.sita.dlt, Hdlt, D[hh,], Vsita.dlt)
    old.Lsita.dlt <- exp(-0.5*old.Lsita.dlt)
    
    # MH�X�e�b�v
    alpha <- min(1, new.Lsita.dlt/old.Lsita.dlt)
    if(alpha=='NaN') alpha <- -1
    uni <- runif(1)
    if(uni < alpha){
      Sita.dlt[hh] <- new.sita.dlt
    }else{
      rej.dlt[hh] <- rej.dlt[hh] + 1
    }

    ## lmbd���̌v�Z
    # ����̃Ƃ��m�ۂ���
    old.sita.lmbd <- Sita.lmbd[hh]
    # �V�����Ƃ��T���v�����O�i�����T���v�����O�j
    new.sita.lmbd <- Sita.lmbd[hh] + rnorm(1, 0, sigma.lmbd)
    
    # �ޓx�̌v�Z�i�ΐ��ޓx�̏ꍇ�̓��R�r�A���Œ����j
    new.Lsita.lmbd <- Lsita + Nkernel(new.sita.lmbd, Hlmbd, D[hh,], Vsita.lmbd)
    new.Lsita.lmbd <- exp(-0.5*new.Lsita.lmbd)
    old.Lsita.lmbd <- Lsita + Nkernel(old.sita.lmbd, Hlmbd, D[hh,], Vsita.lmbd)
    old.Lsita.lmbd <- exp(-0.5*old.Lsita.lmbd)
    
    # MH�X�e�b�v
    alpha <- min(1, new.Lsita.lmbd/old.Lsita.lmbd)
    if(alpha=='NaN') alpha <- -1
    uni <- runif(1)
    if(uni < alpha){
      Sita.lmbd[hh] <- new.sita.lmbd
    }else{
      rej.lmbd[hh] <- rej.lmbd[hh] + 1
    }    
    #--------------------------------------------    
  }
  ### step5--------------------------------------
  ## dlt���̎Z�o----
  # ���ϗʐ��K���z�̃p�����^�̎Z�o
  Hhat.dlt <- solve(crossprod(D)) %*% t(D) %*% Sita.dlt
  Dtld <- solve(crossprod(D) + A0) %*% (crossprod(D) %*% Hhat.dlt + A0%*%m0)
  rtld <- as.vector(Dtld)
  sig <-  (crossprod(D) + A0) %o% Vsita.dlt # %o%�̍��̑O���ς���̂̓_��
  # ���ϗʐ��K���z�ŃT���v�����O
  Hdlt <- rmvnorm(nvar, rtld, as.matrix(data.frame(sig))) 
  ##-----------------
  ## lmbd���̎Z�o----
  # ���ϗʐ��K���z�̃p�����^�̎Z�o
  Hhat.lmbd <- solve(crossprod(D)) %*% t(D) %*% Sita.lmbd
  Dtld <- solve(crossprod(D) + A0) %*% (crossprod(D) %*% Hhat.lmbd + A0%*%m0)
  rtld <- as.vector(Dtld)
  sig <-  (crossprod(D) + A0) %o% Vsita.lmbd # %o%�̍��̑O���ς���̂̓_��
  # ���ϗʐ��K���z�ŃT���v�����O
  Hlmbd <- rmvnorm(nvar, rtld, as.matrix(data.frame(sig))) 
  ##-----------------
  #--------------------------------------------
  
  ### step6--------------------------------------
  ##dlt���̎Z�o
  # �t�E�B�b�V���[�g���z�̃p�����^�̎Z�o
  div <- (Sita.dlt - D%*%matrix(Hdlt,nD,nvar))
  S <- crossprod(div)
  # �t�E�B�b�V���[�g���z�ŃT���v�����O
  Vsita.dlt <- riwish(f0 + nhh, V0 + S)  
  ##------------
  ##lmbd���̎Z�o
  # �t�E�B�b�V���[�g���z�̃p�����^�̎Z�o
  div <- (Sita.lmbd - D%*%matrix(Hlmbd,nD,nvar))
  S <- crossprod(div)
  # �t�E�B�b�V���[�g���z�ŃT���v�����O
  Vsita.lmbd <- riwish(f0 + nhh, V0 + S)  
  ##------------  
  #--------------------------------------------
}
##-------------------------------------------