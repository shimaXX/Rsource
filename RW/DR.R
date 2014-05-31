#------DR（ダブリーロバスト推定量）
#P129の確認方法で共変量として良いのかどうかを確認する
setwd("C:/RW/causal_inference/")
RowData <- read.csv("Ci-LaboPekoFlag_Buzz.csv")

library(Matching)
pseudoR2<-function(model) 
{1-(deviance(model)/model$null.deviance)}

RowData <- RowData[,-1]
dependentV <- RowData[,"kutikomi_vcnt"]
data <- RowData[,-3]

logi.p <- ncol(data) - 1
logi.n <- ncol(data) - 1

#------p129に基づき従属変数と関連がある変数を選択することをやってみる
zcol <- ncol(RowData)
dataL <- RowData[,-zcol]
Linear <- lm(kutikomi_vcnt ~ . , data = dataL)
Linear <- step(Linear)
#--

#------傾向スコアの計算
#--「割り当てZと共変量xのモデル」に該当（正しければ条件Aに該当P88）
#logi <- glm(Z ~., family=binomial, data=data)
logi <- glm(Z ~. , family=binomial, data=data)
logi <- step( logi )
#自由度調整済み疑似決定係数
1-(1-pseudoR2(logi))*logi$df.null/logi$df.residual
PS    <- logi$fitted

n1      <- nrow(data[data$Z == 1, ])
n0      <- nrow(data[data$Z == 0, ])
#--「共変量で結果変数を説明する回帰関数」に該当（正しければ条件Bに該当P88）
Linear1 <- lm(kutikomi_vcnt ~ . , data = RowData[RowData$Z == 1, ])
#Linear1 <- step(Linear1)
Linear0 <- lm(kutikomi_vcnt ~ . , data = RowData[RowData$Z == 0, ])
#Linear0 <- step(Linear0)
Y1      <- RowData$kutikomi_vcnt[RowData$Z == 1]
Y0      <- RowData$kutikomi_vcnt[RowData$Z == 0]
PS1     <- PS[data$Z == 1]
PS0     <- PS[data$Z == 0]

(DR1 <- 1/n1 * sum(Y1 + ( (1 - PS1)/PS1) * (Y1 - Linear1$fitted)))
(DR0 <- 1/n0 * sum(Y0/(1 - PS0) + (1 - 1/(1 - PS0))*Linear0$fitted))


summary(logi)
summary(Linear1)
summary(Linear0)


#nc.logi <- ncol(logi$R)
#cn.logi <- colnames(logi$R)[2:nc.logi]

#nc.Linear1 <- length(Linear1$coefficients)
#cn.Linear1 <- names(Linear1$coefficients)[2:nc.Linear1]

#nc.Linear0 <- length(Linear0$coefficients)
#cn.Linear0 <- colnames(Linear0$coefficients)[2:nc.Linear0]