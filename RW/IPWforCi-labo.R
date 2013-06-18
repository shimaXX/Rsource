setwd("C:/RW/causal_inference/")
RowData <- read.csv("Ci-LaboPekoFlag_Buzz.csv")

library(Matching)

RowData <- RowData[,-1]
dependentV <- RowData[,"kutikomi_vcnt"]
data <- RowData[,-3]

logi <- glm(Z ~., family=binomial, data=data)
#logi <- glm(RowData$kutikomi_vcnt ~., family=binomial, data=RowData)
#eの値が小さすぎると重みが大きくなりすぎるので、e<0.1は0.1に置き換える
e <- ifelse(logi$fitted<0.1,0.1,logi$fitted)

ivec1 <- data$Z
ivec2 <- rep(1, nrow(data)) - ivec1
ivec <- cbind(ivec1, ivec2)
#iestp1 <- (ivec1/logi$fitted) * (length(ivec1)/sum(ivec1))
iestp1 <- (ivec1/e) * (length(ivec1)/sum(ivec1))
#iestp2 <- (ivec2/(1-logi$fitted)) * (length(ivec2)/sum(ivec2))
iestp2 <- (ivec2/(1-e)) * (length(ivec2)/sum(ivec2))

iestp <- iestp1 + iestp2  #傾向スコアの推定値の逆数を重みとする
ipwe <- lm(RowData$kutikomi_vcnt ~ ivec-1, weights=iestp, data=RowData)
summary(ipwe)  #推定値等を表示する

#######
#周辺構造モデル
#install.packages("survey")
library(survey)
psw <- svydesign(ids = ~1, weights = iestp, data= RowData)
ipwsu <- svyglm(kutikomi_vcnt ~ Z, design = psw)
summary(ipwsu)