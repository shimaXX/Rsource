#install.packages("Matching")
library(Matching)
data(lalonde) #職業訓練の受講と訓練後の年収、その他個人の属性データ

# 訓練を受けそうかどうかの推定を属性を使ってロジスティック回帰で推定
logi <- glm(treat ~age + educ + black + hisp + married + nodegr +
              re74 + re75 + u74 + u75, family=binomial, data=lalonde)

ivec1 <- lalonde$treat
ivec2 <- rep(1, nrow(lalonde)) - ivec1
ivec <- cbind(ivec1, ivec2)
iestp1 <- (ivec1/logi$fitted) * (length(ivec1)/sum(ivec1))
iestp2 <- (ivec2/(1-logi$fitted)) * (length(ivec2)/sum(ivec2))

iestp <- iestp1 + iestp2  #傾向スコアの推定値の逆数を重みとする

ipwe <- lm(lalonde$re78 ~ ivec-1, weights=iestp, data=lalonde)
summary(ipwe) #推定値等を表示する

#install.packages("survey")
library(survey)
psw <- svydesign(ids = ~1, weights = iestp, data= lalonde)
ipwsu <- svyglm(re78 ~ treat, design = psw)
summary(ipwsu)


# デモ用のコード
mout<-Match(Y=lalonde$re78, Tr=lalonde$treat, X=logi$fitted)
head(mout$index.treated)
head(mout$index.control)