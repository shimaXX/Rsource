library(Matching)
data(lalonde)

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