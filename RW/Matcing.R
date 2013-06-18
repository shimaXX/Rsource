install.packages("Matching")
library(Matching)
data(lalonde)

Y78 <- lalonde$re78   #従属変数を指定.ここではデータlalondeのre78
Tre <- lalonde$treat #独立変数（群別インディケータ）を指定
logi <- glm(treat ~age + educ + black + hisp + married + nodegr +
              re74 + re75 + u74 + u75, family=binomial, data=lalonde)
#glmを用いてロジスティック回帰を実行する
#treatをさまざまな共変量を用いて説明している

#mout <- Match(Y=Y78, Tr=Tre, X=logi$fitted)
mout <- Match(Y=Y78, Tr=Tre, X=logi$fitted,estimand="ATT")
#Yに従属変数,Trに独立変数,Xに傾向スコアの推定値を指定する
summary(mout) #結果を表示する

MatchBalance(treat ~ age + educ + black + hisp + married + nodegr
             + re74 + re75 + u74 + u75,match.out=mout, nboots=1000,data=lalonde)
#nbootsでブートストラップの回数を決められる
