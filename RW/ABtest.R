#検出力を使ったサンプルサイズ決定
#とりあえずχ二乗検定

#install.packages("pwr")
library("pwr")

#効果量を調べる
cohen.ES(test="chisq",size="medium")

(res.pwr <- pwr.chisq.test(w=0.07, df=1, power=0.8))

res.pwr$N

source("http://aoki2.si.gunma-u.ac.jp/R/src/G2.R", encoding="euc-jp")
G2(matrix(c(5, 5, 1, 6, 4, 4, 2, 2, 1), byrow=TRUE, nc=3))
G2(matrix(c(4, 5, 2, 0, 0, 7, 6, 1, 1, 0, 3, 1), byrow=TRUE, nc=4), correct=TRUE)

#http://d.hatena.ne.jp/what_a_dude/20111121/p1
power.prop.test(p1=0.03,p2=0.04,sig.level=0.05,power=0.95)

#http://cse.naro.affrc.go.jp/takezawa/r-tips/r/66.html
prop.test(c(400,250),c(9000,9000))

pt(q=(qt(p=0.025,df=18)),df=18,ncp=-1.18)
curve(dt(x,df=18,ncp=-0.5*sqrt(5)),-5,3)