# TODO: Add comment
# 
# Author: n_shimada
###############################################################################

###↓TV普及データ
year<-c(1966:1984)
hukyu<-c(0.003,0.016,0.054,0.139,0.263,0.423,0.611,0.758,0.859,0.903,0.937,0954,0.978,0.978,0.982,0.985,01.989,0.988,0.9992)

TV<-data.frame(year, hukyu)
fm5<-glm(hukyu~., data=TV, family=binomial)

#fm<-nls(hukyu~a/(1+b*exp(c*year)),start=c(a=1,b=1,c=-1), trace=TRUE)

fm<-nls(hukyu~a/(1+b*exp(c*1:19)),start=c(a=1,b=1,c=-1), trace=TRUE)
####↑


####↓タイタニックデータ
q <- glm(Survived ~ ., data = Titanic1, family = "binomial")
#cbind(z, SurveRate = predict(q, Titanic, type = "response"))
#exp(q$coefficient)[-1]
###//タイタニック


##青木
source("http://aoki2.si.gunma-u.ac.jp/R/src/sreg.R", encoding="euc-jp")
dat <- matrix(c(	# 3 列目が従属変数
				1.2, 1.9, 0.9,
				1.6, 2.7, 1.3,
				3.5, 3.7, 2.0,
				4.0, 3.1, 1.8,
				5.6, 3.5, 2.2,
				5.7, 7.5, 3.5,
				6.7, 1.2, 1.9,
				7.5, 3.7, 2.7,
				8.5, 0.6, 2.1,
				9.7, 5.1, 3.6
		), byrow=TRUE, nc=3)
ans <- sreg(dat)
ans$予測
plot(ans, which="slope1")
##青木



##ファイナンスデータ
test1 <- lm( Fertility ~ ., data = swiss )

summary( test1 )

rlt1 <- step( test1 )

summary( rlt1 )
##ファイナンスデータ

##サンプル
fm5<-glm(buy_flag~., data=test1, family=binomial)

test1<-read.csv("galster_usre_master_rev2.csv")
fm5<-glm(buy_flag~., data=test1, family=binomial)
fm5<-glm(buy_flag~., data=test1, family=gaussian())
lines(test1[,18],fitted(fm5), lty=2, col="red", lwd=2)

predict(fm5, type="response")
summary(fm5)

# 回帰曲線の式を入力、グラフ上へプロット
lineLogistic = function(x){
	exp (-1.105246661+(-0.006204704) * x)/(1+exp(-1.105246661+(-0.006204704) * x))
}
plot (lineLogistic, -360, 0, add=TRUE, col = "black")
##サンプル終了

##サンプリング
n<-nrow(xx)
size<-nrow(xx)
s <- c(sample(1:size, num))# 検証用データの無作為抽出
xxm<-xx[s,]
nrow(xxm)
##サンプリング