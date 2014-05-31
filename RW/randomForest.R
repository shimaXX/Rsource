# TODO: ランダムフォレスト分析
# 
# 作成者: nahki
###############################################################################



#install.packages("randomForest")
library(randomForest)
library(mlbench)

#データの取得
setwd("C:/RW/galsterBIG")		# ワークスペースの設定
#rowdata<-read.csv("galster_classed.csv")	#dataの読み込み
data <- read.csv("galster_per_day.csv")
#data(spam7)			# デモデータの読み込み
rowdata<- data[,-1]

seg0 <- sqldf(" select * from rowdata where seg='seg0' ")
seg1 <- sqldf(" select * from rowdata where seg='seg1' ")
seg2 <- sqldf(" select * from rowdata where seg='seg2' ")
seg3 <- sqldf(" select * from rowdata where seg='seg3' ")
seg4 <- sqldf(" select * from rowdata where seg='seg4' ")
seg5 <- sqldf(" select * from rowdata where seg='seg5' ")

nr.seg0 <- nrow(seg0)
nr.seg1 <- nrow(seg1)
nr.seg2 <- nrow(seg2)
nr.seg3 <- nrow(seg3)
nr.seg4 <- nrow(seg4)
nr.seg5 <- nrow(seg5)

n0 <-1:nr.seg0
n1 <-1:nr.seg1
n2 <-1:nr.seg2
n3 <-1:nr.seg3
n4 <-1:nr.seg4

seg0.smp <- sample(n0,nr.seg5)
seg1.smp <- sample(n1,nr.seg5)
seg2.smp <- sample(n2,nr.seg5)
seg3.smp <- sample(n3,nr.seg5)
seg4.smp <- sample(n4,nr.seg5)

seg0 <- seg0[seg0.smp,]
seg1 <- seg1[seg1.smp,]
seg2 <- seg2[seg2.smp,]
seg3 <- seg3[seg3.smp,]
seg4 <- seg4[seg4.smp,]

rowdata <- as.data.frame(rbind(seg0,seg1,seg2,seg3,seg4,seg5))
#rowdata <- as.data.frame(rbind(seg1,seg2,seg3,seg4,seg5))

nr <- nrow(rowdata)

nr.s <- as.integer(nr/2)

even.n<- 2*(1:nr.s)
BC.train<-rowdata[even.n,-1]
BC.test<-rowdata[-even.n,-1]

#ランダムシード
set.seed(20)

#データの格納
data.train<-BC.train
data.test<-BC.test

#列数を取得
nc<-ncol(data.test)

#ランダムフォレストの実行
#na.omitでデータセット中の欠損値を除く
data.rf<-randomForest(seg~., data=data.train, na.action="na.omit")	
print(data.rf)

#オブジェクトに記録されている項目を確認
summary(data.rf)

#上記で行ったRFがどのような種類（回帰、分類etc）なのかを確認
data.rf$type

#用いた木の数と誤判別率との関係をグラフ表示
plot(data.rf)

#判別・回帰における変数の重要度（ジニ係数）を表示
data.rf$importance

#ジニ係数による変数の重要度をプロット
varImpPlot(data.rf)

#エラー回避のためdata.test[[1]]の因子水準をdata.rfのxlevels[[1]]に格納
data.rf$forest$xlevels[[1]]<-levels(data.test[[1]])

#新しいデータに対して判別・回帰を行う
data.rfp<-predict(data.rf, data.test[,-nc])
(data.rft<-table(data.test[,nc], data.rfp))

#誤判別率の計算
round(1-sum(diag(data.rft))/sum(data.rft),3)