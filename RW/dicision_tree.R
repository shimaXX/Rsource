###############################################################################
# 【rpartオブジェクトの葉のルールを表示する関数】
# http://www.togaware.com/datamining/survivor/Convert_Tree0.html
###############################################################################
list.rules.rpart <- function(model)
{
	if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
	frm     <- model$frame
	names   <- row.names(frm)
	ylevels <- attr(model, "ylevels")
	ds.size <- model$frame[1,]$n
	for (i in 1:nrow(frm))
	{
		if (frm[i,1] == "<leaf>")
		{
			cat("\n")
			cat(sprintf(" Rule number: %s ", names[i]))
			cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
							ylevels[frm[i,]$yval], frm[i,]$n,
							round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
			pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
			cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
		}
	}
}

###############################################################################
# 【誤分類率算出関数】
# http://www.genetics.ucla.edu/horvathlab/Biostat278/Discussion3supervised.doc
###############################################################################
misclassification.rate <- function(tab){
	num1 <- sum(diag(tab))
	denom1 <- sum(tab)
	signif(1-num1/denom1,3)
}

###############################################################################
# 【分析の下準備】
# 事前にmvpartパッケージをインストールしておくこと。
# また、日本語対応のRdevgaをマイドキュメントに置いておくこと。
# CSVファイルは下記のようにして読み込む。
# mushroom <- read.csv("C:/R/data/mushroom.japanese.csv")
# タブ区切りファイルは下記のようにして読み込む。
# mushroom <- read.delim("C:/R/data/mushroom.japanese.txt")
# 名義尺度は下記のように設定。文字列なら不要。
# car.test.frame$Type <- as.factor(car.test.frame$Type)
# 水準の順番を指定したい時は
# car.test.frame$Type <- factor(car.test.frame$Type,
#				levels=c("Small","Compact","Sporty",
#					"Medium", "Van", "Large"))
# 順序尺度は下記のようにして設定。
# car.test.frame$Type <- ordered(car.test.frame$Type,
#				levels=c("Small","Compact","Sporty",
#					"Medium", "Van", "Large"))
###############################################################################
options(scipen=20)	# 整数で表示させるための設定
library(sqldf)		# sqlパッケージの読み込み
library(mvpart)		# 多変量決定木パッケージの読み込み
library(DAAG)		# デモデータ用パッケージの読み込み
setwd("C:/RW/Eva/")		# ワークスペースの設定
#rowdata<-read.csv("galster_classed.csv")	#dataの読み込み
data <- read.csv("Eva1114_rev3.csv")
#data(spam7)			# デモデータの読み込み
rowdata<- data[,-1]

#seg0 <- sqldf(" select * from rowdata where seg='seg0' ")
#seg1 <- sqldf(" select * from rowdata where seg='seg1' ")
#seg2 <- sqldf(" select * from rowdata where seg='seg2' ")
#seg3 <- sqldf(" select * from rowdata where seg='seg3' ")
#seg4 <- sqldf(" select * from rowdata where seg='seg4' ")
#seg5 <- sqldf(" select * from rowdata where seg='seg5' ")

#nr.seg0 <- nrow(seg0)
#nr.seg1 <- nrow(seg1)
#nr.seg2 <- nrow(seg2)
#nr.seg3 <- nrow(seg3)
#nr.seg4 <- nrow(seg4)
#nr.seg5 <- nrow(seg5)

#n0 <-1:nr.seg0
#n1 <-1:nr.seg1
#n2 <-1:nr.seg2
#n3 <-1:nr.seg3
#n4 <-1:nr.seg4

#seg0.smp <- sample(n0,nr.seg5)
#seg1.smp <- sample(n1,nr.seg5)
#seg2.smp <- sample(n2,nr.seg5)
#seg3.smp <- sample(n3,nr.seg5)
#seg4.smp <- sample(n4,nr.seg5)

#seg0 <- seg0[seg0.smp,]
#seg1 <- seg1[seg1.smp,]
#seg2 <- seg2[seg2.smp,]
#seg3 <- seg3[seg3.smp,]
#seg4 <- seg4[seg4.smp,]

#rowdata <- as.data.frame(rbind(seg0,seg1,seg2,seg3,seg4,seg5))

###############################################################################
# 【学習用データと検証用データの用意】
# ここでは３分の１を検証用に、残りを学習用にしている。
# 分類するクラスタのサイズを揃える時は、事前にデータをクラスタごとにソートして
# おいてから、下記のようにクラスタごとに抽出する。
# sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
###############################################################################
(n.size <- nrow(rowdata))		# サンプルサイズの確認
p <- floor(n.size/3)			# 検証用データのサイズ設定
sub <- c(sample(1:n.size, p))		# 検証用データの無作為抽出
train.data <- rowdata[-sub,]	# 学習用データの切り出し
test.data <- rowdata[sub,]	# 検証用データの切り出し

###############################################################################
# 初期の決定木の実行
###############################################################################
tree1 <- mvpart(seg~.,			# 変数の指定。目的変数~説明変数
		data=train.data,		# データセットの指定
		# parms=list(split="information"),	# 分割基準の指定
		# cp=0,				# 剪定基準の設定
		xval=3)				# 交差検証の回数設定
train.model <- predict(	tree1,			# 初期のモデルに基づく分類
		newdata=train.data,	# 学習用データセットの指定
		type="class")		# モデルのタイプ指定
(tab1 <- table(train.model,train.data$seg))	# 学習用データの分類状況
misclassification.rate(tab1)			# 誤分類率の算出

###############################################################################
# 決定木の剪定
# pruned()を使った方が効率的だが、mvpartのグラフが便利なので再分析をしている。
###############################################################################
printcp(tree1)				# 交差検証の結果を出力
plotcp(tree1)				# 交差検証の結果をグラフ化
(tree.pruned <- mvpart(	seg~.,		# 変数の指定。目的変数~説明変数
					data=train.data,	# データセットの指定
					uniform=F,		# グラフの枝の長さの指定
					margin=0.01,		# グラフの余白の指定
					branch=1,		# グラフの枝の角度の指定
					# parms=list(split="information"),	# 分割基準の指定
					cp=0.02,		# 交差検証の結果から枝を剪定 cp=0.029
					xval=3))		# 交差検証の回数設定
pruned.model <- predict(tree.pruned,		# 剪定後のモデルに基づく分類
		newdata=train.data,	# 学習用データセットの指定
		type="class")		# モデルのタイプ指定
(tab2 <- table(pruned.model,train.data$seg))	# 学習用データの分類状況
misclassification.rate(tab2)			# 誤分類率の算出
list.rules.rpart(tree.pruned)		# 葉ごとのルールの表示

###############################################################################
# 検証用データによるモデルの検証
###############################################################################
test.model <- predict(	tree.pruned,		# 剪定後のモデルに基づく分類
		newdata=test.data,	# 検証用データセットの指定
		type="class")		# モデルのタイプ指定
(tab3 <- table(test.model,test.data$seg))	# 検証用データの分類状況
misclassification.rate(tab3)			# 誤分類率の算出