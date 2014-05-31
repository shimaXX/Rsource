# TODO: Add comment
# 
# Author: n_shimada
###############################################################################

setwd("C:/RW")

library(e1071)
お札<-read.csv("お札.csv")

#学習モデルの構成(crossは重交差妥当化の為のデータ分割数)
お札結果<-svm(真偽~.,data=お札,probability=TRUE,kernel="radial",cross=1)
お札予測値<-predict(お札結果,newdata=お札,probability=TRUE,decision.values=TRUE)
table(お札予測値,お札[,1])


#サポートベクター，係数，ガンマ, Cの抽出
サポートベクター<-お札結果$SV
係数<-お札結果$coefs
ガンマ<-お札結果$gamma
C<-お札結果$cost

#予測値，予測確率の表示
予測値<-round(attr(お札予測値, "decision.values"),3)
予測確率<-round(attr(お札予測値, "probabilities"),3)

t(head(予測値,8));t(tail(予測値,8))
t(head(予測確率,8));t(tail(予測確率,8))


#ガウシアンカーネル推定値
予測値算出<-function(x,y){
	サポートベクター<-お札結果$SV
	係数<-お札結果$coefs
	ガンマ<-お札結果$gamma
	mad<-as.matrix(cbind(x,y))
	mat<-scale(mad)
	zero<-numeric(length(x)*length(サポートベクター[,1]))
	カーネル<-matrix(zero,nrow=length(x),ncol=length(サポートベクター[,1]),byrow=F)
	重み付け<-matrix(zero,nrow=length(y),ncol=length(サポートベクター[,1]),byrow=F)
	for(i in 1:length(x)){
		　for(s in 1:length(サポートベクター[,1])){
			　　カーネル[i,s]<-exp(-ガンマ*sqrt(t(mat[i,1:2]-サポートベクター[s,1:2])%*%(mat[i,1:2]
										-サポートベクター[s,1:2]))^{2})
			　　重み付け[i,s]<-係数[s]*カーネル[i,s]
			　}
	}
	和<-apply(重み付け,1,sum)
	予測値<-和-お札結果$rho
}

#関数"予測値算出"による予測値
予測値2<-予測値算出(お札[,2],お札[,3])

#ライブラリー“scatterplot3d”の読み込み
library(scatterplot3d)

#予測曲面のプロット
N<-30
x<-seq(-3,3,length=N);y<-seq(-3,3,length=N)
persp(x,y,outer(x,y,予測値算出),zlab="f(x1,x2)",theta=133,phi=20,
		ticktype = "detailed",xlab="x1",ylab="x2")

#等高線のプロット
contour(x,y,outer(x,y,予測値算出),lwd=1,nlevels=10,labcex=1,
		xlim=c(-3,3),ylim=c(-3,3),method="edge")
par(new=T)
ID<-as.numeric(labels(お札結果$SV)[[1]])
ラベル<-as.numeric(お札[,1][ID])+1
plot(お札結果$SV,xlim=c(-3,3),ylim=c(-3,3),pch=ラベル)

#パッケージe1071付属の等高線プロット
plot(お札結果,お札,grid=300,color.palette =terrain.colors,svSymbol=5)

#チューニング K=10で重交差妥当化を行う．
#無作為にデータを分割するから最良のチューニング母数の値は本文書と異なることがある．
チューニング<-tune.svm(真偽~.,data=お札,gamma = 2^c(seq(-2,2,0.25)),
		cost = 2^c(seq(-2,2,0.25)))

#最適な母数による推定
最良モデル<-svm(真偽~.,data=お札,cross=10,gamma=0.2973018, cost=0.25)

#結果
結果<-summary(チューニング)

#グリッドサーチの誤判別率の3次元プロット
#無作為にデータを分割するから3次元プロットが本文と異なることがある．
scatterplot3d(結果[7]$performances[,1],結果[7]$performances[,2],
		結果[7]$performances[,3],xlab="gamma",ylab="C",zlab="error rate")