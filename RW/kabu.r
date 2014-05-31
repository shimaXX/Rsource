setwd("D:/RW")

install.packages("RFinanceYJ")
library("RFinanceYJ")

DeNA<- quoteStockTsData("2432.t", "2012-01-04")
GREE<- quoteStockTsData("3632.t", "2012-01-04")

#【課題1】
head(DeNA,5)
head(GREE,5)

#【課題2】
plot(0:(nrow(DeNA)-1),DeNA$close,axes=F, xlab="2012/1/4からの経過日数",ylab="終値")
axis(1)
axis(2)

par(new=T)
plot(0:(nrow(GREE)-1),GREE$close,xlab="",ylab="",pch=0, col=2,axes=F)
axis(4)

#凡例を描く
Labels <- c("DeNA終値","GREE終値")
pchs <- c(1,0)
cols <- c(1,2)
legend(100,2700,cex=0.8,legend=Labels,pch=pchs,col=cols)

#【課題3】
plot(0:(nrow(DeNA)-1),DeNA$close/DeNA[1,c("close")],axes=F, xlab="2012/1/4からの経過日数",ylab="相対終値")
axis(1)
axis(2)

par(new=T)

plot(0:(nrow(GREE)-1),GREE$close/GREE[1,c("close")],xlab="",ylab="",pch=0, col=2,axes=F)
mtext("GREE終値",side=4,line=3)
axis(4)

#凡例を描く
Labels <- c("DeNA相対終値","GREE相対終値")
pchs <- c(1,0)
cols <- c(1,2)
legend(85,1.0,cex=0.8,legend=Labels,pch=pchs,col=cols)