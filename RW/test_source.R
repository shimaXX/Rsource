setwd("D:/RW")
library(mvpart)

data<-read.csv("test.csv")

rep<-rpart(service~.,data=data)
plot(rp,uniform=TRUE)
text(rp,use.n=T,all=T)
