setwd("C:/RW")
data <- read.table("test/EvaSeq1209.csv",sep=",")
uid <- data[,1]
data <- data[,-1]
date <- seq(ncol(data))

library(RHmm)

n <- ncol(data)
out <- data[,1:n]

# HMMモデルによる推定
for(i in 1:nrow(data)){
  hmm.fitted <- HMMFit(as.numeric(data[i,1:n]), dis = "NORMAL", nStates = 4)
  probabilities <- forwardBackward(hmm.fitted, as.numeric(data[i,1:n]))

  hmm.fitted$HMM$distribution

  #上のコマンドで取得した内容から、平均値の最も高い状態を取ってきて以下に入力
  probability.anomaly <- probabilities$Gamma[, 2]
  out[i,1:n] <- ifelse(is.na(probability.anomaly), 0, probability.anomaly)
}
out <- cbind(uid,out)
head(out)

write.table(probability.anomaly,"test/output.csv",sep=",")