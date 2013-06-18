# TODO: Add comment
# 
# Author: n_shimada
###############################################################################



setwd("C:/RW/galsterBIG")
data <- read.csv("galster_analysis_energy.csv")
data<-data[,-1]


#入力データの整形
attach(data)
data.atention <- new + top + article + ranking + category
data.interest <- search + comment + inquiry + w_review

#data[,6]はlogin。loginは予約語なので外す。
data.disposition <- cart + entry + data[,6] + re_mail + a_favorite + w_favorite + inquiry_article	
data.purchase <- buy + a_review
data.attachment <- attach_flag + mail_magazine_reg	#atach_flag = case when buy >1 then 1 else 0 end + case when login >1 then 1 else 0
fixed.data <- as.matrix(cbind(data.atention,data.interest,data.disposition,data.purchase,data.attachment))


#列ごと、または、行ごとに関数の適用
result <- apply(fixed.data,2,sum)	#2は列ごとに適用

div <- 1/result	#逆数をとる
div <- div/div[1]

result.sum <- rbind(result, div)

#データ型をマトリクスに
div.vec <- as.matrix(div)


#行列積に計算
result.enegy <- fixed.data %*% div.vec

#energy = 0の人はlogとるとマイナスになるので1をプラスする
result.enegy <- result.enegy +1
result.enegy <- log(result.enegy)


#結果の出力
write.table(result.sum,"data_sum.csv",sep=",")
write.table(result.enegy,"data_enegy.csv",sep=",")