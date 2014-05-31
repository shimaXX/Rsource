library(ChangeAnomalyDetection)
#library(RFinanceYJ) 
library(ggplot2) 
library(reshape2) 

#stock <- quoteStockXtsData("2432.t", since = "2011-01-01")
#head(stock)

#stock <- as.data.frame(stock)
#stock$date <- as.POSIXct(rownames(stock)) #文字列の年月日をPOIXct型に変換

setwd("C:/RW")
data <- read.table("test/test.txt")

head(data)

data <- t(data)
s <- as.data.frame(seq(nrow(data)))

data.v <- as.data.frame(cbind(s, data))
colnames(data.v) <- c("time","cnt")

ggplot(data.v, aes(x = time, y = cnt)) + geom_line()

change.score <- changeAnomalyDetection(x = data.v$cnt, term = 35, smooth.n = 5, order = c(1, 1, 0))
tail(change.score)

data.v$change.score <- change.score

ggplot(data.v, aes(x = time, y = change.score)) + 
  geom_line()

#stock <- stock[stock$change.score != 0, ]

# ggplotでは2軸のプロットが出来ないので、スケールを合わせてる
#stock$change.score <- stock$change.score * 10 + 1000 

# この手の整形はreshape2パッケージのmelt関数が便利！
data.melt <- melt(data.v, id.vars = "time", measure.vars = c("cnt", "change.score"))

ggplot(data.melt, aes(x = time, y = value)) + 
  geom_line(aes(col = variable))

write.table(change.score,"test/output.csv",sep=",")