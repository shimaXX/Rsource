library(ChangeAnomalyDetection)
library(RFinanceYJ) 
library(ggplot2) 
library(reshape2) 

stock <- quoteStockXtsData("2432.t", since = "2011-01-01")
head(stock)

stock <- as.data.frame(stock)
stock$date <- as.POSIXct(rownames(stock)) #文字列の年月日をPOIXct型に変換

ggplot(stock, aes(x = date, y = Close)) + geom_line()

change.score <- changeAnomalyDetection(x = stock$Close, term = 30, order = c(1, 1, 0))
tail(change.score)

stock$change.score <- change.score

ggplot(stock, aes(x = date, y = change.score)) + 
  geom_line()

#stock <- stock[stock$change.score != 0, ]

# ggplotでは2軸のプロットが出来ないので、スケールを合わせてる
#stock$change.score <- stock$change.score * 10 + 1000 

# この手の整形はreshape2パッケージのmelt関数が便利！
stock.melt <- melt(stock, id.vars = "date", measure.vars = c("Close", "change.score"))

ggplot(stock.melt, aes(x = date, y = value)) + 
  geom_line(aes(col = variable))
