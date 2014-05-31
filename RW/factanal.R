# TODO: Add comment
# 
# Author: n_shimada
###############################################################################



setwd("C:/RW/galsterBIG")
data <- read.csv("galster_data_class.csv",row.names=1)

(data.fac <- factanal(data, factors=5))

barplot(data.fac$loading[,1],col="lightblue")
barplot(data.fac$loading[,2],col="lightblue")
barplot(data.fac$loading[,3],col="lightblue")
barplot(data.fac$loading[,4],col="lightblue")
barplot(data.fac$loading[,5],col="lightblue")
#barplot(data.fac$loading[,6],col="lightblue")
#barplot(data.fac$loading[,7],col="lightblue")

#“ÆŽ©ˆöŽq‚Ì•\Ž¦
round(data.fac$uniquenesses,3)