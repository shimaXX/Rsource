# TODO: Add comment
# 
# Author: n_shimada
###############################################################################


library(som)
library(mvpart)

setwd("C:/RW")
data <- read.csv("ci-labo_data.csv",row.names=1)

normarizedData <- normalize(data[,-1], byrow = F)
somData<- som(normarizedData, xdim = 10, ydim = 10, topol = "rect")

rnd <- cbind(rnorm(nrow(data), 0,0.15), rnorm(nrow(data), 0,0.15))

mapData <- somData$visual[,1:2] + rnd + 0.5
plot(mapData, xlim = c(0.10), ylim = c(0,10))
text(mapData[,1], mapData[,2], data$client_user_id)

som2Data<-som(normarizedData, xdim=5,ydim=1, topol="rect")
treeData<-cbind(data,som3Data$visual[,1]+1)

myco<-ncol(treeData)

cokname(treeData)[mycol] <- "segment"

tree<-rpat(segment~., data=treeData[,-1], method="class")
print(tree)