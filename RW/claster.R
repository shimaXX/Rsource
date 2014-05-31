# TODO: Add comment
# 
# Author: n_shimada
###############################################################################


library(MASS)

setwd("C:/RW/galsterBIG")
data <- read.csv("galster_data_class_without_cdel.csv",row.names=1)

classedData <- kmeans(data, centers=5)
classedData
classedData$cluster
result.name <- names(classedData$cluster)
result.value <- classedData$cluster
#result <- rbind(result.name, result.value)
#result <- data.frame(result.name, result.value)
result <- data.frame(result.value)

write.table(result, "galster_claster.csv",sep=",")

#tapply(names(classedData$cluster), classedData$cluster,unique)


#library(mclust)
#plot(EMclust(data[,-1]))
#data.hc<-hc(modelName ="VVV", data=~data[,-1])		
#data.hcl <- hclass(data.hc,3)
#clPairs(data[,-1], cl=data.hcl)


library(cluster)
plot(pam(data[,-1],4), ask =TRUE)


tdata.d <- dist(tdata)
(tdata.hc <- hclust(tdata.d))
tdata.hc$height
plot(tdata.hc,hang=-1)
cutree(tdata.hc, k=5)
#cophenetic(tdata.hc)
cor(cophenetic(tdata.hc),tdata.d)



data <- read.csv("galster_data_class_without_cdel.csv",row.names=1)
data<-data[,-24]
data<-data[,-23]
tdata <- t(data)

d<-dist(tdata,"can")
ha<-hclust(d,"average")
hw<-hclust(d,"ward")
cor(cophenetic(ha),d)
cor(cophenetic(hw),d)
plot(ha,cex=1.2,hang=-1)
plot(hw,cex=1.2,hang=-1)