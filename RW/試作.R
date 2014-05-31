setwd("D:/RW")

library(mvpart)

data<-read.csv("CodeIQ_Finance.csv")
#data<-read.table("tesehoge.txt",head=T)
modData <- data[,-11]

#rep<-rpart(modData[,11]~.,data=modData,cp=0.002,method="class")
rep<-rpart(プリペイ2~.,data=modData,cp=0.002)
plot(rep,uniform=TRUE)
text(rep,use.n=T,all=T)


library(MASS)

row_num <- (nrow(modData))

even.n <- sample(seq(row_num),row_num/3)
train.data <- modData[-even.n,]
test.data <- modData[even.n,]
(Z.qda <- qda(プリペイ2~.,data=train.data))
y <- predict(Z.qda,test.data[,-11])
table(test.data[,11],y$class)


install.packages("randomForest")
library(randomForest)

set.seed(20)
pp.rf <- randomForest(プリペイ2~.,data=train.data,na.action="na.omit")
print(pp.rf)
plot(pp.rf)
varImpPlot(pp.rf)
pp.rfp <- predict(pp.rf,test.data[,-11])
(pp.rft <- table(test.data[,11],pp.rfp))
round(1-sum(diag(pp.rft))/sum(pp.rft),3)


fm5 <- glm(プリペイ2~.,data=modData, family=binomial)
f <- step(fm5)
fm5$coefficients
x <- predict(fm5,type="response")
write.table(x,"output.csv",sep=",")