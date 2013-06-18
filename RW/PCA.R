# TODO: Add comment
# 
# Author: n_shimada
###############################################################################

library(sqldf)

setwd("C:/RW/Eva")
dataOrg <- read.csv("evaRFMprior.csv")
data <- as.data.frame(dataOrg[,-1])
data <- as.data.frame(data[,-4])

#cox-box変換を入れる→毎回変わっては困るので、平方近似する
#data.sq <- ((data)^0.5 - 1)/0.5
data.log <- log(data+1)

data.mean <- apply(data,2,mean)
data.sd <- apply(data.log,2,sd)
data.mean$SessionTime <- mean(data.log$SessionTime)
data.mean$Payment <- mean(data$Payment[data$Payment > 0])
data.mean <- as.data.frame(data.mean)

#dataScale <- scale(data.sq)

SessionCnt <-		
	ifelse(data[,1] <= qpois(0.2,data.mean$SessionCnt), 1, 
			ifelse(data[,1] > qpois(0.2,data.mean$SessionCnt) & data[,1] <= qpois(0.4,data.mean$SessionCnt) , 2,
					ifelse(data[,1] > qpois(0.4,data.mean$SessionCnt) & data[,1] <= qpois(0.6,data.mean$SessionCnt) , 3,
							ifelse(data[,1] > qpois(0.6,data.mean$SessionCnt) & data[,1] <= qpois(0.8,data.mean$SessionCnt) , 4,
									ifelse(data[,1] > qpois(0.8,data.mean$SessionCnt) , 5, "er")
							)
					)
			)
	)

SessionTime <-		
		ifelse(data.log[,2] <= qnorm(0.2,data.mean$SessionTime,data.sd["SessionTime"]), 1, 
				ifelse(data.log[,2] > qnorm(0.2,data.mean$SessionTime,data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.4,data.mean$SessionTime,data.sd["SessionTime"]) ,2,
						ifelse(data.log[,2] > qnorm(0.4,data.mean$SessionTime,data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.6,data.mean$SessionTime,data.sd["SessionTime"]) ,3,
								ifelse(data.log[,2] > qnorm(0.6,data.mean$SessionTime,data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.8,data.mean$SessionTime,data.sd["SessionTime"]) ,4,
										ifelse(data.log[,2] > qnorm(0.8,data.mean$SessionTime,data.sd["SessionTime"]) , 5, "er")
								)
						)
				)
		)


Payment <-		
		ifelse(data[,3] <= qexp(0.2,1/data.mean$Payment), 1, 
				ifelse(data[,3] > qexp(0.2,1/data.mean$Payment) & data[,3] <= qexp(0.4,1/data.mean$Payment) ,2,
						ifelse(data[,3] > qexp(0.4,1/data.mean$Payment) & data[,3] <= qexp(0.6,1/data.mean$Payment) ,3,
								ifelse(data[,3] > qexp(0.6,1/data.mean$Payment) & data[,3] <= qexp(0.8,1/data.mean$Payment) ,4,
										ifelse(data[,3] > qexp(0.8,1/data.mean$Payment) , 5, "er")
								)
						)
				)
		)


#Recency <-		
#		ifelse(data[,4] <= qpois(0.2,data.mean$Recency), 1, 
#				ifelse(data[,4] > qpois(0.2,data.mean$Recency) & data[,4] <= qpois(0.4,data.mean$Recency) ,2,
#						ifelse(data[,4] > qpois(0.4,data.mean$Recency) & data[,4] <= qpois(0.6,data.mean$Recency) ,3,
#								ifelse(data[,4] > qpois(0.6,data.mean$Recency) & data[,4] <= qpois(0.8,data.mean$Recency) ,4,
#										ifelse(data[,4] > qpois(0.8,data.mean$Recency) , 5, "er")
#								)
#						)
#				)
#		)

#data.bin <- cbind(as.integer(SessionCnt), as.integer(SessionTime), as.integer(Payment), as.integer(Recency)-1)
data.bin <- cbind(as.integer(SessionCnt), as.integer(SessionTime), as.integer(Payment))

data.pc <- prcomp(data, scale=TRUE)
summary(data.pc)

#固有ベクトルの表示
data.pc$rotation

#固有値の算出
data.pc$sdev^2

#主成分負荷量の計算
t( data.pc$sdev * t( data.pc$rotation ) )[, drop = FALSE]

#得点を求める
vecter <- as.data.frame(data.pc$rotation[,1])
#vecter <- as.matrix(vecter/sum(vecter[-4,]))
vecter <- as.matrix(vecter/sum(vecter))
#vecter[4,] <- -1	

#data$SessionTime <- data$SessionTime/10
#data$Payment <- data$Payment/10
dat <- as.matrix(data.bin) 

#行列積の計算
result <- dat %*% vecter

#マイナス値の修正とロングテール補正
#result <- result*-1
write.table(result,"C:/Users/n_shimada/Desktop/rankPrior.csv",sep=",")
write.table(dat,"C:/Users/n_shimada/Desktop/binPrior.csv",sep=",")

##############
##post側の計算
#############
dataPost <- read.csv("evaRFMpost.csv")
dataPost <- as.data.frame(dataPost[,-1])
dataPost <- as.data.frame(dataPost[,-4])

dataPost.log <- log(dataPost + 1)

dataPost.mean <- apply(dataPost,2,mean)
dataPost.sd <- apply(dataPost.log,2,sd)
dataPost.mean$SessionTime <- mean(dataPost.log$SessionTime)
dataPost.mean$Payment <- mean(dataPost$Payment[dataPost$Payment > 0])
dataPost.mean <- as.data.frame(dataPost.mean)

#dataPostScale <- scale(dataPost.sq)

SessionCnt <-		
		ifelse(dataPost[,1] <= qpois(0.2,dataPost.mean$SessionCnt), 1, 
				ifelse(dataPost[,1] > qpois(0.2,dataPost.mean$SessionCnt) & dataPost[,1] <= qpois(0.4,dataPost.mean$SessionCnt) , 2,
						ifelse(dataPost[,1] > qpois(0.4,dataPost.mean$SessionCnt) & dataPost[,1] <= qpois(0.6,dataPost.mean$SessionCnt) , 3,
								ifelse(dataPost[,1] > qpois(0.6,dataPost.mean$SessionCnt) & dataPost[,1] <= qpois(0.8,dataPost.mean$SessionCnt) , 4,
										ifelse(dataPost[,1] > qpois(0.8,dataPost.mean$SessionCnt) , 5, "er")
								)
						)
				)
		)

SessionTime <-		
		ifelse(dataPost.log[,2] <= qnorm(0.2,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]), 1, 
				ifelse(dataPost.log[,2] > qnorm(0.2,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) & dataPost.log[,2] <= qnorm(0.4,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) ,2,
						ifelse(dataPost.log[,2] > qnorm(0.4,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) & dataPost.log[,2] <= qnorm(0.6,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) ,3,
								ifelse(dataPost.log[,2] > qnorm(0.6,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) & dataPost.log[,2] <= qnorm(0.8,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) ,4,
										ifelse(dataPost.log[,2] > qnorm(0.8,dataPost.mean$SessionTime,dataPost.sd["SessionTime"]) , 5, "er")
								)
						)
				)
		)


Payment <-		
		ifelse(dataPost[,3] <= qexp(0.2,1/dataPost.mean$Payment), 1, 
				ifelse(dataPost[,3] > qexp(0.2,1/dataPost.mean$Payment) & dataPost[,3] <= qexp(0.4,1/dataPost.mean$Payment) ,2,
						ifelse(dataPost[,3] > qexp(0.4,1/dataPost.mean$Payment) & dataPost[,3] <= qexp(0.6,1/dataPost.mean$Payment) ,3,
								ifelse(dataPost[,3] > qexp(0.6,1/dataPost.mean$Payment) & dataPost[,3] <= qexp(0.8,1/dataPost.mean$Payment) ,4,
										ifelse(dataPost[,3] > qexp(0.8,1/dataPost.mean$Payment) , 5, "er")
								)
						)
				)
		)


#Recency <-		
#		ifelse(dataPost[,4] <= qpois(0.2,dataPost.mean$Recency), 1, 
#				ifelse(dataPost[,4] > qpois(0.2,dataPost.mean$Recency) & dataPost[,4] <= qpois(0.4,dataPost.mean$Recency) ,2,
#						ifelse(dataPost[,4] > qpois(0.4,dataPost.mean$Recency) & dataPost[,4] <= qpois(0.6,dataPost.mean$Recency) ,3,
#								ifelse(dataPost[,4] > qpois(0.6,dataPost.mean$Recency) & dataPost[,4] <= qpois(0.8,dataPost.mean$Recency) ,4,
#										ifelse(dataPost[,4] > qpois(0.8,dataPost.mean$Recency) , 5, "er")
#								)
#						)
#				)
#		)

#dataPost.bin <- cbind(as.integer(SessionCnt), as.integer(SessionTime), as.integer(Payment), as.integer(Recency)-1)
dataPost.bin <- cbind(as.integer(SessionCnt), as.integer(SessionTime), as.integer(Payment))

dataPost.pc <- prcomp(dataPost, scale=TRUE)
summary(dataPost.pc)

#固有ベクトルの表示
dataPost.pc$rotation

#固有値の算出
dataPost.pc$sdev^2

#主成分負荷量の計算
t( dataPost.pc$sdev * t( dataPost.pc$rotation ) )[, drop = FALSE]

#得点を求める
#vecter <- dataPost.pc$rotation[,1]
#vecter <- as.matrix(vecter/sum(vecter))
#dataPost$SessionTime <- dataPost$SessionTime/10
#dataPost$Payment <- dataPost$Payment/10
dat <- as.matrix(dataPost.bin) 

#行列積の計算
resultPost <- dat %*% vecter
write.table(resultPost,"C:/Users/n_shimada/Desktop/rankPost.csv",sep=",")
write.table(dat,"C:/Users/n_shimada/Desktop/binPost.csv",sep=",")


#マイナス値の修正とロングテール補正
#resultPost <- resultPost*-1
#res <- (resultPost[,1] - result[,1])/result[,1]
res <- (resultPost[,1] - result[,1])

uid <- as.data.frame(dataOrg$uid)
output <- cbind(uid,result[,1],resultPost[,1],res)
colnames(output)<- c("uid","priorE","postE","div")

(av.prior <- median(output$priorE))

#outp <- sqldf("select * from output,av.prior where priorE > av.prior and postE > 0 and div > 0.2 order by div DESC")
outp <- output[output$priorE > av.prior,]
outp <- outp[outp$postE > 0,]
outp <- outp[outp$div > 1,]


write.table(outp,"C:/Users/n_shimada/Desktop/RankUpUser.csv",sep=",")