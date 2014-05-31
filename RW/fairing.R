# TODO: Add comment
# 
# Author: n_shimada
###############################################################################



#library(som)
#library(mvpart)

setwd("C:/RW/galsterBIG")
data <- read.table("galster_verb_count.txt",sep="\t")

verb <- read.csv("galster_verb.csv",header=F)
usr <- read.csv("galster_usr.csv",header=F)
day <- read.table("galster_day.txt",header=F)

NoUsr <- nrow(usr)
Noverb <- nrow(verb)
NoRow <- nrow(data)

#データの初期化
Uroop <- 1
myCol <- 1

#データの箱を準備
result <- matrix(0, NoUsr, Noverb)	#ユーザ名を格納しないもの
resultInName <- matrix(0, NoUsr, Noverb+1)	#ユーザ名を格納するもの


for(u in 1:NoUsr){
	while(data[Uroop,1] == usr[u,1]){
		for(i in 1:Noverb){
			if(data[Uroop,2] == verb[i,1]){
				result[u, i] <- data[Uroop,3]
				break
			}
		}
		Uroop <- Uroop + 1 
	}
}

resultInName <- cbind(usr[,1],result)

write.table(result, "galster_data.txt")
write.table(resultInName, "galster_class_data.txt")