# TODO: Add comment
# 
# Author: n_shimada
###############################################################################
setwd("C:/RW/engagement")
dataOrg <- read.csv("Engagement1218-0114.csv")
data <- as.data.frame(dataOrg[,-1])
data <- data[,-4]

#主成分分析用にデータを分割しておく
#分割は購買、コミュニケーション、情報提供とする
#recencyは意図的にとっていない
data.salse <- data[,1:3]
data.comm <- data[,4:7]

#cox-box変換を入れる→毎回変わっては困るので、対数近似する
data.log <- log(data+1)

data.mean <- apply(data,2,mean)
data.sd <- apply(data.log,2,sd)
data.mean["SessionTime"] <- mean(data.log$SessionTime)
data.mean["Payment"] <- mean(data$Payment[data$Payment > 0])
data.mean["Buzz"] <- mean(data$Buzz[data$Buzz > 0])
data.mean["Community"] <- mean(data$Community[data$Community > 0])
data.mean["Nayami"] <- mean(data$Nayami[data$Nayami > 0])
data.mean["BestAnswer"] <- mean(data$BestAnswer[data$BestAnswer > 0])
#data.mean["GiveInformation"] <- mean(data$GiveInformation[data$GiveInformation > 0])
data.mean <- as.data.frame(data.mean)

SessionCnt <-    
  ifelse(data[,1] <= qpois(0.2,data.mean["SessionCnt",]), 1, 
         ifelse(data[,1] > qpois(0.2,data.mean["SessionCnt",]) & data[,1] <= qpois(0.4,data.mean["SessionCnt",]) , 2,
                ifelse(data[,1] > qpois(0.4,data.mean["SessionCnt",]) & data[,1] <= qpois(0.6,data.mean["SessionCnt",]) , 3,
                       ifelse(data[,1] > qpois(0.6,data.mean["SessionCnt",]) & data[,1] <= qpois(0.8,data.mean["SessionCnt",]) , 4,
                              ifelse(data[,1] > qpois(0.8,data.mean["SessionCnt",]) , 5, "er")
                       )
                )
         )
  )

SessionTime <-  	
  ifelse(data.log[,2] <= qnorm(0.2,data.mean["SessionTime",],data.sd["SessionTime"]), 1, 
         ifelse(data.log[,2] > qnorm(0.2,data.mean["SessionTime",],data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.4,data.mean["SessionTime",],data.sd["SessionTime"]) ,2,
                ifelse(data.log[,2] > qnorm(0.4,data.mean["SessionTime",],data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.6,data.mean["SessionTime",],data.sd["SessionTime"]) ,3,
                       ifelse(data.log[,2] > qnorm(0.6,data.mean["SessionTime",],data.sd["SessionTime"]) & data.log[,2] <= qnorm(0.8,data.mean["SessionTime",],data.sd["SessionTime"]) ,4,
                              ifelse(data.log[,2] > qnorm(0.8,data.mean["SessionTime",],data.sd["SessionTime"]) , 5, "er")
                       )
                )
         )
  )


Payment <-		
  ifelse(data[,3] <= qexp(0.2,1/data.mean["Payment",]), 1, 
         ifelse(data[,3] > qexp(0.2,1/data.mean["Payment",]) & data[,3] <= qexp(0.4,1/data.mean["Payment",]) ,2,
                ifelse(data[,3] > qexp(0.4,1/data.mean["Payment",]) & data[,3] <= qexp(0.6,1/data.mean["Payment",]) ,3,
                       ifelse(data[,3] > qexp(0.6,1/data.mean["Payment",]) & data[,3] <= qexp(0.8,1/data.mean["Payment",]) ,4,
                              ifelse(data[,3] > qexp(0.8,1/data.mean["Payment",]) , 5, "er")
                       )
                )
         )
  )


Buzz <-    
  ifelse(data[,4] <= qexp(0.2,1/data.mean["Buzz",]), 1, 
         ifelse(data[,4] > qexp(0.2,1/data.mean["Buzz",]) & data[,4] <= qexp(0.4,1/data.mean["Buzz",]) ,2,
                ifelse(data[,4] > qexp(0.4,1/data.mean["Buzz",]) & data[,4] <= qexp(0.6,1/data.mean["Buzz",]) ,3,
                       ifelse(data[,4] > qexp(0.6,1/data.mean["Buzz",]) & data[,4] <= qexp(0.8,1/data.mean["Buzz",]) ,4,
                              ifelse(data[,4] > qexp(0.8,1/data.mean["Buzz",]) , 5, "er")
                       )
                )
         )
  )


Community <-    
  ifelse(data[,5] <= qexp(0.2,1/data.mean["Community",]), 1, 
         ifelse(data[,5] > qexp(0.2,1/data.mean["Community",]) & data[,5] <= qexp(0.4,1/data.mean["Community",]) ,2,
                ifelse(data[,5] > qexp(0.4,1/data.mean["Community",]) & data[,5] <= qexp(0.6,1/data.mean["Community",]) ,3,
                       ifelse(data[,5] > qexp(0.6,1/data.mean["Community",]) & data[,5] <= qexp(0.8,1/data.mean["Community",]) ,4,
                              ifelse(data[,5] > qexp(0.8,1/data.mean["Community",]) , 5, "er")
                       )
                )
         )
  )


Nayami <-    
  ifelse(data[,6] <= qexp(0.2,1/data.mean["Nayami",]), 1, 
         ifelse(data[,6] > qexp(0.2,1/data.mean["Nayami",]) & data[,6] <= qexp(0.4,1/data.mean["Nayami",]) ,2,
                ifelse(data[,6] > qexp(0.4,1/data.mean["Nayami",]) & data[,6] <= qexp(0.6,1/data.mean["Nayami",]) ,3,
                       ifelse(data[,6] > qexp(0.6,1/data.mean["Nayami",]) & data[,6] <= qexp(0.8,1/data.mean["Nayami",]) ,4,
                              ifelse(data[,6] > qexp(0.8,1/data.mean["Nayami",]) , 5, "er")
                       )
                )
         )
  )


BestAnswer <-    
  ifelse(data[,7] <= qexp(0.2,1/data.mean["BestAnswer",]), 1, 
         ifelse(data[,7] > qexp(0.2,1/data.mean["BestAnswer",]) & data[,7] <= qexp(0.4,1/data.mean["BestAnswer",]) ,2,
                ifelse(data[,7] > qexp(0.4,1/data.mean["BestAnswer",]) & data[,7] <= qexp(0.6,1/data.mean["BestAnswer",]) ,3,
                       ifelse(data[,7] > qexp(0.6,1/data.mean["BestAnswer",]) & data[,7] <= qexp(0.8,1/data.mean["BestAnswer",]) ,4,
                              ifelse(data[,7] > qexp(0.8,1/data.mean["BestAnswer",]) , 5, "er")
                       )
                )
         )
  )

########################
#■binデータのbind
########################
data.bin.salse <- cbind(as.integer(SessionCnt), as.integer(SessionTime), as.integer(Payment))
data.bin.comm <-  cbind(as.integer(Buzz), as.integer(Community), as.integer(Nayami),
                        as.integer(BestAnswer))
#data.bin.giveInfo <- cbind(as.integer(GiveInformation))

########################
#■元データに主成分分析を実行
########################
data.pc.salse <- prcomp(data.salse, scale=TRUE)
data.pc.comm <- prcomp(data.comm, scale=TRUE)

summary(data.pc.salse)
summary(data.pc.comm)

#固有ベクトルの表示
data.pc.salse$rotation
data.pc.comm$rotation

#固有値の算出
data.pc.salse$sdev^2
data.pc.comm$sdev^2

#主成分負荷量の計算
t( data.pc.salse$sdev * t( data.pc.salse$rotation ) )[, drop = FALSE]
t( data.pc.comm$sdev * t( data.pc.comm$rotation ) )[, drop = FALSE]

#####################
#■得点を求める
#####################
#第一主成分のベクトルのみ取得
vecter.salse <- as.data.frame(data.pc.salse$rotation[,1])
vecter.comm <- as.data.frame(data.pc.comm$rotation[,1])

#マイナス値があった場合は最小値を取得
min.salse <- if(length(vecter.salse[vecter.salse<0])>0){
                min(vecter.salse)
              }else{
                0
              } 
min.comm <- if(length(vecter.comm[vecter.comm<0])>0){
                min(vecter.comm)
              }else{
                0
              } 

#マイナス値があった場合は最小値が0になるようにする
vecter.salse <- vecter.salse - min.salse
vecter.comm <- vecter.comm - min.comm

#重みを前一か月のデータを使用する
vecter.salse <- c(0.2493636, 0.6576405, 0.7108634)
vecter.comm <- c(0.1216034, 0.6913071, 0.1376758, 0.6988222)

#重みの合計を1に調節
vecter.salse <- as.matrix(vecter.salse/sum(vecter.salse))
vecter.comm <- as.matrix(vecter.comm/sum(vecter.comm))

#行列積の計算のために、binを切ったデータの型をmatrixに変換
dat.salse <- as.matrix(data.bin.salse) 
dat.comm <- as.matrix(data.bin.comm) 

#行列積の計算
result.salse <- dat.salse %*% vecter.salse
result.comm <- dat.comm %*% vecter.comm

#####################
#■エンゲージメント算出
#  口コミのエンゲージメント = (binの値×μ^2 + 平均購買額)×10とする
#  購買カテゴリのエンゲージメント = bin×μ^2 + 平均購買額:μ
#  購買のエンゲ+口コミのエンゲ
#  マクスウェル方程式に当てはめる
#考え方の注：
#　平均購買額（購入者のみの平均）をかけることで、その期間の絶対値を反映させる
#####################
#購買のエンゲ+口コミのエンゲ
engagement <- ((result.salse-1)^3 + ((result.comm-1)*10)^3)*(median(data$Payment[data$Payment > 0]))^2

k = 1.381e-23                       # ボルツマン定数[J/K]
Na = 6.022e+23                      # アボガドロ定数[1/mol]
weight = 197                        #質量はキセノンXeの132を使用
                                    #やっぱり金の197にする

weight = weight/1000                # kgに直す
N = length(weight)
mm = weight/Na
#ma = sqrt(k*abst/mm)

#分子の平均速度の算出（単位はkm/s）
v <- sqrt(8*k*engagement/pi/mm)/1000 

result.v <- cbind(dataOrg["uid"], v)

#######################
#■出力
#######################
#購買
write.table(result.salse,"C:/Users/n_shimada/Desktop/eng/0122-0128/rankPrior_salse_0122.csv",sep=",")
write.table(dat.salse,"C:/Users/n_shimada/Desktop/eng/0122-0128/binPrior_salse_0122.csv",sep=",")

#コミュニケーション
write.table(result.comm,"C:/Users/n_shimada/Desktop/eng/0122-0128/rankPrior_comm_0122.csv",sep=",")
write.table(dat.comm,"C:/Users/n_shimada/Desktop/eng/0122-0128/binPrior_comm_0122.csv",sep=",")

#総合点
write.table(result.v,"C:/Users/n_shimada/Desktop/eng/0122-0128/rankPrior_total_0122.csv",sep=",")