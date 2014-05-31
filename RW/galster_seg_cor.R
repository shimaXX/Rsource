# TODO: Add comment
# 
# Author: n_shimada
###############################################################################


##############################
#相関係数を出す
##############################
library(sqldf)

setwd("C:/RW/galsterBIG")
data <- read.csv("seg_think.csv",row.names=1)

data<-data[,-27]

seg0 <- sqldf("select * from data where seg = 'seg0'")
seg1 <- sqldf("select * from data where seg = 'seg1'")
seg2 <- sqldf("select * from data where seg = 'seg2'")
seg3 <- sqldf("select * from data where seg = 'seg3'")
seg4 <- sqldf("select * from data where seg = 'seg4'")
seg5 <- sqldf("select * from data where seg = 'seg5'")

seg0 <- seg0[,-26]
seg1 <- seg1[,-26]
seg2 <- seg2[,-26]
seg3 <- seg3[,-26]
seg4 <- seg4[,-26]
seg5 <- seg5[,-26]

cor.seg0 <- cor(seg0)
cor.seg1 <- cor(seg1)
cor.seg2 <- cor(seg2)
cor.seg3 <- cor(seg3)
cor.seg4 <- cor(seg4)
cor.seg5 <- cor(seg5)

result <- rbind(cor.seg0,cor.seg1,cor.seg2,cor.seg3,cor.seg4,cor.seg5)

write.table(result, "galster_seg_cor0to5.csv", sep=",")


##############################
#決定木した準備
##############################
setwd("C:/RW/galsterBIG")
data <- read.csv("seg_think.csv",row.names=1)
data <- data[,-27] #flagをカット
data <- data[,-24] #resentをカット
data[,-24] <- data[,-24]+1 #ログイン日数

data <- sqldf("select buy/recent_visit , new/recent_visit , top/recent_visit , cart/recent_visit , entry/recent_visit
 , login/recent_visit , mypage/recent_visit , search/recent_visit , article/recent_visit , comment/recent_visit
 , inquiry/recent_visit , ranking/recent_visit , re_mail/recent_visit , a_review/recent_visit , category/recent_visit
 , w_review/recent_visit , a_favorite/recent_visit , order_list/recent_visit , w_favorite/recent_visit
 , customer_reg/recent_visit , inquiry_article/recent_visit , mail_magazine_del/recent_visit , mail_magazine_reg/recent_visit from data ")

data <- data/data[,-24] #登録からの稼働日数で丸める（これがいいかどうかはおいといて）
data <- data[,-24]



