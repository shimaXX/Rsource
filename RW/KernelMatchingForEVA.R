setwd("C:/RW/causal_inference/")
RowData <- read.csv("EVAVerbCount0115.csv")
data <- 

logi <- glm(treat ~age + educ + black + hisp + married + nodegr +
              re74 + re75 + u74 + u75, family=binomial, data=lalonde)

kmy <- lalonde$re74
ivec1 <- lalonde$treat
estp <- logi$fitted
km <- cbind(kmy, estp, ivec1)
km1 <- subset(km, ivec1==1)  #処置群のみ抽出
km2 <- subset(km, ivec1==0)  #対象群のみ抽出
km1x <- km1[,2]
km1y <- km1[,1]
km2x <- km2[,2]
km2y <- km2[,1]
bw1 <- 1.06*(nrow(km1))^(-0.2)*sd(km1x) #最適バンド幅の指定
bw2 <- 1.06*(nrow(km2))^(-0.2)*sd(km2x)
esty1 <- ksmooth(x=km1x, y=km1y, kernel="normal", bandwidth=bw1, x.points=km2x)
esty0 <- ksmooth(x=km2x, y=km2y, kernel="normal", bandwidth=bw2, x.points=km1x)
