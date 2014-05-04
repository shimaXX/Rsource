# TODO: Add comment
# 
# Author: n_shimada
###############################################################################

setwd("C:/RW")

library(arules)

data <- read.csv("ci-labo_test.csv",row.names=1)
data <- as.matrix(data)
trans <- as(data, "transactions")
summary(trans)
inspect(trans)

summary(trans)

inspect(trans)

itemFrequency(trans)
itemFrequencyPlot(trans, ylim = c(0,1))
itemFrequency(trans)
rules <- apriori(trans, parameter = list(maxlen =4, support = 0.04, confidence = 0.50, ext =TRUE))

summary(rules)

inspect(rules)
