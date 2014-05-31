setwd("C:/RW")
data <- read.csv("test/EVAVerbCount1212.csv")
data <- as.matrix(data[,-1])
weight <- read.csv("test/EVAweight.csv")
weight <- as.matrix(as.numeric(weight))

energy <- data %*% weight

head(energy)

write.table(energy, "C:/Users/n_shimada/Desktop/EvaEnergy1212.csv",sep =",")