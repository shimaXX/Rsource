setwd("C:/RW")
data <- read.csv("ci-labo/ci-labo_seg.csv")
nc <- ncol(data)
output <- aggregate(data[,-nc], mean, by = list(data[,nc]))
write.table(output,"ci-labo/ci-labo_output.csv",sep = ",")

data <- data[,-1]

for(i in 1:nc){
  x <- pairwise.t.test(data[,i], data[,nc-1], p.adj="holm")
  fname <- paste("ci-labo/0", i, sep="")
  fname <- paste(fname, "ci-labo_test.csv", sep="")
  write.table(x$p.value, fname,sep =",")
}