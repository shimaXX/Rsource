#install.packages("igraph")
#library("igraph")

#mat <- matrix(c(1,0,0,2,0,1,1,0,2,1,0,0),nrow=4,byrow=T)
mat <- matrix(floor(runif(500000)+0.3),nrow=5000,byrow=T)
mat.svd <- svd(mat)
cumsum( mat.svd$d/sum(mat.svd$d) )
mat.svd$u
mat.svd$v
mat%*%(mat.svd$v[,1:10])