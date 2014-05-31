setwd("D:/RW/stan/rats/")
#http://cran.r-project.org/web/packages/LiblineaR/LiblineaR.pdf

#install.packages('LiblineaR')
library(LiblineaR)

data(iris)
attach(iris)
x=iris[,1:4]
y=factor(iris[,5])
train=sample(1:dim(iris)[1],100)

xTrain=x[train,]
xTest=x[-train,]
yTrain=y[train]
yTest=y[-train]

# Center and scale data
s=scale(xTrain,center=TRUE,scale=TRUE)

# Logistic Regression
t=0

# Find the best model with the best cost parameter via 10-fold cross-validations
#• 0 – L2-regularized logistic regression
#• 1 – L2-regularized L2-loss support vector classiﬁcation (dual)
#• 2 – L2-regularized L2-loss support vector classiﬁcation (primal)
#• 3 – L2-regularized L1-loss support vector classiﬁcation (dual)
#• 4 – multi-class support vector classiﬁcation by Crammer and Singer
#• 5 – L1-regularized L2-loss support vector classiﬁcation
#• 6 – L1-regularized logistic regression
#• 7 – L2-regularized logistic regression (dual)

tryTypes=c(0:7)
tryCosts=c(1000,100,10,1,0.1,0.01,0.001)
bestCost=NA
bestAcc=0
bestType=NA
for(ty in tryTypes){
  for(co in tryCosts){
    acc=LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,cross=10,verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost=co
      bestAcc=acc
      bestType=ty
    }
  }
}

cat("Best model type is:",bestType,"\n")
cat("Best cost is:",bestCost,"\n")
cat("Best accuracy is:",bestAcc,"\n")

# Re-train best model with best cost value.
m=LiblineaR(data=s,labels=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)

# Scale the test data
s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))

# Make prediction
pr=FALSE
if(bestType==0 | bestType==7) pr=TRUE
p=predict(m,s2,proba=pr,decisionValues=TRUE)

# Display confusion matrix
res=table(p$predictions,yTest)
print(res)

# Compute Balanced Classification Rate
BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
print(BCR)





# Example of the use of a sparse matrix:
install.packages('SparseM')
library(SparseM)

# Sparsifying the iris dataset:
iS=apply(iris[,1:4],2,function(a){a[a<quantile(a,probs=c(0.25))]=0;return(a)})
irisSparse<-as.matrix.csr(iS)

# Applying a similar methodology as above:
xTrain=irisSparse[train,]
xTest=irisSparse[-train,]

# Re-train best model with best cost value.
m=LiblineaR(data=xTrain,labels=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)

# Make prediction
p=predict(m,xTest,proba=pr,decisionValues=TRUE)

# Display confusion matrix
res=table(p$predictions,yTest)
print(res)