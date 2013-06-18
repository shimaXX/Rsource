# TODO: Add comment
# 
# Author: n_shimada
###############################################################################

setwd("C:/RW")

library(e1071)
���D<-read.csv("���D.csv")

#�w�K���f���̍\��(cross�͏d�����Ó����ׂ̈̃f�[�^������)
���D����<-svm(�^�U~.,data=���D,probability=TRUE,kernel="radial",cross=1)
���D�\���l<-predict(���D����,newdata=���D,probability=TRUE,decision.values=TRUE)
table(���D�\���l,���D[,1])


#�T�|�[�g�x�N�^�[�C�W���C�K���}, C�̒��o
�T�|�[�g�x�N�^�[<-���D����$SV
�W��<-���D����$coefs
�K���}<-���D����$gamma
C<-���D����$cost

#�\���l�C�\���m���̕\��
�\���l<-round(attr(���D�\���l, "decision.values"),3)
�\���m��<-round(attr(���D�\���l, "probabilities"),3)

t(head(�\���l,8));t(tail(�\���l,8))
t(head(�\���m��,8));t(tail(�\���m��,8))


#�K�E�V�A���J�[�l������l
�\���l�Z�o<-function(x,y){
	�T�|�[�g�x�N�^�[<-���D����$SV
	�W��<-���D����$coefs
	�K���}<-���D����$gamma
	mad<-as.matrix(cbind(x,y))
	mat<-scale(mad)
	zero<-numeric(length(x)*length(�T�|�[�g�x�N�^�[[,1]))
	�J�[�l��<-matrix(zero,nrow=length(x),ncol=length(�T�|�[�g�x�N�^�[[,1]),byrow=F)
	�d�ݕt��<-matrix(zero,nrow=length(y),ncol=length(�T�|�[�g�x�N�^�[[,1]),byrow=F)
	for(i in 1:length(x)){
		�@for(s in 1:length(�T�|�[�g�x�N�^�[[,1])){
			�@�@�J�[�l��[i,s]<-exp(-�K���}*sqrt(t(mat[i,1:2]-�T�|�[�g�x�N�^�[[s,1:2])%*%(mat[i,1:2]
										-�T�|�[�g�x�N�^�[[s,1:2]))^{2})
			�@�@�d�ݕt��[i,s]<-�W��[s]*�J�[�l��[i,s]
			�@}
	}
	�a<-apply(�d�ݕt��,1,sum)
	�\���l<-�a-���D����$rho
}

#�֐�"�\���l�Z�o"�ɂ��\���l
�\���l2<-�\���l�Z�o(���D[,2],���D[,3])

#���C�u�����[�gscatterplot3d�h�̓ǂݍ���
library(scatterplot3d)

#�\���Ȗʂ̃v���b�g
N<-30
x<-seq(-3,3,length=N);y<-seq(-3,3,length=N)
persp(x,y,outer(x,y,�\���l�Z�o),zlab="f(x1,x2)",theta=133,phi=20,
		ticktype = "detailed",xlab="x1",ylab="x2")

#�������̃v���b�g
contour(x,y,outer(x,y,�\���l�Z�o),lwd=1,nlevels=10,labcex=1,
		xlim=c(-3,3),ylim=c(-3,3),method="edge")
par(new=T)
ID<-as.numeric(labels(���D����$SV)[[1]])
���x��<-as.numeric(���D[,1][ID])+1
plot(���D����$SV,xlim=c(-3,3),ylim=c(-3,3),pch=���x��)

#�p�b�P�[�We1071�t���̓������v���b�g
plot(���D����,���D,grid=300,color.palette =terrain.colors,svSymbol=5)

#�`���[�j���O K=10�ŏd�����Ó������s���D
#����ׂɃf�[�^�𕪊����邩��ŗǂ̃`���[�j���O�ꐔ�̒l�͖{�����ƈقȂ邱�Ƃ�����D
�`���[�j���O<-tune.svm(�^�U~.,data=���D,gamma = 2^c(seq(-2,2,0.25)),
		cost = 2^c(seq(-2,2,0.25)))

#�œK�ȕꐔ�ɂ�鐄��
�ŗǃ��f��<-svm(�^�U~.,data=���D,cross=10,gamma=0.2973018, cost=0.25)

#����
����<-summary(�`���[�j���O)

#�O���b�h�T�[�`�̌딻�ʗ���3�����v���b�g
#����ׂɃf�[�^�𕪊����邩��3�����v���b�g���{���ƈقȂ邱�Ƃ�����D
scatterplot3d(����[7]$performances[,1],����[7]$performances[,2],
		����[7]$performances[,3],xlab="gamma",ylab="C",zlab="error rate")