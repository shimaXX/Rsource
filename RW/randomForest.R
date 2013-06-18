# TODO: �����_���t�H���X�g����
# 
# �쐬��: nahki
###############################################################################



#install.packages("randomForest")
library(randomForest)
library(mlbench)

#�f�[�^�̎擾
setwd("C:/RW/galsterBIG")		# ���[�N�X�y�[�X�̐ݒ�
#rowdata<-read.csv("galster_classed.csv")	#data�̓ǂݍ���
data <- read.csv("galster_per_day.csv")
#data(spam7)			# �f���f�[�^�̓ǂݍ���
rowdata<- data[,-1]

seg0 <- sqldf(" select * from rowdata where seg='seg0' ")
seg1 <- sqldf(" select * from rowdata where seg='seg1' ")
seg2 <- sqldf(" select * from rowdata where seg='seg2' ")
seg3 <- sqldf(" select * from rowdata where seg='seg3' ")
seg4 <- sqldf(" select * from rowdata where seg='seg4' ")
seg5 <- sqldf(" select * from rowdata where seg='seg5' ")

nr.seg0 <- nrow(seg0)
nr.seg1 <- nrow(seg1)
nr.seg2 <- nrow(seg2)
nr.seg3 <- nrow(seg3)
nr.seg4 <- nrow(seg4)
nr.seg5 <- nrow(seg5)

n0 <-1:nr.seg0
n1 <-1:nr.seg1
n2 <-1:nr.seg2
n3 <-1:nr.seg3
n4 <-1:nr.seg4

seg0.smp <- sample(n0,nr.seg5)
seg1.smp <- sample(n1,nr.seg5)
seg2.smp <- sample(n2,nr.seg5)
seg3.smp <- sample(n3,nr.seg5)
seg4.smp <- sample(n4,nr.seg5)

seg0 <- seg0[seg0.smp,]
seg1 <- seg1[seg1.smp,]
seg2 <- seg2[seg2.smp,]
seg3 <- seg3[seg3.smp,]
seg4 <- seg4[seg4.smp,]

rowdata <- as.data.frame(rbind(seg0,seg1,seg2,seg3,seg4,seg5))
#rowdata <- as.data.frame(rbind(seg1,seg2,seg3,seg4,seg5))

nr <- nrow(rowdata)

nr.s <- as.integer(nr/2)

even.n<- 2*(1:nr.s)
BC.train<-rowdata[even.n,-1]
BC.test<-rowdata[-even.n,-1]

#�����_���V�[�h
set.seed(20)

#�f�[�^�̊i�[
data.train<-BC.train
data.test<-BC.test

#�񐔂��擾
nc<-ncol(data.test)

#�����_���t�H���X�g�̎��s
#na.omit�Ńf�[�^�Z�b�g���̌����l������
data.rf<-randomForest(seg~., data=data.train, na.action="na.omit")	
print(data.rf)

#�I�u�W�F�N�g�ɋL�^����Ă��鍀�ڂ��m�F
summary(data.rf)

#��L�ōs����RF���ǂ̂悤�Ȏ�ށi��A�A����etc�j�Ȃ̂����m�F
data.rf$type

#�p�����؂̐��ƌ딻�ʗ��Ƃ̊֌W���O���t�\��
plot(data.rf)

#���ʁE��A�ɂ�����ϐ��̏d�v�x�i�W�j�W���j��\��
data.rf$importance

#�W�j�W���ɂ��ϐ��̏d�v�x���v���b�g
varImpPlot(data.rf)

#�G���[����̂���data.test[[1]]�̈��q������data.rf��xlevels[[1]]�Ɋi�[
data.rf$forest$xlevels[[1]]<-levels(data.test[[1]])

#�V�����f�[�^�ɑ΂��Ĕ��ʁE��A���s��
data.rfp<-predict(data.rf, data.test[,-nc])
(data.rft<-table(data.test[,nc], data.rfp))

#�딻�ʗ��̌v�Z
round(1-sum(diag(data.rft))/sum(data.rft),3)