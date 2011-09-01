##### �v���O�����ۑ��ꏊ�ƍ�ƃf�B���N�g���̕ύX
setwd(""C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/analysis")
getwd()
##### ���C�u�����̓ǂݍ���
library(ggplot2)
##### LASSO package
library(lars)###����͖��g�p
library(glmnet)
##### ROC curve package
library(Epi)
library(DiagnosisMed)
library(epicalc)
library(ROCR)

library(sampling)
#sampling�g�p���̗����V�[�h
set.seed(5963)

##### �f�[�^�̓ǂݍ���
testdata <- read.csv("../data/testdata.csv", header=T)
#head(testdata)


#�\���i�K�Ŗ�����̐l�Ԃ𒊏o
tdc <- subset(testdata, complete.cases(testdata))
tdc <- subset(tdc, tdc$med==0 & tdc$med2==0)

#�����ϐ��̃t���O�t��
#3�N�ڂɂ������f�������X�N�t���O
tdc$sbpj <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 140, Inf), labels=c(0,1)))-1
tdc$dbpj <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 90, Inf), labels=c(0,1)))-1
tdc$tgj <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 300, Inf), labels=c(0,1)))-1
tdc$hdlj <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 34, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgj <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 126, Inf), labels=c(0,1)))-1
tdc$hba1cj <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 6.1, Inf), labels=c(0,1)))-1

#3�N�ڂɂ�����ی��w�����X�N�t���O
tdc$sbpr <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 130, Inf), labels=c(0,1)))-1
tdc$dbpr <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 85, Inf), labels=c(0,1)))-1
tdc$tgr <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 150, Inf), labels=c(0,1)))-1
tdc$hdlr <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 39, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgr <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 100, Inf), labels=c(0,1)))-1
tdc$hba1cr <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 5.2, Inf), labels=c(0,1)))-1

#�T���v�����O(cube�@���g�p�A���N���𔽉f����`�Œ����A10����1���T���v�����O)
#�ی��w�����X�N�̗����̊����������ɂȂ�悤��
X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube�֐���vector�ł����󂯕t���Ȃ�
#p Training data �̊���������
p <- rep(0.9, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
Train <- tdc[sample==0, ]
Valid <- tdc[sample==1, ]
##### 
#head(Train)
#head(Valid)
########################
##### over fitting #####
########################
func <- function(TrainY,ValidY){
##### full model
logit.full <- glm(TrainY ~ .,data = Train[c(1:14,18:29)],family=binomial(link="logit"))
##### stepwise model
logit.step <- step(logit.full,direction="backward",trace=FALSE)
##### LASSO model
cv_ob1 <- cv.glmnet(do.call(cbind,Train[c(1:14,18:29)]), TrainY, alpha=1,family="binomial")
cv_lambda <- cv_ob1$lambda[cv_ob1$cvm == min(cv_ob1$cvm)]
glmnet <- glmnet(do.call(cbind,Train[c(1:14,18:29)]), TrainY, lambda=cv_lambda,family="binomial")
glmnet$coefficients <- Matrix(coef(glmnet))

####################
##### outlier ######
####################
logit.f.cook <- cooks.distance(logit.full)
logit.s.cook <- cooks.distance(logit.step)

#######################
##### validation ######
#######################
##### full model
logit_f.vaid <- predict(logit.full,newdata = Valid)
logit_f.ROC <- data.frame(cbind(Valid$Y,logit_f.vaid))
colnames(logit_f.ROC) <- c("Y","score")
Epi::ROC(test = logit_f.ROC$score, stat=logit_f.ROC$Y)
logit.f.AUC <- DiagnosisMed::ROC(logit_f.ROC$Y,logit_f.ROC$score)
logit.f.AUC$AUC
##### stepwise model
logit_s.vaid <- predict(logit.step,newdata = Valid)
logit_s.ROC <- data.frame(cbind(ValidY,logit_s.vaid))
colnames(logit_s.ROC) <- c("Y","score")
Epi::ROC(test = logit_s.ROC$score, stat=logit_s.ROC$Y)
logit.s.AUC <- DiagnosisMed::ROC(logit_s.ROC$Y,logit_s.ROC$score)
logit.s.AUC$AUC
##### LASSO model
LASSO.valid <- predict(glmnet,newx=do.call(cbind,Valid[c(1:14,18:29)]),family="binomial")
LASSO.ROC <- data.frame(cbind(Valid$Y,LASSO.valid))
colnames(LASSO.ROC) <- c("Y","score")
Epi::ROC(test = LASSO.ROC$score, stat=LASSO.ROC$Y)
LASSO.AUC <- DiagnosisMed::ROC(LASSO.ROC$Y,LASSO.ROC$score)
LASSO.AUC$AUC
AUC <- data.frame(rbind(logit.f.AUC$AUC,logit.s.AUC$AUC,LASSO.AUC$AUC))
colnames(AUC) <- c("L.AUC","AUC","U.AUC")
rownames(AUC) <- c("full.model","stepwise.model","LASSO.model")
return(AUC)
}

func(Train$sbpj,Valid$sbpj)

