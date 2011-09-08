library(ncvreg)

library(sampling)
#sampling�g�p���̗����V�[�h
set.seed(5963)
setwd("C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/analysis")
getwd()

##### �f�[�^�̓ǂݍ���
testdata <- read.csv("../data/testdata.csv", header=T)
testdata$fbg <- ifelse(testdata$fbg >= 200, 200, testdata$fbg)
testdata$tg <- ifelse(testdata$tg >= 400, 400, testdata$tg)   
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


#head(tdc)
#�w�ʉ��(���ʂ̎w��)
sex_flag <- c("�j��")##### ���ʂ̎w��(�j���C�����ȊO����͂���ƁC�S�f�[�^)
if(sex_flag == "�j��"){
tdc <- subset(tdc,tdc$sex==1)
X <- cbind(tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube�֐���vector�ł����󂯕t���Ȃ�

}else if(sex_flag == "����"){
tdc <- subset(tdc,tdc$sex==2)
X <- cbind(tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube�֐���vector�ł����󂯕t���Ȃ�
}else{
X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube�֐���vector�ł����󂯕t���Ȃ�
}

#�T���v�����O(cube�@���g�p�A���N���𔽉f����`�Œ����A10����1���T���v�����O)
#�ی��w�����X�N�̗����̊����������ɂȂ�悤��
#X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube�֐���vector�ł����󂯕t���Ȃ�
#p Training data �̊���������
p <- rep(0.9, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
Train <- tdc[sample==0, ]
Valid <- tdc[sample==1, ]

###### outcome variable
out <- c("sbpj")
###### predictor variable
bg_type <- c("fbg")##### �\�����q�̎w��
if (bg_type == "fbg"){
pred <- c(
"age", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hb",
"bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2", "fbg2","hb2")
}else if(bg_type == "hba1c"){
pred <- c(
"age", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "hba1c", "hb",
"bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2","hba1c2", "hb2")
}

cv_ob1 <- cv.ncvreg(do.call(cbind,Train[c(pred)]), Train[,c(out)], alpha=1,family="binomial")
cv_lambda <- cv_ob1$lambda[cv_ob1$cve == min(cv_ob1$cve)]
ncvreg <- ncvreg(do.call(cbind,Train[c(pred)]), Train[,c(out)],penalty=c("SCAD"),family="binomial",lambda=c(cv_lambda,cv_lambda))
