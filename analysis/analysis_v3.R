##### �v���O�����ۑ��ꏊ�ƍ�ƃf�B���N�g���̕ύX
setwd("C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/analysis")
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


#head(tdc)
###### outcome variable
out <- c("sbpj")
###### predictor variable
pred <- c(
"age", "sex", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hba1c", "hb",
"bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2", "fbg2", "hba1c2", "hb2")
##### or
#pred <- c(1:14,18:29)
#head(Train[c(pred)])

func <- function(out,pred){

########################
##### over fitting #####
########################

##### full model
logit.full <- glm(Train[,c(out)] ~ .,data = Train[c(pred)],family=binomial(link="logit"))
##### stepwise model
logit.step <- step(logit.full,direction="backward",trace=FALSE)
##### LASSO model
cv_ob1 <- cv.glmnet(do.call(cbind,Train[c(pred)]), Train[,c(out)], alpha=1,family="binomial")
cv_lambda <- cv_ob1$lambda[cv_ob1$cvm == min(cv_ob1$cvm)]
glmnet <- glmnet(do.call(cbind,Train[c(pred)]), Train[,c(out)], lambda=cv_lambda,family="binomial")
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
logit_f.valid <- predict(logit.full,newdata = Valid)
logit_f.ROC <- data.frame(cbind(Valid[,c(out)],logit_f.valid))
colnames(logit_f.ROC) <- c("Y","score")
#Epi::ROC(test = logit_f.ROC$score, stat=logit_f.ROC$Y)
pred.f.valid <- prediction(logit_f.ROC$score,logit_f.ROC$Y)
#plot(performance(prediction(logit_f.ROC$score,logit_f.ROC$Y),"tpr","fpr"),colorize=T)
logi.f.AUC <- performance(pred.f.valid,"auc")@y.values[[1]]
#logit.f.AUC <- DiagnosisMed::ROC(logit_f.ROC$Y,logit_f.ROC$score)
##### stepwise model
logit_s.valid <- predict(logit.step,newdata = Valid)
logit_s.ROC <- data.frame(cbind(Valid[,c(out)],logit_s.valid))
colnames(logit_s.ROC) <- c("Y","score")
#Epi::ROC(test = logit_s.ROC$score, stat=logit_s.ROC$Y)
pred.s.valid <- prediction(logit_s.ROC$score,logit_s.ROC$Y)
logi.s.AUC <- performance(pred.s.valid,"auc")@y.values[[1]]
#logit.s.AUC <- DiagnosisMed::ROC(logit_s.ROC$Y,logit_s.ROC$score)
##### LASSO model
LASSO.valid <- predict(glmnet,newx=do.call(cbind,Valid[c(pred)]),family="binomial")
LASSO.ROC <- data.frame(cbind(Valid[,c(out)],LASSO.valid))
colnames(LASSO.ROC) <- c("Y","score")
#Epi::ROC(test = LASSO.ROC$score, stat=LASSO.ROC$Y)
pred.L.valid <- prediction(LASSO.ROC$score,LASSO.ROC$Y)
LASSO.AUC <- performance(pred.L.valid,"auc")@y.values[[1]]
#LASSO.AUC <- DiagnosisMed::ROC(LASSO.ROC$Y,LASSO.ROC$score)
AUC <- data.frame(rbind(logi.f.AUC,logi.s.AUC,LASSO.AUC))
colnames(AUC) <- c("AUC")
rownames(AUC) <- c("full.model","stepwise.model","LASSO.model")

png("graph.png")
par(mfrow=c(2,2))
logi.f.ROC <- Epi::ROC(test = logit_f.ROC$score, stat=logit_f.ROC$Y,plot=c("ROC"),main="full.model")
logi.s.ROC <- Epi::ROC(test = logit_s.ROC$score, stat=logit_s.ROC$Y,plot=c("ROC"),main="stepwise.model")
LASSO.ROC <- Epi::ROC(test = LASSO.ROC$score, stat=LASSO.ROC$Y,plot=c("ROC"),main="LASSO.model")
dev.off()

logi.f.mx <- max(logi.f.ROC$res[, 1] + logi.f.ROC$res[, 2])#���x�C���ٓx�̘a�̍ő�l
logi.f.mhv <- which((logi.f.ROC$res[, 1] + logi.f.ROC$res[, 2]) == logi.f.mx)#���x�E���ٓx�̘a�̍ő�l��index
logi.f.Sens <- logi.f.ROC$res[logi.f.mhv,1]#Sensitivity
logi.f.Spec <- logi.f.ROC$res[logi.f.mhv,1]#Specificity
logi.f.res <- cbind(logi.f.Sens,logi.f.Spec)

logi.s.mx <- max(logi.s.ROC$res[, 1] + logi.s.ROC$res[, 2])
logi.s.mhv <- which((logi.s.ROC$res[, 1] + logi.s.ROC$res[, 2]) == logi.s.mx)
logi.s.Sens <- logi.s.ROC$res[logi.s.mhv,1]#Sensitivity
logi.s.Spec <- logi.s.ROC$res[logi.s.mhv,1]#Specificity
logi.s.res <- cbind(logi.s.Sens,logi.s.Spec)

LASSO.mx <- max(LASSO.ROC$res[, 1] + LASSO.ROC$res[, 2])
LASSO.mhv <- which((LASSO.ROC$res[, 1] + LASSO.ROC$res[, 2]) == LASSO.mx)
LASSO.Sens <- LASSO.ROC$res[LASSO.mhv,1]#Sensitivity
LASSO.Spec <- logi.s.ROC$res[LASSO.mhv,1]#Specificity
LASSO.res <- cbind(LASSO.Sens,LASSO.Spec)

Sens.Spec <- rbind(logi.f.res,logi.s.res,LASSO.res)
colnames(Sens.Spec) <- c("Sensitivity","Specificity")
rownames(Sens.Spec) <- c("full.model","stepwise.model","LASSO.model")

res <- list(AUC,Sens.Spec)
return(res)
}

# outcome
res <- func(out,pred)
write.csv(res,"output.csv")
