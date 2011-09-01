##### プログラム保存場所と作業ディレクトリの変更
setwd(""C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/analysis")
getwd()
##### ライブラリの読み込み
library(ggplot2)
##### LASSO package
library(lars)###今回は未使用
library(glmnet)
##### ROC curve package
library(Epi)
library(DiagnosisMed)
library(epicalc)
library(ROCR)

library(sampling)
#sampling使用時の乱数シード
set.seed(5963)

##### データの読み込み
testdata <- read.csv("../data/testdata.csv", header=T)
#head(testdata)


#予測段階で未服薬の人間を抽出
tdc <- subset(testdata, complete.cases(testdata))
tdc <- subset(tdc, tdc$med==0 & tdc$med2==0)

#応答変数のフラグ付け
#3年目における受診勧奨リスクフラグ
tdc$sbpj <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 140, Inf), labels=c(0,1)))-1
tdc$dbpj <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 90, Inf), labels=c(0,1)))-1
tdc$tgj <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 300, Inf), labels=c(0,1)))-1
tdc$hdlj <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 34, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgj <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 126, Inf), labels=c(0,1)))-1
tdc$hba1cj <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 6.1, Inf), labels=c(0,1)))-1

#3年目における保健指導リスクフラグ
tdc$sbpr <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 130, Inf), labels=c(0,1)))-1
tdc$dbpr <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 85, Inf), labels=c(0,1)))-1
tdc$tgr <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 150, Inf), labels=c(0,1)))-1
tdc$hdlr <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 39, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgr <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 100, Inf), labels=c(0,1)))-1
tdc$hba1cr <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 5.2, Inf), labels=c(0,1)))-1

#サンプリング(cube法を使用、性年齢比を反映する形で調整、10分の1をサンプリング)
#保健指導リスクの両方の割合が同等になるように
X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない
p <- rep(0.5, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
Train <- tdc[sample==0, ]
Valid <- tdc[sample==1, ]
##### 
#head(Train)
#head(Valid)
########################
##### over fitting #####
########################
##### full model
sbpj_logit.full <- glm(Train$sbpj ~ .,data = Train[c(1:14,18:29)],family=binomial(link="logit"))
##### stepwise model
sbpj_logit.step <- step(sbpj_logit.full,direction="backward",trace=FALSE)
#summary(sbpj_logit)
##### LASSO model
sbpj_cv_ob1 <- cv.glmnet(do.call(cbind,Train[c(1:14,18:29)]), Train$sbpj, alpha=1,family="binomial")
sbpj_cv_lambda <- sbpj_cv_ob1$lambda[sbpj_cv_ob1$cvm == min(sbpj_cv_ob1$cvm)]
sbpj_glmnet <- glmnet(do.call(cbind,Train[c(1:14,18:29)]), Train$sbpj, lambda=sbpj_cv_lambda,family="binomial")
sbpj_glmnet$coefficients <- Matrix(coef(sbpj_glmnet))

####################
##### outlier ######
####################
sbpj_logit.f.cook <- cooks.distance(sbpj_logit.full)
sbpj_logit.s.cook <- cooks.distance(sbpj_logit.step)
lm.influence(sbpj_glmnet)

#######################
##### validation ######
#######################
##### full model
sbpj_logit_f.vaid <- predict(sbpj_logit.full,newdata = Valid)
sbpj_logit_f.ROC <- data.frame(cbind(Valid$sbpj,sbpj_logit_f.vaid))
colnames(sbpj_logit_f.ROC) <- c("sbpj","score")
Epi::ROC(test = sbpj_logit_f.ROC$score, stat=sbpj_logit_f.ROC$sbpj)
sbpj_logit.f.AUC <- DiagnosisMed::ROC(sbpj_logit_f.ROC$sbpj,sbpj_logit_f.ROC$score)
sbpj_logit.f.AUC$AUC
##### stepwise model
sbpj_logit_s.vaid <- predict(sbpj_logit.step,newdata = Valid)
sbpj_logit_s.ROC <- data.frame(cbind(Valid$sbpj,sbpj_logit_s.vaid))
colnames(sbpj_logit_s.ROC) <- c("sbpj","score")
Epi::ROC(test = sbpj_logit_s.ROC$score, stat=sbpj_logit_s.ROC$sbpj)
sbpj_logit.s.AUC <- DiagnosisMed::ROC(sbpj_logit_s.ROC$sbpj,sbpj_logit_s.ROC$score)
sbpj_logit.s.AUC$AUC
##### LASSO model
sbpj_LASSO.valid <- predict(sbpj_glmnet,newx=do.call(cbind,Valid[c(1:14,18:29)]),family="binomial")
sbpj_LASSO.ROC <- data.frame(cbind(Valid$sbpj,sbpj_LASSO.valid))
colnames(sbpj_LASSO.ROC) <- c("sbpj","score")
Epi::ROC(test = sbpj_LASSO.ROC$score, stat=sbpj_LASSO.ROC$sbpj)
sbpj_LASSO.AUC <- DiagnosisMed::ROC(sbpj_LASSO.ROC$sbpj,sbpj_LASSO.ROC$score)
sbpj_LASSO.AUC$AUC
sbpj_AUC <- data.frame(rbind(sbpj_logit.f.AUC$AUC,sbpj_logit.s.AUC$AUC,sbpj_LASSO.AUC$AUC))
colnames(sbpj_AUC) <- c("L.AUC","AUC","U.AUC")
rownames(sbpj_AUC) <- c("full.model","stepwise.model","LASSO.model")
sbpj_AUC
write.csv(sbpj_AUC,file="sbpj_AUC.csv")
