library(ncvreg)

library(sampling)
#sampling使用時の乱数シード
set.seed(5963)
setwd("C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/analysis")
getwd()

##### データの読み込み
testdata <- read.csv("../data/testdata.csv", header=T)
testdata$fbg <- ifelse(testdata$fbg >= 200, 200, testdata$fbg)
testdata$tg <- ifelse(testdata$tg >= 400, 400, testdata$tg)   
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


#head(tdc)
#層別解析(性別の指定)
sex_flag <- c("男性")##### 性別の指定(男性，女性以外を入力すると，全データ)
if(sex_flag == "男性"){
tdc <- subset(tdc,tdc$sex==1)
X <- cbind(tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない

}else if(sex_flag == "女性"){
tdc <- subset(tdc,tdc$sex==2)
X <- cbind(tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない
}else{
X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない
}

#サンプリング(cube法を使用、性年齢比を反映する形で調整、10分の1をサンプリング)
#保健指導リスクの両方の割合が同等になるように
#X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない
#p Training data の割合を決定
p <- rep(0.9, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
Train <- tdc[sample==0, ]
Valid <- tdc[sample==1, ]

###### outcome variable
out <- c("sbpj")
###### predictor variable
bg_type <- c("fbg")##### 予測因子の指定
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

