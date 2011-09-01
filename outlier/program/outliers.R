setwd("C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/outlier")
getwd()
#################   データ読み込み    #######################
df <- read.csv("../data/Q_data_R.csv")
#head(df)
#names(df)
#####################   変数名の置換   #########################
names(df) <- c("ID", "sex", "birth", "support","web_letter","supporter",
"shokai_inter","implement","censor","point","pre_weight","pre_fukui",
"post_weight","post_fukui",
"exercise1","exercise2","exercise3","exercise4","exercise5",
"exercise6","exercise7","exercise8","exercise9","exercise10",
"diet1","diet2","diet3","diet4","diet5","diet6","diet7",
"diet8","diet9","diet10")
#names(df)
###################　　　　水準の置換　　　　　　　　　#########################
#head(df)
myfunc <- function(a, myn){
        mydata <- c()
        n <- length(a)
        n2 <- length(myn)
        for(i in 1:n){
                for(j in 1:n2){
                        if(a[i] == levels(a)[j]) mydata[i] <- myn[j]
                }
        }
        mydata <- as.factor(mydata)
}
df$sex <- myfunc(df$sex,c("female","male"))#性別の水準名変更
df$support <- myfunc(df$support,c("active","motive"))#支援レベルの水準名変更
df$web_letter <- myfunc(df$web_letter,c("Web","letter"))#web or letter
df$supporter <- myfunc(df$supporter,c("gonoi","nakazato","noguchi"))#支援サポーター名の水準名変更
df$censor <- myfunc(df$censor,c(1,2,3,0))
###運動系メニュー or 食事系メニュー　の行動をカウントする
####データが格納されているものは，1　ないものは0の2値データに変換
df$exercise1 <- ifelse(as.character(df$exercise1) != "",1, 0)
df$exercise2 <- ifelse(as.character(df$exercise2) != "",1, 0)
df$exercise3 <- ifelse(as.character(df$exercise3) != "",1, 0)
df$exercise4 <- ifelse(as.character(df$exercise4) != "",1, 0)
df$exercise5 <- ifelse(as.character(df$exercise5) != "",1, 0)
df$exercise6 <- ifelse(as.character(df$exercise6) != "",1, 0)
df$exercise7 <- ifelse(as.character(df$exercise7) != "",1, 0)
df$exercise8 <- ifelse(as.character(df$exercise8) != "",1, 0)
df$exercise9 <- ifelse(as.character(df$exercise9) != "",1, 0)
df$exercise10 <- ifelse(as.character(df$exercise10) != "",1, 0)
df$diet1 <- ifelse(as.character(df$diet1) != "",1, 0)
df$diet2 <- ifelse(as.character(df$diet2) != "",1, 0)
df$diet3 <- ifelse(as.character(df$diet3) != "",1, 0)
df$diet4 <- ifelse(as.character(df$diet4) != "",1, 0)
df$diet5 <- ifelse(as.character(df$diet5) != "",1, 0)
df$diet6 <- ifelse(as.character(df$diet6) != "",1, 0)
df$diet7 <- ifelse(as.character(df$diet7) != "",1, 0)
df$diet8 <- ifelse(as.character(df$diet8) != "",1, 0)
df$diet9 <- ifelse(as.character(df$diet9) != "",1, 0)
df$diet10 <- ifelse(as.character(df$diet10) != "",1, 0)
df$exercise <- df$exercise1 + df$exercise2 + df$exercise3 + df$exercise4 + df$exercise5 +
df$exercise6 + df$exercise7 + df$exercise8 + df$exercise9 + df$exercise10
df$diet <- df$diet1 + df$diet2 + df$diet3 + df$diet4 + df$diet5 +
df$diet6 + df$diet7 + df$diet8 + df$diet9 + df$diet10

##############変数の削除##############
#names(df)
df <- df[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,35,36)]

############# 生年月日に欠測値のあるデータにNAを代入 #############
df$birth <- ifelse(df$birth == "", NA , as.character(df$birth))
##########生年月日の欠測値データを除去#########
df <- df[complete.cases(df$birth),]
dtparts <- strsplit(as.character(df$birth)," ")
df$birth <- do.call("rbind", dtparts)[,1]
#############  生年月日から年齢を算出  ##############
df$age <- as.numeric(floor((as.Date("2010-04-01") - as.Date(df$birth))/ 365.25)) #基準日を2010年3月1日に設定

########        解析対象   ###########
############ 状態ステータスが評価完了したもののみデータを抽出  ##############
df <- subset(df, df$censor == 0)
######### 体重に欠測値のあるデータを除去 #############
df <- df[complete.cases(df$pre_weight) & complete.cases(df$post_weight),]
########　初回面接担当の影響の検討
#### データ読み込み
df2 <- read.csv("../data/djmensetsusya110513.csv")
names(df2) <- c("ID","siensya_name","siensya")
df2$siensya_name <- myfunc(df2$siensya_name,c("sakaguchi","ichihashi","ikuo","tomioka","noguchi"))
sortlist <- order(df$ID)
df <- df[sortlist,]
sortlist <- order(df2$ID)
df2 <- df2[sortlist,]
merge_df <- merge(df,df2,by = "ID",all=T)
##欠測値の除去
df <- subset(merge_df, complete.cases(merge_df))
############### 変数の標準化が行われるため，カテゴリカル変数は要ダミー変数化 #############
########### ダミー変数の作成
############ supporter ######################
#as.factor(df$supporter)
df$sup_gonoi <- as.integer(df$supporter == "gonoi")
df$sup_noguchi <- as.integer(df$supporter == "noguchi")
df$sup_nakazato <- as.integer(df$supporter == "nakazato")
############ siensya_name ######################
#as.factor(df$siensya_name)
df$sien_ichihasi <- as.integer(df$siensya_name == "ichihashi")
df$sien_ikuo <- as.integer(df$siensya_name == "ikuo")
df$sien_noguchi <- as.integer(df$siensya_name == "noguchi")
df$sien_sakaguchi <- as.integer(df$siensya_name == "sakaguchi")
df$sien_tomioka <- as.integer(df$siensya_name == "tomioka")
################ 説明変数の行列 ###################
xvar = 13#説明変数の数を指定
############# パラメータ推定値は，以下の変数の順番で出力される ##############
df$x<- matrix(c(df$sex,df$pre_weight,df$age,df$exercise,df$diet,
df$sup_gonoi,df$sup_noguchi,df$sup_nakazato,
df$sien_ichihasi,df$sien_ikuo,df$sien_noguchi,df$sien_sakaguchi,df$sien_tomioka),nrow(df),xvar)

############ Cook's distance ###############
lm <- lm(df$post_weight ~ ., data = data.frame(df$x))
library(ggplot2)
plot(cooks.distance(lm))

############ influence.measures
influence.measures(lm)
############ dfbeta & dfbetas
dfbeta(lm);plot(dfbeta(lm))
dfbetas(lm);plot(dfbetas(lm))
dffits(lm);plot(dffits(lm))
############ covratio
covratio(lm);plot(covratio(lm))

#ロバストパッケージ