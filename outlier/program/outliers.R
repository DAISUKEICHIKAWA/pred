setwd("C:/Users/Arai_lenovo/Documents/laboratory/HCC/work/outlier")
getwd()
#################   �f�[�^�ǂݍ���    #######################
df <- read.csv("../data/Q_data_R.csv")
#head(df)
#names(df)
#####################   �ϐ����̒u��   #########################
names(df) <- c("ID", "sex", "birth", "support","web_letter","supporter",
"shokai_inter","implement","censor","point","pre_weight","pre_fukui",
"post_weight","post_fukui",
"exercise1","exercise2","exercise3","exercise4","exercise5",
"exercise6","exercise7","exercise8","exercise9","exercise10",
"diet1","diet2","diet3","diet4","diet5","diet6","diet7",
"diet8","diet9","diet10")
#names(df)
###################�@�@�@�@�����̒u���@�@�@�@�@�@�@�@�@#########################
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
df$sex <- myfunc(df$sex,c("female","male"))#���ʂ̐������ύX
df$support <- myfunc(df$support,c("active","motive"))#�x�����x���̐������ύX
df$web_letter <- myfunc(df$web_letter,c("Web","letter"))#web or letter
df$supporter <- myfunc(df$supporter,c("gonoi","nakazato","noguchi"))#�x���T�|�[�^�[���̐������ύX
df$censor <- myfunc(df$censor,c(1,2,3,0))
###�^���n���j���[ or �H���n���j���[�@�̍s�����J�E���g����
####�f�[�^���i�[����Ă�����̂́C1�@�Ȃ����̂�0��2�l�f�[�^�ɕϊ�
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

##############�ϐ��̍폜##############
#names(df)
df <- df[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,35,36)]

############# ���N�����Ɍ����l�̂���f�[�^��NA���� #############
df$birth <- ifelse(df$birth == "", NA , as.character(df$birth))
##########���N�����̌����l�f�[�^������#########
df <- df[complete.cases(df$birth),]
dtparts <- strsplit(as.character(df$birth)," ")
df$birth <- do.call("rbind", dtparts)[,1]
#############  ���N��������N����Z�o  ##############
df$age <- as.numeric(floor((as.Date("2010-04-01") - as.Date(df$birth))/ 365.25)) #�����2010�N3��1���ɐݒ�

########        ��͑Ώ�   ###########
############ ��ԃX�e�[�^�X���]�������������̂̂݃f�[�^�𒊏o  ##############
df <- subset(df, df$censor == 0)
######### �̏d�Ɍ����l�̂���f�[�^������ #############
df <- df[complete.cases(df$pre_weight) & complete.cases(df$post_weight),]
########�@����ʐڒS���̉e���̌���
#### �f�[�^�ǂݍ���
df2 <- read.csv("../data/djmensetsusya110513.csv")
names(df2) <- c("ID","siensya_name","siensya")
df2$siensya_name <- myfunc(df2$siensya_name,c("sakaguchi","ichihashi","ikuo","tomioka","noguchi"))
sortlist <- order(df$ID)
df <- df[sortlist,]
sortlist <- order(df2$ID)
df2 <- df2[sortlist,]
merge_df <- merge(df,df2,by = "ID",all=T)
##�����l�̏���
df <- subset(merge_df, complete.cases(merge_df))
############### �ϐ��̕W�������s���邽�߁C�J�e�S���J���ϐ��͗v�_�~�[�ϐ��� #############
########### �_�~�[�ϐ��̍쐬
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
################ �����ϐ��̍s�� ###################
xvar = 13#�����ϐ��̐����w��
############# �p�����[�^����l�́C�ȉ��̕ϐ��̏��Ԃŏo�͂���� ##############
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

#���o�X�g�p�b�P�[�W