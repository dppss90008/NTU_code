library(magrittr)
library(ggplot2)
library(randomForest)
library(e1071)

# Read the titanicTrain data and store it in titanic

train <- read.csv("titanicTrain.csv")
train <- train[c(1:1000),]
test <- read.csv("titanicQuestion.csv")
all <- rbind(train,test)

# Train data 上面的NA
str(all)
sapply(all, function(x) {sum(is.na(x))})

# 篩選登船與沒有登船存活率
boat <- train[train$boat!='',] %>% as.factor()
notboat <- titanic[titanic$boat=='',]
boat <- summary(as.factor(boat$survived))
notboat <- summary(as.factor(notboat$survived))
names(boat) <- c("No","Yes")
boat %>% barplot

# 篩選男女存活率
all$sex <- as.factor(all$sex)
summary(all$sex[all])

all[!is.na(all$Survived)
male <- titanic[titanic$sex=='male',]
female <- titanic[titanic$sex=='female',]
summary(as.factor(male$survived))
summary(as.factor(female$survived))
sex <- rbind(summary(as.factor(male$survived)),summary(as.factor(female$survived)))
sex <- t(sex)
colnames(sex) <- c("male","female")
sex
barplot(sex,col=c("coral","aquamarine","coral","aquamarine"),main="Sex variable",beside=TRUE,ylab="counts")
  legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("coral","aquamarine"), horiz=TRUE, cex=0.8)

# Pclass 
p1 <- titanic[titanic$pclass==1,] 
p1 <- summary(as.factor(p1$survived))
p2 <- titanic[titanic$pclass==2,]
p2 <- summary(as.factor(p2$survived))
p3 <- titanic[titanic$pclass==3,]
p3 <- summary(as.factor(p3$survived))
pclass <- rbind(p1,p2,p3)
pclass <- t(pclass)

barplot(pclass,col=c("gray","black"),main="Pclass variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


# embarked
C <- titanic[titanic$embarked=="C",] 
C <- summary(as.factor(C$survived))
Q <- titanic[titanic$embarked=="Q",] 
Q <- summary(as.factor(Q$survived))
S <- titanic[titanic$embarked=="S",] 
S<- summary(as.factor(S$survived))
embark <- rbind(C,Q,S)
embark <- t(embark) 

barplot(embark,col=c("gray","black"),main="embarked variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)

# parch
parch0 <- titanic[titanic$parch==0,]$survived %>% as.factor %>% summary
parch1 <- titanic[titanic$parch==1,]$survived %>% as.factor %>% summary
parch2 <- titanic[titanic$parch==2,]$survived %>% as.factor %>% summary
parch3 <- titanic[titanic$parch==3,]$survived %>% as.factor %>% summary
parch4 <- titanic[titanic$parch==4,]$survived %>% as.factor %>% summary
parch5 <- titanic[titanic$parch==5,]$survived %>% as.factor %>% summary
parch6 <- titanic[titanic$parch==6,]$survived %>% as.factor %>% summary
parch <- rbind(parch0,parch1,parch2,parch3,parch4,parch5,parch6) %>% t
parch
barplot(parch,col=c("gray","black"),main="parch variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


# sibsp + parch



parch_sib <- titanic$parch + titanic$sibsp 
titanic <- cbind(titanic,parch_sib)

ps0 <- titanic$parch_sib[titanic$survived==0] %>% as.factor %>% summary
ps1 <- titanic$parch_sib[titanic$survived==1] %>% as.factor %>% summary %>% c(.,0)
ps <- rbind(ps0,ps1)

barplot(ps,col=c("gray","black"),main="parch+sibsp variable",beside=TRUE,ylab="counts",xlab="parch+sibsp 數量")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)

set.seed(2017)
