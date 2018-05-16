library(magrittr)
library(ggplot2)

# Read the titanicTrain data and store it in train

train <- read.csv("titanicTrain.csv")
train <- train[c(1:1000),]

# Train data 上面的NA
str(train)
sapply(train, function(x) {sum(is.na(x))})

## Pclass ##
train$pclass <- train$pclass %>% as.factor
Pclass <- cbind(summary(train$pclass[train$survived==0]),summary(train$pclass[train$survived==1]))
Pclass <- Pclass %>% t
rownames(Pclass) <- c("0","1")
barplot(Pclass,col=c("gray","black"),main=" Pclass variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)

## Sex ##
train$sex <- train$sex %>% as.factor
sex <- cbind(summary(train$sex[train$survived==0]),summary(train$sex[train$survived==1]))
sex <- sex[-1,] %>% t
rownames(sex) <- c("0","1")
barplot(sex,col=c("gray","black"),main=" sex variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


## embarked ##
train$embarked <- train$embarked %>% as.factor
summary(train$embarked[train$survived==0])
summary(train$embarked[train$survived==1])
embark <- cbind(summary(train$embarked[train$survived==0]),summary(train$embarked[train$survived==1]))
embark <- embark[-1,] %>% t
rownames(embark) <- c("0","1")

barplot(embark,col=c("gray","black"),main=" embarked variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)

## family = sibsp + parch + 1  ##

family <- train$parch + train$sibsp + 1
train <- cbind(train,family)

ps0 <- train$family[train$survived==0] %>% as.factor %>% summary
ps1 <- train$family[train$survived==1] %>% as.factor %>% summary %>% c(.,0)
family <- rbind(ps0,ps1)
rm(ps0)
rm(ps1)

barplot(family,col=c("gray","black"),main="family",beside=TRUE,ylab="counts",xlab="Number of people")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


## boat ##
newboat <- sapply(train$boat,function(x){
  if(x=="") x<- "Not On"
  else x<- "On"
})

train <- cbind(train,newboat)
boatdead <- train$newboat[train$survived==0] %>% as.factor %>% summary
boatSurvive <- train$newboat[train$survived==1] %>% as.factor %>% summary
boat <- rbind(boatdead,boatSurvive)
rm(boatdead)
rm(boatSurvive)
boat
rownames(boat) <- c("0","1")
colnames(boat) <- c("on boat","not on boat")
barplot(boat,col=c("gray","black"),main="boat",beside=TRUE,ylab="counts",xlab="Number of people")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


## body ##

