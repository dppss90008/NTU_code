library(magrittr)
library(ggplot2)
# Read the titanicTrain data and store it in titanic
titanic <- read.csv("titanicTrain.csv")
titanic <- titanic[c(1:1000),] 
# Train data 上面的NA
str(titanic)
sapply(titanic, function(x) {sum(is.na(x))})

# 篩選登船與沒有登船存活率
boat <- titanic[titanic$boat!='',]
notboat <- titanic[titanic$boat=='',]
boat <- summary(as.factor(boat$survived))
notboat <- summary(as.factor(notboat$survived))
names(boat) <- c("No","Yes")
boat %>% barplot

# 篩選男女存活率
male <- titanic[titanic$sex=='male',]
female <- titanic[titanic$sex=='female',]
summary(as.factor(male$survived))
summary(as.factor(female$survived))

# Pclass 
p1 <- titanic[titanic$pclass==1,] 
summary(as.factor(p1$survived))
p2 <- titanic[titanic$pclass==2,]
summary(as.factor(p2$survived))
p3 <- titanic[titanic$pclass==3,]
summary(as.factor(p3$survived))

# embarked
C <- titanic[titanic$embarked=="C",] 
summary(as.factor(C$survived))
Q <- titanic[titanic$embarked=="Q",] 
summary(as.factor(Q$survived))
S <- titanic[titanic$embarked=="S",] 
summary(as.factor(S$survived))
empty <- titanic[titanic$embarked=="",] 
summary(as.factor(empty$survived))

