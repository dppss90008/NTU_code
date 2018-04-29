# Read the titanicTrain data and store it in titanic
titanic <- read.csv("titanicTrain.csv")
# Train data 上面的NA
str(titanic)

boat <- titanic[titanic$boat!='',]
notboat <- titanic[titanic$boat=='',]
Summary(boat$survived)

boat$survived
summary(as.factor(boat$survived))
summary(as.factor(notboat$survived))

