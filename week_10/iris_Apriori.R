library(magrittr)
data <- iris
data$Species <- data$Species %>% as.factor()
summary(data)

Class<- function(Ary){
  DATA = c()
  Min = min(Ary)
  Max = max(Ary)
  Inter = (Max - Min)/3
  
  Output <- sapply(Ary,function(x){
    if(Min<= x && x < Min+Inter){
      DATA=c(DATA,"small")
    }else if(Min+Inter<= x && x < Min+Inter*2){
      DATA=c(DATA,"median")
    }else if(Min+Inter*2<= x && x <= Min+Inter*3){
      DATA=c(DATA,"large")
    }
  })
  return(Output)
}  


data$Sepal.Length <- Class(data$Sepal.Length) %>% as.factor()
data$Sepal.Width <- Class(data$Sepal.Width) %>% as.factor()
data$Petal.Length <- Class(data$Petal.Length) %>% as.factor()
data$Petal.Width <- Class(data$Petal.Width) %>% as.factor()


require(arules)
install.packages("arules")
rule <- apriori(data, 
                parameter = list(minlen=4, supp=0.1, conf=0.7),
                appearance = list(default = "lhs",
                                  rhs = c("Species=setosa", "Species=versicolor", "Species=virginica")))

inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

install.packages("arulesViz")
require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")

