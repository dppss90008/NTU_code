# data visualization 

library(ggplot2)
iris_data <- iris
# Scatter Diagram
qplot(data=iris_data, x=iris_data$Sepal.Length, y=iris_data$Sepal.Width, 
      xlab = 'Sepal.Length', ylab = 'Sepal.Width',color = iris_data$Species, 
      geom = c("point", "smooth"))

qplot(iris_data$Sepal.Length, data = iris_data, fill = iris_data$Species)

