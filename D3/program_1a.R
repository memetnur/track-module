#Program 1a
library(corrplot) #install if necessary
cor(iris[,1:4])
corrplot(cor(iris[,1:4]),method="color")
corrplot(cor(iris[,1:4]),method="number")
plot(iris[,1:4]) #scatterplot for all pairs of variables
