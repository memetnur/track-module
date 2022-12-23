#code source: memeti Nurdzane
#Date: 21.11.2020

#load the iris dataset
library(datasets)
install.packages("predict3d")
library(predict3d)
data(iris)
summary(iris)

head(iris)
plot(iris)
plot(iris$Sepal.Width,iris$Sepal.Length)
fit1=lm(Sepal.Length~Sepal.Width,data=iris)
fit1

ggPredict(fit1)
iris=lm(iris$Petal.Length, iris$Petal.Width, data=iris)
