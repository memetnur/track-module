#install.packages (" tidyverse ")
library ( tidyverse )
dfr=data.frame(v1=c('a', 'b','c'), v2=c(1,2,3))
tib=as_tibble(dfr)
dfr2=as.data.frame(tib)
print(dfr)
print(tib)
print(dfr2)

# Data frame with normally distributed random sample
dfr=data.frame(matrix(rnorm(20), ncol=30, nrow=30))
print(dfr)

tib = as_tibble(dfr)
print(tib, n=10, width=50)

print(tib,n=10,width=Inf)
print ( tib [c (10 ,11) ] , n =10)

View ( dfr )
View ( tib )

# Solution Exercise 2
table1
rate = table1$cases / table1$population * 100000
rate
table1$rate = rate
table1
table1 = subset(table1, select=-population)
table1
#Column headers are values, not variable names
table4a

gather(table4a,'1999','2000' , key =" year ", value =" cases ")
table4a
# Section "Multiple variables are stored in one column"
table2
spread(table2, key = type, value = count)

people= tibble(
  name=c("A", "A", "A", "B", "B"),
  key =c("age", "height", "age", "age", "height"),
  value=c(45,186, 50, 37, 156)
  
)
table3
table3Tidy = separate(table3 , rate , into =c(" cases ",
                                                " population ") , sep ="/", convert = TRUE)
table3Tidy

## 3.2 treating missing data

install.packages("mice")
library ("mice")

install.packages("missForest")
library ("missForest")
# Generate an iris table with 10% missing values at random:
iris.mis = prodNA(iris, noNA = 0.1)
# Remove the categorical variable "Species"
iris.mis = subset(iris.mis, select=-c(Species))
summary(iris.mis)
md.pattern(iris.mis)

# Section "Imputation of the mean"

# Exercise
iris.im = iris.mis
iris.im$Sepal.Length[is.na(iris.mis$Sepal.Length)] = 
  mean(iris.mis$Sepal.Length, na.rm=TRUE)
md.pattern(iris.im)

# Advanced Exercise
# iris$Petal.Length and iris[["Petal.Length"]] are equivalent, but using "$" does
# not work inside a function
ImputationOfMean = function(t) {
  for (i in names(t)) {
    t[[i]][is.na(t[[i]])] = mean(t[[i]], na.rm=TRUE)
  }
  return(t)
}
iris.iom = ImputationOfMean(iris.mis)

# Section "Regression imputation"
# Regression: "Sepal.Length" on "Sepal.Width", "Petal.Length", "Petal.Width"
iris.mis.lm = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris.mis)
# Extract the rows from iris.mis where Sepal.Length is NA
tmp = iris.mis[is.na(iris.mis$Sepal.Length),]
# Use the regression model (iris.mis.lm) to predict the Sepal.Length in tmp:
# Note: na.action = na.omit makes sure that rows that contain NA are treated correctly
predSepalLength = predict(iris.mis.lm, newdata = tmp, na.action = na.omit)
predSepalLength


##Chapter 4. Data visualisation

library (" ggplot2 ")
library (" reshape2 ")
library (" ggthemes ")
library (" devtools ")
library (" ggExtra ")
library (" psych ")


