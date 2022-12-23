"""
bismillah
date: 25.20.20
author: nurdzane memeti
e.mail: memetnur@students.zhaw.ch
topic: 4. Data visualisation
"""
#install.packages("ggplot2")

library ("ggplot2")
library ("reshape2")
library ("ggthemes")
library ("devtools")
library ("ggExtra")
library ("psych")


ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, colour = Species))+geom_point()

"""
Exercise 4.1.1 In this exercise you will use the built-in data set mtcars. You
can learn about the dataset with R's help function: help(mtcars). Make a scatterplot as above. Assign the variable mpg to the x-axis, wt to the y-axis, and
gear to the colour. This does not lead to the desired result. Why? (Compare the
data type of Species and gear). How can you solve the problem?
"""
ggplot(mtcars,aes(x=mpg, y=wt, colour = gear))+geom_point()

ggplot(mtcars, aes(x = mpg, y = wt, colour = factor(gear))) + geom_point()

ggplot(mtcars, aes(x = mpg, y = wt, shape = gear)) + geom_point()

ggplot(iris,aes(x=Sepal.Length))+geom_histogram()

"""
Exercise 4.1.2 Make a density graph by simply adding a geom density() layer,
instead of geom histogram()
"""

# Exercise 4.1.3 and 4.1.4
ggplot(iris, aes(x = Sepal.Length, color = Species)) + geom_histogram()
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(x = Sepal.Length, color = Species)) + geom_density()
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density()

#boxplot:
ggplot(iris,aes(x=Species, y=Sepal.Length))+geom_boxplot()

ggplot(iris,aes(x=Species, y=Sepal.Length))+coord_flip()

ggplot(iris,aes(Species, Sepal.Length))+geom_point(position="jitter")+geom_boxplot(alpha=0.5)

#4.1.3 Aesthetics and more than one layer
g=ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, colour=Species))+geom_point()

ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, colour=Species))+geom_point()+geom_smooth(method=lm)

#4.1.4 Faceting, multiple plots, saving plots
base=ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+geom_point()
g1=base+facet_grid(Species ~.)
g2=base+facet_grid(.~Species)

library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol=2, top="main title")

ggsave("myplot.pdf", g)
