library(ggplot2)

# Example from slides
A = c(2,1,4,9)
B = c(3,2,5,10)
C = c(4,1,15,80)
D = c("a","a","b","b")
d = data.frame(A,B,C,D)
g = ggplot(d, aes(x=A, y=B, shape=D)) + geom_point()
g

# Paragraph "Scatter plot"
g = ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point() 
g = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) + geom_point()
# The following is needed to display the graph:
g


# Exercise
# The following code does not lead to the desired result, as "gear" is numeric and not of
# type factor. "gear" is interpreted as a continuous variable
ggplot(mtcars, aes(x = mpg, y = wt, colour = gear)) + geom_point()
# Casting the "gear" to the type factor leads to the desired result
ggplot(mtcars, aes(x = mpg, y = wt, colour = factor(gear))) + geom_point()
# With shape you get an error
ggplot(mtcars, aes(x = mpg, y = wt, shape = gear)) + geom_point()


# Paragraph "Histogram"
ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()

# Exercise
ggplot(iris, aes(x = Sepal.Length, color = Species)) + geom_histogram()
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(x = Sepal.Length, color = Species)) + geom_density()
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density()

# here is some code, in case you would like to control the number of bins
numBins = 7;
g = ggplot(iris, aes(x = Sepal.Length, fill = Species))
g = g + geom_histogram(binwidth=(max(iris$Sepal.Length)-min(iris$Sepal.Length))/(numBins-1))
print(g)


# Paragraph "Boxplot"
ggplot(iris, aes(x = Species, y = Sepal.Length)) +  geom_boxplot()

# Exercise
ggplot(iris, aes(x = Species, y = Sepal.Length)) +  geom_boxplot() + coord_flip()

# Exercise
ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot() + geom_point(position = "jitter") 
ggplot(iris, aes(Species, Sepal.Length)) + geom_point(position = "jitter") + geom_boxplot() 
ggplot(iris, aes(Species, Sepal.Length)) + geom_point(position = "jitter") + geom_boxplot(alpha = 0.8)


# Section "Aesthetics and more than one layer"

# Exercise
# All the commands lead to the same plot.
# Explanation: the various ways to specify the aesthetics refer to one Geom layer
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(colour = Species))
ggplot(iris, aes(x = Sepal.Length)) + geom_point(aes(y = Sepal.Width, colour = Species))

# Exercise
# In the following examples we have two Geom layers:
# Here the aesthetics are defined in the call to ggplot, and, thus, apply to both Geoms:
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point() + geom_smooth()
# Here the aesthetics to color by Species only applies to geom_point, and, thus, geom_smooth draws a global trend line:
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(colour = Species)) + geom_smooth()
# Here geom_smooth is missing aesthetics, because they are only specified in geom_point. This leads to an error.
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_smooth()
# And here it is the other way round, which also leads to an error, as geom_point has not aesthetics
ggplot(iris) + geom_point() + geom_smooth(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))

# Add a regression line, with and without confidence intervals
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point() + geom_smooth(method = lm, se = FALSE)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point() + geom_smooth(method = lm)


# Section "Faceting"
base = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
g1 = base + facet_grid(Species ~ .)
g2 = base + facet_grid(. ~ Species)

# Multiple plots
library(gridExtra)
# The following call assumes that you have assigned plots to the variables p1, ..., p4
grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Main title")

# Saving a plot
ggsave("tufte.pdf", g)


# Seting dimensions of plot
g = g + theme(aspect.ratio = 1/1.5)

