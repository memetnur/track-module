#Program l
# install.packages("e1071") #install if necessary
library(e1071)
numberofclusters=3;
cl_result = cmeans(iris[,-5], numberofclusters, iter.max=100, m=1.1, method="cmeans")
# iter.max is the max. number of iterations of the algorithm
#m=2 defines the degree of fuzziness

plot(iris[,1], iris[,2], col=cl_result$cluster)
points(cl_result$centers[,c(1,2)], col=1:3, pch=8, cex=2)
cl_result$membership[,] 

library(rgl)
plot3d(iris[,1], iris[,2],iris[,3], col=cl_result$cluster)