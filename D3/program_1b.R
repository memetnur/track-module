#Program 1b
 library(MASS)
 mu=c(1,4) #vector with means
 varx=1; vary=1;covarxy=0.8 
 covar=array(c(varx,covarxy,covarxy,vary),dim=c(2,2)) #defines covariance matrix
leaves=mvrnorm(150,mu,covar) #2D Gaussian
 x=leaves[,1]
 y=leaves[,2]


hist(x)
hist(y)
plot(x,y)