#Program e
library(MASS)
library(rgl)

#Define 3D covariance matrix:
varx=1.5;vary=1;varz=0.05
covar=array(c(varx,0,0,0,vary,0,0,0,varz),dim=c(3,3))
data1=mvrnorm(300,c(0,0,0),covar)
plot3d(data1,xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3),xlab="x",ylab="y",zlab="z")

data_pca=prcomp(data1)
pc1=data_pca$rotation[,1]
pc2=data_pca$rotation[,2]
pc3=data_pca$rotation[,3]


segments3d(c(0,3*pc1[1]),c(0,3*pc1[2]),c(0,3*pc1[3]))
segments3d(c(0,3*pc2[1]),c(0,3*pc2[2]),c(0,3*pc2[3]))
segments3d(c(0,3*pc3[1]),c(0,3*pc3[2]),c(0,3*pc3[3]))