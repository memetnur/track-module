#Program k  
library(MASS)
  #generating a toy data set with two Gaussian clusters 
  
   mu1=c(0,0); #center cluster 1
   mu2=c(4,4); #center cluster 2
   sd=1; #standard deviation of cluster distributions
   cl1x=rnorm(20,mu1[1],sd);cl1y=rnorm(20,mu1[2],sd);cl1=cbind(cl1x,cl1y)
   #cluster 1: cl1x are x-coordinates, cl2y are y-coordinates
   #randomly drawn from a normal distribution with 20 points
   cl2x=rnorm(20,mu2[1],sd);cl2y=rnorm(20,mu2[2],sd);cl2=cbind(cl2x,cl2y)
   cl=rbind(cl1,cl2)

   plot(cl)

 # hierarchical clustering with the function hclust 
  d=dist(cl) #creates a object with the interpoint distances
  hc=hclust(d,method="single") #the result of hclust is written into hc
  plot(hc,hang=-1)

 democut=cutree(hc,k=5)  #try and find out
 democut

 democut2=cutree(hc,h=0.65)
 democut2

  #new plot
  plot(hc, hang=-1)
  # draw dendogram with red borders around the 5 clusters 
  rect.hclust(hc, k=5, border="red") 
  
  
  
