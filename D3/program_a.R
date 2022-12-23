#Program a
#make sure that you are in the right directory
 
#loading iris data s
  data(iris)
  
  #loading fever data set
  fever=read.csv(file="fever.csv",header=TRUE)
  feverlabels= read.csv(file="feverlabels.csv")

 #loading colon cancer data set
  cancer=t(read.table("cancer.txt",header=FALSE))
  cancerid=as.matrix(read.delim("canceridentity.txt",header=FALSE))
  
  
  
 #loading isis chemical data set
  isisall=read.csv(file="isisall.csv",header=TRUE)
  isis=isisall[,2:167] #only numerics
  # use the data set 'isis ' for the analysis
  
 #loading mammal teeth data set
  dental=read.table("dental.csv",header=TRUE,sep=",")
  
  #some libraries used later
  #install first if needed
  library(rgl)
  library(graphics)
  library(ggplot2)
  library(reshape2)
  library(corrplot)
  library(MASS)
  library(vegan)
  library(clValid)
  library (e1071)
  