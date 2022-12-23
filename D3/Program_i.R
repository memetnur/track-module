
#Program i

  #using the results from Program f
  #run f first if necessary
  pcmatrix=cbind(pc1,pc2) #data in one matrix
  plot(pcmatrix) #you can see 4 clusters

 # clustering
  k=4 #number of clusters
  cl=kmeans(pcmatrix,k)
  plot(pcmatrix,col=cl$cluster)
  points(cl$centers,pch=25,col="blue")
  cl

 # confusion matrix
  feverlabels= read.csv(file="feverlabels.csv")
  actual_classes=as.matrix(feverlabels) #actual labels as matrix
  predicted_classes = cl$cluster   #predicted cluster labels
  confusion_matrix = table(actual_classes, predicted_classes)
  confusion_matrix
  