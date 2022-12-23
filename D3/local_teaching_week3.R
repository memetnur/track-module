#Bismillah
#D3-local session 3- part1

#exercise 1:



"""
1) Choose five classes, i.e. different types of products (sorts of fruits or vegetables). Select the rows containing the data for your classes measured
without a bag. Save the resulting table in a new variable.
"""
spectra=read.csv("spectra.csv", sep=";", header=T, stringsAsFactors = T)
fruits=c("Apple", "Apricot", "Banana", "Carrot", "Peach")
spectra1=spectra[spectra$bag=="none" & spectra$class %in% fruits,]

head(spectra)
spec_dist_man=dist(spectra1[,-c(1,2)], method="manhattan")
mds=cmdscale(spec_dist_man, k=2)
plot(mds, col=spectra1$class)
legend('topleft', legend=unique(spectra1$class), text.col=as.numeric(unique(spectra1$class)))


"""
2) Perform a k???means clustering on the original 500-dimensional data and
assess the results using a confusion matrix.
"""

#kmeans
k=5
cl=kmeans(spectra1[,-c(1,2)],k)
actual_class=as.matrix(spectra1$class)
predicted_cluster=cl$cluster
confusion_matrix=table(actual_class,predicted_cluster)
confusion_matrix
"""
in confusion_matrix you see that apple is in class 1 predicted but there are 3 apples in cluster 4 predicted, which is wrong. 
furthermore, you can see in the confusion matrix that banana carrot and peach are not good identified in cluster. 
"""

"""
3. Perform a hierarchical clustering (on the original data) and illustrate the
clustering in a dendrogram. Assess the quality of the results.
"""
# hierarchical clustering
d= dist(spectra1[,-c(1,2)])
hc= hclust(d,method ="ward.D")
plot (hc , labels=spectra1$class,hang = -1)
actual_class=as.matrix(spectra1$class)
predicted_cluster=cutree(hc,k=5) #cut at 5 clusters
#you may again use a confusion matrix to assess the result
confusion_matrix=table(actual_class,predicted_cluster)
confusion_matrix

"""
Exercise 2 Read Chapter 4.4 in the lecture notes.
a) Interpret the results from Program l. What happens when you make m very
large or almost 1?
"""
library(e1071)
library(rgl)
cl_result = cmeans(iris[,1:2], 3, 100, m=2, method="cmeans")
plot(iris[,1], iris[,2], col=cl_result$cluster)
points(cl_result$centers[,c(1,2)], col=1:3, pch=8, cex=2)
points(iris[135,1:2], cex=2)
cl_result$membership[135,]
cl_result$membership[136,]
iris[135,5]
x=iris[,1]; y=iris[,2]; z=iris[,3];
plot3d(x,y,z,col=as.numeric(iris$Species))
points3d(x[135],y[135],z[135],col="blue",size=10)

"""note:
Note that in the 2D projection the membership is wrong. This is an example where relevant information is
lost in a projection.
"""

#Exercise 3

# run Program a first
k=7
cl_isis=cmeans(isis,k, iter.max=100, m=2, method="cmeans")
cl_isis
#comparison
isislist=isisall$class #only labels
actual_classes=as.matrix(as.numeric(isislist)) #actual labels as matrix
predicted_classes = cl_isis$cluster #predicted cluster labels
confusion_matrix = table(actual_classes, predicted_classes)
confusion_matrix

