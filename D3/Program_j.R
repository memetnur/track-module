# Program j: calculating Dunn index for different k
# extract first two classes in fever data set:
fever2=fever[1:200,]
fever2_pca=prcomp(fever2)
pcc=predict(fever2_pca) # calculates new coordinates
pc1=pcc[,1] # write coordinates relative to first p.c.
pc2=pcc[,2] # write coordinates relative to second p.c.
plot(pc1,pc2)
library(clValid)
pcmatrix =cbind(pc1,pc2) # data in one matrix
d= dist (pcmatrix) # calculating all the distances
dindex =0;
for (k in 2:30) {
  cl= kmeans(pcmatrix,k)
  dindex =c(dindex,dunn(d,cl$cluster))}
plot(dindex)
lines(dindex)