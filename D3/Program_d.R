# Program d
#run Program 1b first to create a 2d data distribution
# the data will be written into the variable 'leaves '
leaves_pca=prcomp(leaves) # calculates rotation ( matrix )
print(leaves_pca)
pc=predict(leaves_pca) # calculates new coordinates
pc1=pc[ ,1] # write coordinates relative to first p.c. into pc1
pc2=pc[ ,2] # write coordinates relative to second p.c.into pc2
plot(pc1,pc2)
