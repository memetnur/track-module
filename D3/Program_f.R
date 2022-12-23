# Program f
# fever curves : run Program a first
# plotting curves as time series
library (graphics) # install first if necessary
feverdata = data.frame (t(fever))
ts.plot(feverdata) # without colors
#PCA and plotting
fever_pca=prcomp(fever)
pc1=predict(fever_pca)[,1]
pc2= predict(fever_pca)[,2]
plot(pc1,pc2)