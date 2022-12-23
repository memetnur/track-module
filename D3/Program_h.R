#Program h
library(vegan)


#we first construct the distance matrix. In many cases, this is already given
cities=c("Zurich","Geneva","Basel","Bern","Lugano","Lausanne","St.Gallen","St. Moritz","Schaffhausen")
N = length(cities)
dstMat = matrix(numeric(N^2), nrow=N)

##            GE   BS  BE   LUG   LA    SG  SM   SH
cityDst <- c(225,  87, 96,  156,  174, 64,  140, 37, ## ZH
                  187, 130, 218,  51,  282, 286, 251,  ## GE
                       69,  202,  137, 136, 208, 80,  ## BS
                            156,  78,  155, 190, 123, ## BE
                                  188, 162, 87,  190,  ## LUG
                                       232, 246, 201,  ## LA
                                            110, 63, ## SG
                                                 162) ## SM
              dstMat[upper.tri(dstMat)] = rev(cityDst)
dstMat = t(dstMat[ , N:1])[ , N:1]
dstMat[lower.tri(dstMat)] = t(dstMat)[lower.tri(dstMat)]
dimnames(dstMat) = list(city=cities, city=cities)


#here we perform MDS
mds = cmdscale(dstMat, k=2)


#plotting:
xLims = range(mds[ , 1]) + c(0, 100)
yLims = range(mds[ , 2]) + c(-50, 50)
plot(mds[,1],-mds[,2], xlim=xLims, ylim=yLims,ylab="South-North", xlab="West-East", pch=16,
     main="City locations according to MDS")
text(mds[ , 1]+5, -mds[ , 2], adj=0, labels=cities)
#minus might be introduced to adjust the directions


