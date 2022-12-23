#### Task 2

setwd("C:/Users/Nurdzane Memeti/OneDrive - ZHAW/ZHAW/Master/D Module/D3")
selection_parameters = read.csv(file = "Wineparameters.csv", sep = ';', header = TRUE)
selection_spectra= read.csv(file = "Winespect.csv", sep = ';', header = TRUE)
#Subset data frame rows in R:

library(dplyr)
library(tidyverse)
library("fossil")
new_data= filter(selection_parameters, name=="Blauburgunder" | name=="Merlot 2009" | name=="Garanoir_Gamaret"| name== "Pino noir" |name== "Zweigelt"|name== "Chardonay"|name== "Rheinriesling"|name== "Sauvignon Blanc")
#merge datasets together

new_wine=t(selection_spectra)
new_winespect=as.data.frame(new_wine)
n_spec <- tibble::rownames_to_column(new_winespect,"WineID")
my_data=merge(new_data, n_spec, by="WineID")

#### Task 3
#enter your code and run it

n=length(my_data)
ts.plot(t(my_data[10:n]), 
        col=my_data$color, 
        gpars=list(main=paste("IR-Spectrum Analysis of Wine Varieties"),
                   xlab=paste("Wavelength [cm^-1]"),
                   ylab=paste("Absorbance"), lty=3)
)

# Add legend to top right, outside plot region
legend("topright", legend=c("Red Wines", "White Wines"), pch=20, 
       col=my_data$color,
       horiz=TRUE, bty='n', cex=0.8)

#### Task 4
library(vegan)
n=length(my_data)
distance=dist(my_data[10:n],method="euclidean")
MDS = cmdscale (distance , k=2)
MDS1=MDS[,1]
MDS2=MDS[,2]

plot(MDS1,MDS2,
     pch=20, 
     cex=1,
     col=my_data$color,#for the 2 classes red and white 
     main = paste("MDS on Infrarot-spectrum")
)
text(MDS2~MDS1,labels=my_data$name,data=my_data, cex=0.5, font=2, pos=3)


#for comparison I choose to use PCA too
data2=my_data[10:n]
my_data_pca=prcomp(data2)
pc=predict(my_data_pca)
pc1=pc[,1]
pc2=pc[,2]

plot(pc1,pc2,
     pch=20, 
     cex=1,
     col=my_data$color,#for the 2 classes red and white 
     main = paste("PCA on IR-Spectrum Analysis of Wine Varieties")
)
text(pc2~pc1,labels=my_data$name,data=my_data, cex=0.5, font=2, pos=3)
#### Task 5
data2=my_data[10:n]
my_data_pca=prcomp(data2)
pc=predict(my_data_pca)
pc1=pc[,1]
pc2=pc[,2]

sd=my_data_pca$sdev
variance = sd^2 #variance
var.percent = variance/sum(variance) * 100 #percentage of variance 
#plot:
barplot(var.percent , xlab="WineID", ylab="Percent Variance",names.arg=1:length(var.percent),
        las=1, ylim=c(0,max(var.percent)), col="yellow", main=paste("Percent Variance of wine bottles")
)
abline(h=1/ncol(my_data[10:17])*100, col="blue") #12 percentage
length(which(var.percent>(100*1/nrow(my_data[10:17]))))


#### Task 6
#pc at least 97%
length(which(var.percent>(97*1/nrow(my_data[10:n]))))
#perform a wards clustering
#label the dendogram using the wine varieties.
#confusion matrix, actual classes
plot(MDS)
d = dist(MDS) # creates a object with the interpoint distances
hc= hclust(d , method ="ward.D") #the result of hclust is written into hc
plot(hc , hang = -1,labels= my_data$name, main="Cluster Dendogram of 29 Wine Bottles", ylab = "Distances between clusters")
democut = cutree ( hc , k =2) #try and find out
democut
democut2 = cutree ( hc , h =0.65)
democut2
# new plot :
plot(hc , hang = -1, my_data$name, main="Cluster Dendogram of 29 Wine Bottles", ylab = "Distances between clusters")
# draw dendogram with blue borders around the 5 clusters
rect.hclust( hc , k =5 , border ="blue")

# confusion matrix
k=8
actual_classes=as.matrix(my_data$name) #actual labels as matrix
predicted_classes = cutree(hc,k) #predicted cluster labels
confusion_matrix = table(actual_classes, predicted_classes)
confusion_matrix


#Task 7
#confusion matrix with k=2, because 2 classes white and red
k=2
actual_classes=as.matrix(my_data$color) #color white and red
predicted_classes = cutree(hc,k) #predicted clustering
confusion_matrix = table(actual_classes, predicted_classes)
confusion_matrix

#randindex= Measures to compare the similarity of two clustering outcomes

rand.index(as.numeric(as.factor(actual_classes)),predicted_classes)
adj.rand.index(as.numeric(as.factor(actual_classes)),predicted_classes)

"""
diskussion: methode zur schätzung der clusteranzahl, den das ziel einer methode ist es, cluster zu finden, die homogen und isoliert sind. 
ob diese sinnvoll ist?
"""

#Task 8:
#calculating the rand.index of task 6
#calcualting the rand.index of single linkage "sl" first:
hc_sl=hclust(d,method="single")
rand.index(hc$height, hc_sl$height) #result: 0.9150231
#compare ward's method and single linkage by calculating the rand.index


#Task 9:
data2=my_data[10:n]
my_data_pca=prcomp(data2)
pc=predict(my_data_pca)
pc1=pc[,1]
pc2=pc[,2]

#my_data$Alc.Vol.=factor(my_data$Alc.Vol.)
ggplot(my_data, aes(x=pc1,y=my_data$Alc.Vol.)) + 
        geom_point() + 
        geom_smooth(method = "lm", col = "black") + 
        geom_smooth(method = "loess", span = 0.3, linetype = "dashed", se = FALSE) +
        geom_smooth(method = "loess")


lin = lm(Alc.Vol. ~ pc1, data = my_data)
summary(lin)$adj.r.squared


