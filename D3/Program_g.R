#Program g
#run Program f first

sd=fever_pca$sdev
var=sd^2
var.percent = var/sum(var)*100

barplot(var.percent,xlab="PC",ylab="Percent Variance", names.arg=1:length(var.percent),las=1,ylim=c(0,max(var.percent)),col="gray")
abline(h=1/ncol(fever)*100,col="red")
