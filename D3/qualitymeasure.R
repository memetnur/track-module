#qualitiy measure
#run Program f first if necessary
pcmatrix=cbind(pc1,pc2) #data in one matrix

k=4;
clratio=0 #initiate ratio
for (i in 1:15) #run kmeans 15 times
{
  cl=kmeans(pcmatrix,k)
  if (cl$betweenss/cl$totss > clratio){ #check whether result is better
    clratio=cl$betweenss/cl$totss;
    clk=cl; #write current best result into clk
  } #end if
}# end i-loop
plot(pcmatrix,col=clk$cluster)
