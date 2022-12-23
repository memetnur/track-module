

### Nurdzane Memeti
### ZHAW
### memetnur@students.zhaw.ch

###########################
# Module D2               #
# Graded exercises set 1  #
###########################

### Problem 1 ##########################################################

### load needed packages
library(ggplot2)
library(agricolae)
library(car)
library(NSM3)



### import data and get to know the data
mydata = morley
str(mydata) 
# data.frame:	100 obs. of  3 variables:Expt(int), Run(int), Speed(int)
head(mydata)
with(mydata, tapply(Speed,Expt, length)) 
#20 obs. per Expt
with(mydata, tapply(Speed,Expt, summary))
with(mydata, tapply(Speed,Expt, mean)) 
#mean for 5 Expt: 909.0 856.0 845.0 820.5 831.5 
with(mydata, tapply(Speed,Expt, sd)) 
#sd for 5 Expt:104.92604  61.16414  79.10686  60.04165  54.21934 

### restructure/format data as needed 
mydata$Expt=as.factor(mydata$Expt) 
# change the format of Speed from integer to factor level (5 factor levels)

### fit ANOVA model
myanova= aov(Speed~Expt, data = mydata)

### extract needed information (e.g. p-value)
summary(myanova)

###Interpretation: Given p-value = 0.00311, so therefore the Null hypothesis is rejected
#because at least two theoretical mean among these five experiments are signifcent different.

### Visualization is done with ggplot and geom_boxplot
ggplot(mydata, aes(Expt,Speed)) + geom_boxplot() 


### Problem 2 ##########################################################

# The Tukey  HSD and agricolae function HSD.test methods is used to do the pairwise comparisons 
TukeyHSD(myanova, "Expt") 
# difference between expriment 4-1 and 5-1 are significant with p-value 0.003 resp. 0.012

### Problem 3 ##########################################################

qqPlot(resid(myanova))

shapiro.test(resid(myanova)) 
#Interpretation:It obtains p=0.015, which is less than 0.05, we reject the null hypothesis of normality and 
#therefore it concludes that the residuals do not come from a normal distribution


### Problem 4 ##########################################################

## Test for homogeneity of variances
bartlett.test(Speed ~ Expt, data = mydata) 
#p-value = 0.021 
leveneTest(Speed ~ Expt, data = mydata) 
#p-value=0.162

#Interpretation: one of the two tests (Bartletttest) p vaule is 0.021, less than 0.05.
#It concludes that the variance homogeneity is rejected

### Problem 5 ##########################################################

### non-parametric test with the Kruskal-Wallis test
kruskal.test(Speed ~ Expt, data = mydata) 
# p-value = 0.005, reject the null hypothesis. 


# all two-sided pairwise comparisons (by Dwass, Steel, and Critchlow-Fligner)
with(mydata, pSDCFlig(x = Speed, g = as.numeric(Expt), method=NA))

# with this method, treatments 1-2, and treatments 1-4 with rejection value less than 0.05

