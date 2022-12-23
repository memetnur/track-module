

### Memeti Nurdzane
### ZHAW
### memetnur@students.zhaw.ch

###########################
# Module D2               #
# Graded exercises set 2  #
###########################

### load needed packages
library(tidyverse) 
library(nlme)
library(ggplot2)
library(emmeans)

### import data
my_data = read.csv(file = 'metal.csv', sep = ',', header = TRUE)

### Problem 1 ##########################################################
# Answer:
# This design is called randomized complete block design, as every treatment is applied in every block 
# exactly one time, allocated randomly within blocks. In the experiment each cord is broken into three pieces
# for which the treatment (no dye, dye A, dye B) are allocated randomly, so each treatment is applied for each 
# block (cord) exactly one time. We can also assume that there is no interaction between block and treatment effects

### Problem 2 ##########################################################
# Answer:
# the variables are saved as column name (No.dye, Dye.A, Dye.B),
# the measured value for the strength are spread all over the table,
# therefore I use gather in the package tidyverse to tidy the data to the suitable form.

my_data
str(my_data) 
names(my_data)[2]="Control"
names(my_data)[3]="Treatment A"
names(my_data)[4]="Treatment B"
my_data
my_data=my_data[-nrow(my_data),]# delete the last row and last column in the table 
my_data=my_data[,-ncol(my_data)]
my_data
my_data=gather(my_data, key="Treatment", value="Value", Control:"Treatment B", factor_key = TRUE)
my_data

# change the factor levels of Cord and Treatment
my_data$Cord <- factor(my_data$Cord, levels=c("1","2","3","4","5","6","7","8","9","10")) 
my_data$Treatment=factor(my_data$Dye, levels=c("Control", "Treatment A", "Treatment B") )
my_data=my_data[order(my_data$Cord),] # order the data frame based with Cord
rownames(my_data)=seq(1,nrow(my_data))
my_data
str(my_data) 
# 30 obs. of  3 variables: Cord (factor with 10 levels), Treatment(factor with 3 levels), strength(num) resp. value

### Problem 3  ##########################################################

# Answer:
# the Random block effect models is suitable for analysis this research. 
# The model include both fixed and random effects, therefore it is also called mixed effects model

my_data.lme <- lme(Value ~ Treatment, random = ~ 1 | Cord, data = my_data)
summary(my_data.lme) #30 observations

# plot the data
ggplot(my_data, aes(x=Treatment, y=Value))+
  geom_point()+ 
  facet_grid(~Cord)+ 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1))
shapiro.test(resid(my_data.lme)) # W = 0.98163, p-value = 0.867

# assess for equal variance
ranef(my_data.lme)
shapiro.test(ranef(my_data.lme)$`(Intercept)`) #W = 0.91596, p-value = 0.3245

# the tests indicated that our model satisfy the assumption,
# i.e. the data are normal distributed with equal variance

# the quantify the precision of the estimators, confidence intervals is produced
intervals(my_data.lme)
# The F test is used to test the null hypothesis that all the fixed effect estimates, except the
# intercept, are zero
anova(my_data.lme, type = "marginal") #F-value=3.9166, p=0.0387
##the Dye has a significant effect (p < 0.05)
friedman.test(Value ~ Treatment | Cord, data = my_data) # p-value = 0.045, 
#again here it confirms that the Dye has a significant effect


### Problem 4  ##########################################################
VarCorr(my_data.lme)

# two standard deviations were estimated (σ= 3.957, σblock= 4.333 )
# the result shows that the block effect is not negligible compared to the errors

### Problem 5  ##########################################################
summary(my_data.lme)
# Mean with no dye is 96.67, with dye A 99.29 (96.67+2.62), with dye B 101.62 (96.67+4.95).


# pairwise comparisons with tukey method is produced with method in the library emmeans

ref_grid(my_data.lme) 
my_data.emm <- emmeans(my_data.lme, "Treatment")
pairs(my_data.emm)
# Control - Treatment A p-value= 0.3233
# Control - Treatment B p-value= 0.0305 (p<0.05, significant)
# Treatment A - Treatment B p-value= 0.4044
# this indicates that the treatment B has significant effect on the strength of cord

plot(my_data.emm, comparisons = TRUE) 


