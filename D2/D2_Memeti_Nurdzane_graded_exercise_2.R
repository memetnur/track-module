### Nurdzane Memeti
### ZHAW
### memetnur@students.zhaw.ch


###########################
# Module D2               #
# Graded exercises set 2  #
###########################


### Problem 1 ##########################################################

### load needed packages
library(reshape2)
library(ggplot2)
library(nlme)
library(car)
library(multcomp)

### import data
my_data = read.csv(file = 'metal.csv', sep = ',', header = TRUE)

### Problem 1 ##########################################################

# This design is called randomized complete block design, as every treatment is applied in every block 
# exactly one time, allocated randomly within blocks. In the experiment each cord is broken into three pieces
# for which the treatment (no dye, dye A, dye B) are allocated randomly, so each treatment is applied for each 
# block (cord) exactly one time. We can also assume that there is no interaction between block and treatment effects


### Problem 2 ##########################################################

#get to know and tidy the data
# the data is not ready for analyse, because the variables are saved as column name (No.dye, Dye.A, Dye.B),
# the measured value for the strength are spread all over the table, 
# therefore I use gather in the package "tidyverse" to tidy the data to the suitable form.

head(my_data)
str(my_data) 
# delete the last row and last culumn in the table 
my_data=my_data[-nrow(my_data),]
my_data=my_data[,-ncol(my_data)]

# because the treatment variable are stored as column name, 
#we use the gather method in the library "tidyverse" to formulate the data
library(tidyverse) 
my_data=gather(my_data, No.dye, Dye.A, Dye.B, key = "Dye", value="Strength")

# change the factor levels of Cord and Dye
my_data$Cord <- factor(my_data$Cord, levels=c("1","2","3","4","5","6","7","8","9","10")) 
my_data$Dye=factor(my_data$Dye, levels=c("No.dye", "Dye.A", "Dye.B") )
my_data=my_data[order(my_data$Cord),] # order the data frame based with Cord
rownames(my_data)=seq(1,nrow(my_data))

str(my_data) # 30 obs. of  3 variables: Cord (factor with 10 levels), Dye(factor with 3 levels), strength(num)
head(my_data)


### Problem 3 ##########################################################

# The appropriate model for this experiment is mixed effects model. This model includes both fixed and random effects.
# We are interested in seeing the effect of treatment and not only the effect of block.

# Model
metal.lme <- lme(value ~ variable, random = ~ 1 | Cord, data = metal_long)
summary(metal.lme)

# Diagnostics
qqPlot(resid(metal.lme))
shapiro.test(resid(metal.lme))
# Based on qqplot (only one point outside boundaries) and Shapiro-Wilk test (p-value = 0.867) one can conclude that
# the residuals are normally distributed. Because p > 0.05 we cannot reject null hypothesis of normality and conclude 
# that residuals come from a normal distribution.

plot(metal.lme)
# The variance of the error terms does not seem to change with the fitted values -> no problems with the equal 
# variance assumption.

### Problem 4 ##########################################################
summary(metal.lme)
# two standard deriation were estimated (for σ and for σblock)
# the values are σ= 3.957, σblock= 4.333 
# the result shows that the block effect is not negligible compared to the errors

### Problem 5 ##########################################################
my_data_fixef=fixef(my_data.lme) # get fix effect from model 
my_data_fixef["(Intercept)"] # the value for no.dye is 96.67 
my_data_fixef["(Intercept)"]+my_data_fixef["DyeDye.A"]  # 99.29
my_data_fixef["(Intercept)"]+my_data_fixef["DyeDye.B"] #  101.62 
# the average strength percentages per treatment (No.dye, Dye.A, Dye.B) are 96.67, 99.29, 101.62

# pairwise comparisons with tukey method is produced with method in the library emmeans
library(emmeans)
ref_grid(my_data.lme) # Dye = No.dye, Dye.A, Dye.B
my_data.emm <- emmeans(my_data.lme, "Dye")
pairs(my_data.emm)
# the p value between No.dye and Dye.B is 0.3233
# the p value between No.dye and Dye.B is 0.0305 (p<0.05, significant)
# the p value between Dye.A - Dye.B is 0.4044
# this indicates that the treatment DyeB has significant effect on the strength of cord

plot(my_data.emm, comparisons = TRUE) 

