#Dataset: morley Graded Set 1 One-way ANOVA, the morley data (one-way ANOVA with one factor)

library(agridat)
mydata1=data("yan.winterwheat")
head(mydata1)
mydata1=yan.winterwheat
str(mydata1)
unique(mydata1$gen)

with(mydata1, tapply(yield, gen, length)) #replications per treatment
with(mydata1, table(env, gen))

library(nlme)
ww.lme=lme(yield~gen, random=~1|env, data=mydata1)
anova(ww.lme, type="marginal")
summary(ww.lme)
#typical = referene 3.999444 

library(car)
qqPlot(resid(ww.lme))
shapiro.test(resid(ww.lme))
plot(ww.lme)
ww.lme.het = lme(yield~gen, random=~1|env, data=yan.winterwheat, weights=varIdent(form=~1|gen))
plot(ww.lme.het)
friedman.test(yield~gen|env, mydata1)

paper = read.csv('paper.csv', sep=',', dec='.', header=TRUE)

#problem 1, 13.03.2021 
#EXAM COVER SHEET - Version AC, Problem 1
# Set the directory to the application path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
mydata1 <- readRDS("output_data.rds")
str(mydata1)
#1)Give the R code to produce suitable descriptive statistics to describe the data set. (1)
mydata1$refrigeration=as.factor(mydata1$refrigeration)
mydata1$vacuum=as.factor(mydata1$vacuum)
str(mydata1)
summary(mydata1)
with(mydata1, tapply(output, refrigeration, summary))
with(mydata1, tapply(output, list(vacuum, refrigeration), summary))




#2)Give the R code to produce suitable graphical representations of the data set. What do you observe? (2)
library(ggplot2)
ggplot(mydata1, aes(y=output, x=vacuum, col=refrigeration))+geom_point()

#observation: we observe a systematically higher output with vacuumisation and a significantelly output observation with and without food storaging in refrigeration 
#possible interatiction may be possible between vacuum and refrigeration effects

#3)Give the R code to fit a suitable parametric model to this data set. Perform an overall F-test, report its p-value and state your conclusion. (2)
mydata1.lm=lm(output~vacuum*refrigeration, data=mydata1)
summary(mydata1.lm) #the overall f value is 2.667e-11 and is significant. an partial F test is considered
library(car)
Anova(mydata1.lm, type=2) #partial F test
#significance in vacuum and refrigeration is observed. but there is no significance interaction in vacuum and ref. 
#reject H0 that all treatments have the same theoretical mean

#4) Assess the model assumptions for your final model: explain what you assess, with which method, give your R code, discuss the results and state your conclusions. (3)
#independent observations; Independence is already specified in the problem assignmentx
summary(mydata1)
#normality
library(car)
library(nlme)
qqPlot(resid(mydata1.lm))
#testing for normality. 
shapiro.test(resid(mydata1.lm))
#conclusion: Reject the null hypothesis of normality if p < ??, but in this case p=0.6576, we assume that with small samples (n=28) a nonsignificant Shapiro Wilk test does not imply no problems with normality are present.
plot(mydata1.lm, which=1)
#bei zunehmender fitted values, nimmt varianz zu. brandweite (unten und oben)
#Testing for homoskedasticity
#Bartlett test of homogeneity of variances
bartlett.test(output~refrigeration, data=mydata1) #p-value = 0.8554
bartlett.test(output~vacuum, data=mydata1) #p-value = 0.9275
#Levene's Test for Homogeneity of Variance (center = median
leveneTest(output~refrigeration, data=mydata1) #0.7358
leveneTest(output~vacuum, data=mydata1) #0.6497
#conclusion; we can not reject the null hypothesis of equal variance. we assume homoskedasticity

#1) completely randomized design with factorial treatment with replications/ design is balanced because same size of 7 for four treatments


mydata1$refrigeration=as.factor(mydata1$refrigeration)
mydata1$vacuum=as.factor(mydata1$vacuum)
summary(mydata1) #balanced design
with(mydata1, tapply(count, spray, length))
with(mydata1, tapply(count, spray, summary))
with(mydata1, tapply(count, spray, sd))


with(mydata1, tapply(vacuum, refrigeration, summary)) 
with(mydata1, table(vacuum, refrigeration))


#2)we can see in the plot that  between refrigeration and vacuum is a positive interaction visible, but needs to be proven.
library(ggplot2)
ggplot(mydata1, aes(spray, count)) + geom_point(shape=1, position=position_jitter(width=0.1, height=0))
plot(count~spray, data=mydata1)

#3)
mydata1.lm = lm(output~vacuum*refrigeration, data=mydata1)
summary(mydata1.lm)#overall f test , last line for p-value of F-statistic
library(car)
Anova(mydata1.lm, type=2) #partial F test

#conclusion: refrigeration and vacuum have an influece due to signifiance of overall F-test p, value
#4)
library(car)
library(nlme)
qqPlot(resid(mydata1.lm))
shapiro.test(resid(mydata1.lm))
plot(mydata1.lm, which=1)


#bei zunehmender fitted values, nimmt varianz zu. brandweite (unten und oben)

#5)
mydata1.lm = lm(output~vacuum+refrigeration, data=mydata1)
summary(mydata1.lm)

library(car)
Anova(mydata1.lm, type=2)

#der overall f-test beweist bei beiden modellen eine signifikanz
#The overall F-test gave a non significant interaction effect between the two factors, we
#can fit the model without the interaction effect and only with the two fixed effects for
#the factors vacuum and refrigeration

#problem 2
#1)
mydata2<-readRDS('yield_data.rds')
str(mydata2) #block, randomized 0.5 p
with(mydata2, tapply(yield,temp, mean)) #0.5 p
with(mydata2,tapply(yield, temp, sd))

#2)
library(ggplot2)

ggplot(mydata2, aes(temp, yield,col=day)) + geom_point()
#we can see with higher temp, yield is increasing. positive linear relationship. 
#variables beschreiben, temp, yield, and day. 

#3)
library(nlme)
mydata2.lme=lme(yield~temp, random=~1|day, data=mydata2)
summary(mydata2.lme) #random block effect model - mixed effect model

#4)
library(emmeans)
mydata2.emm=emmeans(mydata2.lme, 'temp')
mydata2.emm
pairs(mydata2.emm)
plot(mydata2.emm, comparison=TRUE)
#smaller as 0.5. 

#5)
summary(mydata2.lme)
fixef(mydata2.lme)
#interpretation: 5.105756 for intercept, 9.302813   for high temperature
plot(mydata2.lme)
qqPlot(resid(mydata2.lme))

#6)
ranef(mydata2.lme)

#problem 2
#randomized complete block design, than every treatment is applied in every plot exactly one time, allocating treatments randomly within blocks.
mydata2 <- readRDS("yield_data.rds")
mydata2
str(mydata2)
summary(mydata2)
str(mydata2) #block, randomized 0.5 p
with(mydata2, tapply(yield,temp, mean)) #0.5 p
with(mydata2,tapply(yield, temp, sd))

#2. Give the R code to produce suitable graphical representations of the data set. What do you observe? (2)
library(ggplot2)
ggplot(mydata2, aes(temp, yield,col=day)) + geom_point()
#observations: we observe that with high temp yield is increasing,an linear relationship. The day 3 show significiantly the highest yield in all temp and day 1 show systematically the worst yield values. 

#3. What is the main goal of the analysis? Give the R code to fit a suitable parametric model to this data set. (2)
#main goal is temp effect on yield with random effect "day" linear mixed model
library(nlme)
mydata2.lme=lme(yield~temp, random=~1|day, data=mydata2)  
#4
library(emmeans)
mydata2.emm = emmeans(mydata2.lme,'temp')
mydata2.emm
pairs(mydata2.emm)
plot(mydata2.emm, comparison=TRUE)
#smaller as 0.5. 

#5 regression model ???
summary(mydata2.lme)
#intercept: 5.105756 +9.302813  for high temp -0.466, We can now interpret that an increase of the nitrogen level of 0.2 units rises the yield by

ranef(mydata2.lme)


#insectspray dataset, completely randomized design without treatment
data("InsectSprays")
mydata1<-InsectSprays
mydata1
str(mydata1)
summary(mydata1) #balanced design
with(mydata1, tapply(count, spray, length))
with(mydata1, tapply(count, spray, summary))
with(mydata1, tapply(count, spray, sd))

library(ggplot2)
ggplot(mydata1, aes(spray, count))+geom_point(shape=1, position=position_jitter(width=0.1, height=0))
plot(count~spray,mydata1)
library(lattice)
stripplot(count~spray, data=mydata1, jitter=0.2)

InsectSprays$block=factor(rep(rep(1:6, each=2), times=6))
ggplot(InsectSprays, aes(spray, count))+geom_point(shape=1, position=position_jitter(width=0.1, height=0))+facet_grid(~block)
s.o=with(InsectSprays, reorder(spray,count, mean, order=TRUE))
InsectSprays$spray.o<-factor(InsectSprays$spray, levels=levels(s.o))
b.o=with(InsectSprays, reorder(block, count, mean, order=TRUE))
InsectSprays$block.o <- factor(InsectSprays$block, levels=levels(b.o))
ggplot(InsectSprays, aes(spray.o, count))+geom_point()+facet_grid(~block.o)

ins.lm=lm(count~spray, data=InsectSprays)
Anova(ins.lm)
ins.aov=aov(count~spray, data=InsectSprays)
summary(ins.aov)
summary(ins.lm)$sigma#3.921902
summary(ins.lm)$r.squared #0.724439 72% wird dieses Modell erklärt

ins.lm.noint=lm(count~spray-1, data=InsectSprays) #excludes intercept since we do not have a control treatment, shows two-sided t test wether the respective group mean is zero.
summary(ins.lm.noint)
TukeyHSD(ins.aov, 'spray')

library(agricolae)
HSD.test(ins.lm,"spray", group=TRUE, console=TRUE)


library("tidyverse")
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)

library(multcomp)
summary(glht(ins.lm, mcp(spray='Dunnett')))

summary(glht(ins.lm, mcp(spray = 'GrandMean')))

library(car)
qqPlot(resid(ins.lm))
shapiro.test(resid(ins.lm))
plot(fitted(ins.lm), resid(ins.lm), las=1, xlab='Fitted Values', ylab='Residuals')
abline(h=0)
bartlett.test(count~spray,  data=InsectSprays)

library(nlme)
ins.lme=lme(sqrt(count)~spray,random=~1|block, data=InsectSprays)
summary(ins.lme)
fixef(ins.lme)
ranef(ins.lme)
