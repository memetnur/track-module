#problem 1, 04.03.2021 
#EXAM COVER SHEET - Version B, Problem 1
# Set the directory to the application path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
#info: 
#1) completely randomized design with factorial treatment with replications/ design is balanced because same size of 7 for four treatments

mydata1 = read.csv(file = 'output_data.csv', header = TRUE )
str(mydata1)
mydata1$refrigeration=as.factor(mydata1$refrigeration)
mydata1$vacuum=as.factor(mydata1$vacuum)
summary(mydata1) #balanced design
with(mydata1, tapply(vacuum, refrigeration, summary))
with(mydata1, tapply(vacuum, refrigeration, length)) 

with(mydata1, table(vacuum, refrigeration))


#2)we can see in the plot that  between refrigeration and vacuum is a positive interaction visible, but needs to be proven.
library(ggplot2)
ggplot(mydata1, aes(refrigeration, output,col=vacuum)) + geom_point(shape=1, position=position_jitter(width=0.1, height=0))


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

df = data.frame(final_g, cond="C2", data=mydata1)
df$pred = predict(Oats.lme, df, level=0)
df
