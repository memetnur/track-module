library(agridat)

str(federer.tobacco) #56 obs. of  4 variables: row, block, dose, height
federer.tobacco$row=factor(federer.tobacco$row)
federer.tobacco$block=factor(federer.tobacco$block)
federer.tobacco$dose=factor(federer.tobacco$dose)

ggplot(federer.tobacco, aes(dose, height))+geom_point()+facet_grid(~row)
str(federer.tobacco) #56 obs. of  4 variables: row(7 levels), block (8 levels), dose(7 levels), height(num)
head(federer.tobacco)
# a)
library(nlme)
ft.lme=lme(height~dose,random=~1|block, data=federer.tobacco)
# b)
anova(ft.lme,type = "marginal") # dose p=0.1985, not sig

#c)

# d)
library(nlme)
ft.lme=lme(height~dose+row,random=~1|block, data = federer.tobacco)
# e) 
anova(ft.lme, type = "marginal") # dose, p=0.0281, row p<0.001, both *** sig

#f)


coef(summary(ft.lme)) # -128

#g)

#h) model assumption assess
library(car)
qqPlot(resid(ft.lme))

shapiro.test(resid(ft.lme)) # test of normal distribution, p=0.570

plot(ft.lme)
# test of equal variance
bartlett.test(height ~ interaction(row, dose), data = federer.tobacco)
leveneTest(height ~ interaction(row, dose), data = federer.tobacco) # p=0.525

# i)
# the equal variance test for bartlett.test is duable, because of only one data, more replicate need to be done
# j)
library(multcomp)
summary(glht(ft.lme, mcp(dose = "Sequen")))

library(emmeans)
ft.emmeans=emmeans(ft.lm_dose, pairwise~dose)
ft.emmeans$contrasts


##problem 2

# how to decide in the analysis, which one is block, which one is main plot and which one is subplot
library(agridat)
mydata=gregory.cotton
str(mydata) #144 obs. of  6 variables: yield, year, nitrogen, date, water, spacing

library(ggplot2)
ggplot(mydata, aes(date, yield, group=year))+geom_point()+facet_grid(year~nitrogen)
# a) 
with(mydata, tapply(yield, list(date, year), mean))
#b)
library(nlme)
mydata.lm=lm(yield~ nitrogen*date,  data=mydata)
summary(mydata.lm)

# c)
anova(mydata.lm)

# f)
library(emmeans)
mydata.emmeans=emmeans(mydata.lm, pairwise~date|nitrogen)
mydata.emmeans$contrasts
# with year as block 
# g)

mydata$yield(which(mydata$year=="Y2"& mydata$nitrogen=="N1"&mydata$date=="D4"))
which(mydata$year=="Y1"& mydata$nitrogen=="N1"&mydata$date=="D4")

mydata$yield[136:144]=c(mydata$yield[64:72]) # replace the value in Y2, D4, N2 WITH Y2, D4, N2

#h)

mydata.lm.full=lm(yield~nitrogen*date*year, data=mydata)
coef(mydata.lm.full)

predict(mydata.lm.full,data.frame(date="D4", nitrogen="N1", year="Y2"))

(1.317+1.455+0.110-0.328-1.148-0.628+0.664-0.259) # 1.183
