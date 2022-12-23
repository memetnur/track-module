## 
library(tidyverse)
library(car)
library(nlme)
library(ggplot2)
library(multcomp)
library(agricolae)
library(dplyr)
library(MASS)
library(caret)
library(ibd)
library(emmeans)
library("utils")

library(NSM3)
library(asbio)
library(lmPerm)
library(emmeans)
library(MuMIn)
library(agridat)

yield=read.csv("yield_data.csv", header = TRUE, sep = ",")

str(yield) # 48 obs. of  5 variables X, yield, fert, irri, blk

mydata1=readRDS("output_data.rds")

str(mydata1) #40 obs. of  4 variables: X, output, temp, press


ggplot(yield, aes(x=blk, y=yield, col=irri)) +geom_point()+facet_grid(~fert)
ggplot(df, aes(x=temp, y=output, col=press)) +geom_point()+ facet_grid(~press)

# 1
str(mydata1)
head(mydata1)
with(mydata1, tapply(output, list(press, temp), mean))
with(mydata1, tapply(output, list(press, temp), sd))
with(mydata1, tapply(output, list(press, temp), length))
with(mydata1, tapply(output, list(press, temp), summary))
with(mydata1, table(output, list(press, temp), summary))

ggplot(mydata1, aes(x=temp, y=output, col=press)) +geom_point()

mydata1.lm=lm(output~press*temp, mydata1)
summary(mydata1.lm)
anova(mydata1.lm)

library(car)
Anova(mydata1.lm, type=2)

qqPlot(resid(mydata1.lm))

shapiro.test(resid(mydata1.lm))

plot(mydata1.lm)

bartlett.test(output~interaction(temp,press), data=mydata1)
leveneTest(output~interaction(temp,press), data=mydata1)

library(emmeans)
emmeans(mydata1.lm, "press")

coef(summary(mydata1.lm))

summary(mydata1.lm)$coefficients



mydata2=readRDS(("yield_data.rds"))
str(mydata2)
with(mydata2, table(fert, irri))

ggplot(mydata2, aes(x=fert, y=yield, col=blk)) +geom_point()+geom_line()+facet_grid(~irri)

mydata2.lme=lme(yield~fert*irri, random = ~1|blk, mydata2)

summary(mydata2.lme)

dat=expend.grid()

predict(mydata2.lm)
predict()
anova(mydata2.lme, type = "marginal")

Anova(mydata2.lme, type =2)

mydata2.lme_2=lme(yield~fert+irri, random = ~1|blk, mydata2)
summary(mydata2.lme_2)

fixef(mydata2.lme_2)
ranef(mydata2.lme_2)

mydata2.lm=lm(yield~fert+irri, mydata2)
Anova(mydata2.lme_2, type =2)
Anova(mydata2.lm, type =2)

summary(mydata2.lm)$coefficents

qqPlot(resid(mydata2.lm))
shapiro.test(resid(mydata2.lm))

plot(mydata2.lm)

bartlett.test(yield~interaction(fert,irri), data=mydata2)

leveneTest(yield~interaction(fert,irri), data=mydata2)

coef(mydata2.lm)

df <- expand.grid(fert = levels(mydata2$fert), irri = levels(mydata2$irri))
df$pred <- predict(mydata2.lm, df, level = 0)

expand.grid(fert = levels(turnip$gen),
            + density = levels(turnip$density))
pred <- predict(object = turnip.sqrt, newdata = dat,
                  + interval = "confidence")

emmeans(mydata2.lm, "irri")

summary(glht(mydata2.lm))

summary(glht(mydata2.lm, mcp(irri = "Dunnet")))
summary(glht(mydata2.lm, mcp(irri = "GrandMean")))

ref_grid(ins.lme)
pairs(emmeans(mydata2.lm, "irri"))

pairs(emmeans(mydata1.lm, "press"))
