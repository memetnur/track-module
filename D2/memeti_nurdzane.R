#MOCK EXAM - Version A, Problem 1
#name pdf: Memeti_Nurdzane_exercise1.pdf

#chapter 5:
#statistical and practical consideration : 1) all have the same size, 2) be sufficiently big to apply each treatment at least once per block
#randomized complete block design
#generalized randomized complete block design,


#load libraries
library(tidyverse)

mydata1 <- readRDS("tobacco_data.rds")
view(mydata1)
str(mydata1)

## 1 descriptive data--------------------------------------------------

table(mydata1$block, mydata1$dose)
with(mydata1, tapply(height, block, length)) #7 obs. per block
with(mydata1, tapply(height, block, summary)) #compare distr.
with(mydata1, tapply(height, block, sd)) #standard deviations
#randomized complete block design with eight blocks. 

## 2. visualize-------------------------------------------------

library(ggplot2)

ggplot(mydata1, aes(block, height)) + 
  geom_point(shape = 1, position = position_jitter(width = 0.2, 
                                                   height = 0))
plot(height ~block, data=mydata1, las=1)
#i observe 8 blocks, some with high variance like block 1, 5-7. there is one outlier in block 4. very different samples. 

library(ggplot2)
ggplot(mydata1, aes(x= dose, y = height)) +
  geom_point() +
  facet_grid(~ block)

## 3. parametric model overall F test------------------------------------------------------------------------
library(nlme)
lm=lme(height~dose, random = ~1|block, data=mydata1)
anova(lm,type="marginal")

#yields a p value of 0.1985. Because p < 0.05, we reject the null hypothesis of dose effect.


## 4. Model Assumptions------------------------------------------------------------------------
library(car)
qqPlot(resid(lm), id=FALSE)
qqPlot(rnorm(72, mean = mean(resid(lm)), sd = sd(resid(lm))), id=FALSE)
shapiro.test(resid(lm))
#????yields a p value of 0.5703. Because p < 0.05, we reject the null hypothesis of normality and conclude that the residuals do not come from a normal distribution.


## testing homoskedasticity-----------------------------------------------------------------------
plot(lm, las=1)
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.6,3.6,1.1,1.1)) #try
plot(fitted(lm), resid(lm), las = 1,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)

