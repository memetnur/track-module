#bismillah prüfung
## Problem 1
#load dataset
install.packages("agridat")
library("agridat")
library(nlme)
library(devtools)


#abbreviation: randomized complete block design (RCBD)

data=federer.tobacco
nrow(data)
ncol(data)
summary(data)
#data description: 56 row with 4 colomns,namely "row", "block", "dose" and "height in  cm"
#20 tobacco plants per treatment and block-> 7 different doses of radiation (dose, in roentgen)

#do turn dose, block and row int o factors:
## 1a.) Give the R code to fit a suitable parametric model to this data set to answer the research question. 
dose=federer.tobacco$dose = factor(federer.tobacco$dose)
block=federer.tobacco$block = factor(federer.tobacco$block)
row=federer.tobacco$row = factor(federer.tobacco$row)
dose # Levels: 0 250 500 1000 1500 2500 5000
block # Levels: 1 2 3 4 5 6 7 8
str(data)
#lme = linear mixed effect
fed.lme <- lme(height ~ dose , random = ~1 | block, data = federer.tobacco)
plot(height~dose, data = data)
#discrete zahlen / faktoren liegen auf der horizontalen (hier dose=X)


## 1. b.) Is the dose effect significant according to the model from a.)? Name the tools used, give
## the R code to perform the tests, report the p value and use it to answer the question

anova(fed.lme, type="marginal")
#Result: A marginal F test showed no significant dose effect (p = 0.199).

## 1. c) 
##interpretation: forecasting residual plot
#fitted values= each observation in a time series can be forecast using all previous observations
#residuals = The "residuals" in a time series model are what is left over after fitting a model.

"""residuals versus fitted plot is useful to illustrate if a linear model presents
1. non linear relationship between the response variable and predictors,
2. heteroscedasticity 
Results: The residuals are observed values ??? fitted values. Accordingly, the rows 2, 3, and 4 (with
negative residuals) seem to produce systematically lower values than the rows 1,5, and 7.

"""
## 1. d.) To take care of the mentioned gradient, one can add the fixed effect of the row to the
## model. Give the R code to fit this model.
fed.grad <- lme(height ~ dose+row, random = ~1 | block, data = federer.tobacco)
fed.grad

## 1. e) According to the model in d.), is the radiation effect significant? And the row effect? Use
## the same tools as in b.), give your R code, p values and your decision. 
anova(fed.grad, type="marginal")
#Results: 
"""According to the marginal F tests, the row (p < 0.001) and the radiation dose (p = 0.028)
both have a significant effect on the average total height."""

## 1. f)
fed.grad
fixef(fed.grad)
#Results: dose was 5000 -> 128.80 
"""According to the fixed effects estimates, the average decrease of the total height is equal to 128.8 cm
"""
## 1. g) Is the answer to f.) different for different rows or not? Why or why not?

"""
Since the model has no dose × row interaction (the row effect is additive), the dose effect
does not depend on the row.
"""
#1.h) Check assumptions of final model
library(car)
qqPlot(resid(fed.grad))
shapiro.test(resid(fed.grad))
  
plot(fed.grad)
#results: p-value = 0.726
""" Residual normality is unproblematic according to the normal QQ plot and the ShapiroWilk test (p = 0.726).
"""
#saphiro-test: auswertung mittels p-Wert:
"""In der Regel wird die Nullhypothese abgelehnt, wenn der {\displaystyle p}p-Wert kleiner ist als das vorgegebene Signifikanzniveau.
"""

##1. i) > 
fed.het <- lme(height ~ dose + row, random = ~1 | block, weights = varIdent(form = ~ dose), data = federer.tobacco)
plot(fed.het)

## 1. j)
library(multcomp)
summary(glht(fed.grad, mcp(dose = "Tukey")))
"""
This is implemented in multcomp's glht. The only significant difference is found between
the 2500 and the 5000 roentgen dose (adjusted p = 0.0356)."""

gc=gregory.cotton
#data inspection:144 row with 6 column, namely "yield", "year", "nitrogen", "date", "water" and "spacing".
summary(gc)
head(gc)

""" for this problem, we focus only on the effect of the sowing date, nitrogen, and the year on the yield.
"""
## 2. a)

gc
with(gc[gc$year== "Y1",], tapply(yield, list(date, nitrogen), mean))
## 2.b) model with effect of date, nitrogen and their interaction on the yield.
gc.lm.base=lm(yield~date*nitrogen, data=gc)

##2.c) 
library(car)
Anova(gc.lm.base, type = 2)

## 2. f) 
fixef(summary(gc.lm.base))
coef(summary(gc.lm.base))

sort(gc$yield[gc$date == "D4" & gc$year == "Y2" & gc$nitrogen == "N1"])

gc.full=lm(yield~date*nitrogen*year, data=gc)
Anova(gc.full, type=2)
df.pred=data.frame(date="D4", nitrogen="N1", year="Y2")
df.pred
predict(gc.full, df.pred)
