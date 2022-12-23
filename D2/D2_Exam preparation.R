#bismillah
#author: nurdzane memeti
#date: 31.01.20

#load libraries
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


############# Abbreviation ############# 
# abbreviation: randomized complete block design (RCBD)


############# Problem 1 ############# 

# load data set
#mydata1 <- read_rds("tobacco_data.rds")
mydata1 <- tobacco_data
str(mydata1)

# 1.1 suitable descriptive statistics

table(mydata1$block, mydata1$dose)

summary(mydata1)

mydata1 %>% group_by(dose) %>% summarise(mean_h = mean(height),
                                         sd_h = sd(height))


# 1.2 suitable graphical representations of the data set

library(ggplot2)
ggplot(mydata1, aes(x= dose, y = height)) +
  geom_point() +
  facet_grid(~ block)


# 1.3. fit a suitable parametric model to this data set. Perform an overall F-test, 
# report its p-value and state your conclusion

library(nlme)

mymodel1.lme <- lme( height ~ dose, random = ~1| block, data = mydata1)

anova(mymodel1.lme, type = "marginal")

# p = 0.198 --> marginal F test did not show significant dose effect



# 1.4. Assess the model assumptions for your final model: explain what you assess, 
# with which method, give your R code, discuss the results and state your conclusions

qqPlot(resid(mymodel1.lme))
shapiro.test(resid(mymodel1.lme))

plot(mymodel1.lme, which = 1)
plot(fitted(mymodel1.lme), resid(mymodel1.lme), las = 1, xlab = "Fitted values", ylab = "Residuals"); abline(h=0)
bartlett.test(height ~ dose, data = mydata1)
leveneTest(height ~ dose, data = mydata1)
boxplot(height ~ dose, xlab='Dose', ylab='Height', data=mydata1)

# Here discuss if: 
# 1. the observations are mutually independent
# 2. Normality
# 3. Equal variance : Barlett / Levene test, boxplot. If p > 0.05 samples have equal variance = homoskedacity



############# Problem 2 ############# 

# 2.1 which of the four dates should we choose to get the highest average yield
# if we use the nitrogen fertilizer, and how high is the average yield for this
# sowing date in year Y1 for the fertilized plots?

mydata2 <- cotton_data

myd2 <- mydata2 %>% select(yield, year, nitrogen, date) # select only the variables that I want

myd2 %>% group_by(year, nitrogen, date) %>% summarise(mean(yield))

# From the displayed summary, for year Y1 and nitrogen N1 we see the greatest 
# mean value is 3.07 and it corresponds to date D2.


# 2.2 We begin with a model that has no year effect. Give the R code to model 
# the effect of the sowing date, the nitrogen fertilizer and their interaction 
# on the yield.

# design with factorial treatment structure

mymodel2.lm <- lm(yield ~ nitrogen*date, data = myd2)

# 2.3 Does the effect of the nitrogen fertilizer on the yield depend on the date
# according to the model from b.)? Give your R code, and answer the question, 
# argue with p values.

Anova(mymodel2.lm, type = 2)

# p.value < 0.05 in Anova two-way method, which means the interaction effect 
# between the nitrogen fertilizer and sowing date is highly significant.


# 2.4 Add the year to the model. Keep in mind that the year effect could also 
# depend on the levels of the nitrogen and the date. Give your R code to fit 
# a suitable model. (1) 

# a model with three-way interaction:
n_model <- lm(yield ~ nitrogen*date*year, data = myd2)
Anova(n_model, type = 2)


######################### Real exam June 2019 ######################### 

########## Problem 1 ##########

library(agridat)
federer.tobacco
mydata_1 <- federer.tobacco

mydata_1$dose <- factor(mydata_1$dose)
mydata_1$block <- factor(mydata_1$block)
mydata_1$row <- factor(mydata_1$row)

# 1.a) fit parametric model

mymodel_1.lme = lme(height ~ dose, random = ~ 1 | block, data = mydata_1) # RCB

# 1.b) is the dose significant acc to the model from a? 

anova(mymodel_1.lme, type = "marginal")

# tool = anova one-way with blocking, p.value = 0.198 which shows dose does not 
# have significant effect.

# 1.c) The residuals values are obtained by subtracting fitted values from the observed values.
# By analyzing this plot, we see that residuals have negative values for rows 2,3 and 4 and 
# positive values for rows 1,5,6 and 7. 

# 1.d) 

n_model_1.lme <- lme(height ~ dose + row, random = ~ 1 | block, data = mydata_1)

# 1.e)

anova(n_model_1.lme, type ="marginal")

# I used a marginal F test and obtained a p value ofp-values are 0.0181 and <0.0001 which are less than 0.05.
#the radiation dose and row have significant effect on the average height, as their 


# 1.f) 

fixef(n_model_1.lme) # extracting the fixed effect from the newmodel_1.lme

# for dose of 5000roentgen, the decrease in height has the value of 128.8105

# 1.g)

# Each row has a different value for the average height. Since the model has 
# no dose Ã— row interaction (the row effect is additive), the dose effect
# does not depend on the row.

# 1.h) Assess the model assumptions: explain what you assess, with which method, 
# give your R code, discuss the results (give p values where you calculate them) 
# and report your conclusions regarding the assumptions.


library(car)
qqPlot(resid(n_model_1.lme))
shapiro.test(resid(n_model_1.lme))
plot(n_model_1.lme)
# p.value of shapiro.test shows that distribution of residuals is normal. But when plotting the 
# residuals, it seems that the variance of the residuals seems to increase with the
# fitted values.

# 1.i)
# When we see heteroskadicty of residuals in our model, we can re-build it with new predictors.

# First we try with varFunc objects, specifically with varIdent, to show a separate variance for each level of a factor.

n_model_1.lme.het <- lme(height ~ dose + row, random = ~1 | block, data=mydata_1, weights = varIdent(form = ~ dose))
n_model_1.lme.het
plot(n_model_1.lme.het)

# But still the variance is not well explained by the dose or the row. Another method is 
# to add the residuals of the original model as a predictor and rebuild the
# regression model through Box-Cox transformation. This is not recommended, but it is an approach if all available options fail.


heightBCMod <- BoxCoxTrans(mydata_1$height)
print(heightBCMod)

mydata_1 <- cbind(mydata_1, height_new=predict(heightBCMod, mydata_1$height))
head(mydata_1)

model_bc <- lme(height_new ~ dose + row, random = ~ 1 | block, data = mydata_1)
plot(model_bc)



# 1.j) We want to compare each of the dose levels only to its adjacent levels ( 0 to 250, 250 to 50, ...). Show how to test
# which of these differences are significant and give the significant results only, with p.values.

summary(glht(n_model_1.lme, mcp(dose = "Sequen")))

# The only significant difference is found between the 2500 and the 5000 roentgen dose (adjusted p = 0.0356).


########## Problem 2 ##########

# 2.a)

mydata_2 <- gregory.cotton

myd_2 <- mydata_2 %>% select(yield, year, nitrogen, date)

myd_2 %>% group_by(year,nitrogen,date) %>% summarize(mean(yield))

# We see that the highest yield mean value in Y1 for N1 is 3.07 and it corresponds to sowing date D2.

# 2.b) # factorial design

myd_2.lm <- lm(yield ~ nitrogen*date, data = mydata_2)

# 2.c)
Anova(myd_2.lm, type = 2)

# The factors interaction between nitrogen fertilizer and sowing date has a significant effect on the average yield value, 
# as showed by the p-value =0.001748, which is less than 0.05

# 2.d)
# The date has 4 levels and nitrogen has 2 levels, their interaction effect involves (4-1)(3-1)=3 terms.
# This is also showed in the Anova test output above.

# 2.e)
# This interaction plot shows that the lines for the two nitrogen levels are more or less paralled for dates D1,D2 and D3.
# For D4 we can see that the lines are more closer, therefore interaction between D4 and N1 might be the most important term.

# 2.f) significance tests for the interaction terms to check 2.e)

summary(myd_2.lm)

# As shown in the Coefficients table from summary, the interaction nitrogen N1:dateD is the only significant interaction effect.

# 2.g)

library(ggplot2)
ggplot(mydata_2, aes(x= date, y = yield)) +
  geom_point() +
  facet_grid(year ~ nitrogen)

print(myd_2$yield[myd_2$date == "D4" & myd_2$year == "Y2" & myd_2$nitrogen == "N1"])
sort(myd_2$yield[myd_2$date == "D4" & myd_2$year == "Y2" & myd_2$nitrogen == "N1"])


# 2.h) add the year to the model

n_myd_2.lm <- lm(yield ~ nitrogen*date*year, data = mydata_2)


# 2.i)

Anova(n_myd_2.lm, type = 2)

# Only the nitrogen:year interaction is significant (p-value = 0.027), this means that the 
# nitrogen effect depends on the year according to the interaction model.

# 2.j)

# fixef(n_myd_2.lm) # it cannot be applied to an object lm

df.pred <- data.frame(date="D4", nitrogen="N1",year="Y2")
predict(n_myd_2.lm, df.pred)

# The fitted average yield is 1.185556. This seems compatible with the data we plotted above. 



########## Problem SOY data set ##########

# a)

sb <- soybean
sb$gen = as.factor(sb$gen)
sb$loc = as.factor(sb$loc)
sb$block = as.factor(sb$block)

library(ggplot2)
ggplot(soybean, aes(y=yield, x=gen)) +
  geom_point() +
  facet_grid(block~loc)

sb %>% group_by(gen) %>% summarise(mean(yield))

# The highest average yield ( 1566 ) is produced by genotype N72-3148 

# b)

sb.lm <- lm(yield ~ loc*gen, data = sb)
Anova(sb.lm, type = 2)

# Anova type 2 suggests that location and genotype have a significant effect on the average yield,
# but the interaction between these two factors has no significant effect.

# c) tool = Anova type 2, p.value for location <2.2e-16, p.value for genotype is 4.639e-05 and p.value
# for loc:gen interaction is 0.22 (which >0.05)

# d) According to Anova type 2 results, the interaction location genotype does not
# have a significant effect.

# e)
sb.lm.add <- lm(yield ~ loc + gen, data = sb)
HSD.test(sb.lm.add, "gen", group = TRUE, console = TRUE)
sb$gen <- factor(sb$gen, levels = c("N72-3058", "Centennial", "D74-7741", "N72-137", "N72-3148", "N73-1102", "N73-693","N73-877","N73-882","R73-81","R75-12","Tracy"), ordered = TRUE)

summary(glht(sb.lm.add, mcp(gen = "Dunnett")))
# The only significant different genotype from N72-3058 is N72-3148

# f) 

summary(sb.lm.add)
coef(sb.lm.add)

pred.df <- data.frame(gen="Centennial", loc="Plymouth") 
predict(sb.lm.add, pred.df)

# 1381.861 kg on average for this genotype in this location

# g) How high is the variance of the yield values, and what proportion of it it is explained with the model from e)

summary(sb.lm.add)$r.squared # r.squared explains the proportion of the variance

var.yield <- var(sb$yield) # the variance of the yield values

# r.squared <- var(fitted(sb.lm.add)) / var.yield
# c(var.yield, r.squared)


# h)

qqPlot(resid(sb.lm.add)) # distribution looks normal
shapiro.test(resid(sb.lm.add)) # confirms the normality
plot(sb.lm.add) # double check

bartlett.test(yield ~ interaction(gen,loc), data = sb)
leveneTest(yield ~ interaction(gen,loc), data = sb) # equal variance of residuals


############# SOY problem - account the block effect ############# 

sb_bl.lme <- lme(yield ~ gen*loc, random = ~1 | block, data = sb)

anova(sb_bl.lme, type = "marginal")
sb_bl.lme


library("tidyverse")
data("InsectSprays") # is a GRCBD with two replications per treatment and block.
summary("InsectSprays")

## 6.1 Data structure------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6)) ## design
head(InsectSprays)

## 6.2 fitting the mixed effect model ------------------------------------------------------------------------
library(nlme) 
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)

## 6.3 extracting model results with model "summary"------------------------------------------------------------------------
summary(ins.lme) #extract the REML parameter estimates
#and see that ?? = 0.579, ^??block = 0.254. This shows that the block effect is not negligible compared to the errors.
#Dies zeigt, dass der Blockeffekt nicht vernachlässigbar ist im Vergleich zu den Fehlern. 

## REML estimates of fixed effects with t-tests and the correlations of the fixed effects estimators------------------------------------------------------------------------
fixef(ins.lme)#extract fixed effects


## 6.3.2 random effects -> are technically not estimated. but predicted------------------------------------------------------------------------
ranef(ins.lme)
#how many random effects are there for the insect sprays data? - one for each block. extract them with ranef
ranef(ins.lme)
ranef(ins.lme)$`(Intercept)`#as a vector
#interpretation: negative values mean that this block has less insects than a typical block (with random effect of zero) to the overall mean.
#interpretation: positive values mean that this block has more insects than a typical block (with random effect of zero) to the overall mean.

## ------------------------------------------------------------------------
blk.centred <- with(InsectSprays, tapply(sqrt(count), block, mean) -
                      mean(sqrt(count)))
blk.ranef <- ranef(ins.lme)$`(Intercept)`
cbind(blk.centred, blk.ranef, ratio = blk.centred/blk.ranef)

## 6.3.3 Fitted values and residuals------------------------------------------------------------------------
#fitted values include the predictions for the random effects - within-group fitted values 
head(fitted(ins.lme))
head(fitted(ins.lme, level = 0)) #exclude all random effects

## residuals: observed values - fitted values------------------------------------------------------------------------
head(resid(ins.lme))

## 6.4 model diagnostics------------------------------------------------------------------------
plot(ins.lme)

## ------------------------------------------------------------------------
qqPlot(resid(ins.lme), id=FALSE)
shapiro.test(resid(ins.lme))

## test the normality of the random effects:------------------------------------------------------------------------
shapiro.test(ranef(ins.lme)$`(Intercept)`)

## 6.5 Confidence intervals and a hypothesis test ------------------------------------------------------------------------
intervals(ins.lme) #they give a first impression about the precision.

## F-test------------------------------------------------------------------------
anova(ins.lme, type = "marginal")
# yields a p= value <.0001 which is a significant effect of the spray
# The type = "marginal" argument specifies that marginal F tests are required, this is the recommended procedure, as in ANOVA
# models without blocks.

## 6.6 Treatment comparisons------------------------------------------------------------------------
library(multcomp)
#because the overall F test was significant we want to compare the different insect sprays. 
summary(glht(ins.lme, mcp(spray = "GrandMean")))

## comparing treatments (complex setting)/unbalanced dataset!!!!------------------------------------------------------------------------
library(emmeans)  #least squares means (lsmeans) / estimated marginal means(emmeans)
#for now: simply show how to obtain confidence intervals for each mean and how to perform pairwise comparison(Tukey)

ref_grid(ins.lme)
(ins.emm <- emmeans(ins.lme, "spray"))
pairs(ins.emm)

## comparison plot (mit pfeilen = TRUE)------------------------------------------------------------------------
plot(ins.emm, comparisons = TRUE)

## ------------------------------------------------------------------------
ins.lm <- lm(sqrt(count) ~ spray, data = InsectSprays)
sigma(ins.lm)
sigma(ins.lme)

## ------------------------------------------------------------------------
(V <- VarCorr(ins.lme))
V <- as.numeric(V)
V[1] / (V[1] +V[2])

## ------------------------------------------------------------------------
library(MuMIn)
r.squaredGLMM(ins.lme)

## 6.10 Friedmann test and r=1 ------------------------------------------------------------------------
#varieties Var (M,P, S, T and V) of barley grown in six location Loc c (C, D, GR, M, UF and W)

library(MASS)
library(ggplot2)
immer
ggplot(immer, aes(x = Var, y = Y1)) +
  geom_point() +
  facet_grid(~Loc)

## A terrible analysis------------------------------------------------------------------------
anova(lm.immer <- lm(Y1 ~ Var, immer))
sigma(lm.immer)

## A slightly less terrible analysis------------------------------------------------------------------------
anova(lm(Y1 ~ Loc + Var, immer)) #We include location as a fixed effect.

## A parametric analysis-->Let us include the location as a random effect. ------------------------------------------------------------------------
immer.lme <- lme(Y1 ~ Var, random = ~ 1 | Loc, data = immer)
immer.lme
anova(immer.lme)

## ------------------------------------------------------------------------
plot(immer.lme)
qqPlot(resid(immer.lme))
shapiro.test(resid(immer.lme))

## a nonparametric appraoch of a null hypothesis-> RCBD------------------------------------------------------------------------
#idea is to rank observations within blocks and then sum ranks for the treatments over all the blocks
#ranking makes the procedure robust to outliers! 
friedman.test(Y1 ~ Var | Loc, immer)

## 6.11 Interactions of block effects and treatment effects------------------------------------------------------------------------
#here:  GRCBD (more than one observation per treatment and block),
#plotting means for each combination of block and treatments 
with(InsectSprays, interaction.plot(spray, block, sqrt(count))) #interaction plot


## nested random effects (LME) / within each block------------------------------------------------------------------------
ins.lme.x <- lme(sqrt(count) ~ spray, random = ~ 1 | block/spray,
                 data = InsectSprays)
ins.lme.x
ranef(ins.lme.x) ## run this code and look at the results!

## ------------------------------------------------------------------------
anova(ins.lme, ins.lme.x)
1 - pchisq(1.360439, df = 9 - 8) # show p value calculation
#there is no evidence for an interaction of treatment effects and blocks.
#the simple model is sufficient

## 7 Designs with factorial treatment structure------------------------------------------------------------------------
library(agridat)#like in exam of 2019
#planting density in kg/ha affects the mean yield of turnips of 2 genotypes
#balanced factorial two-way layout with r = 8 replications per factor combination
turnip <- mcconway.turnip
turnip$density <- as.factor(turnip$density)
turnip
summary(turnip)

#data description: 2 gen (marco and barkant je 32 samples), 4 block with 16 treatments with mean 5.377
## ------------------------------------------------------------------------
ggplot(turnip, aes(x = density, y = yield)) +
  geom_point() +
  facet_grid(~gen)

## ------------------------------------------------------------------------
turnip[c(1:4,61:64),]

## means for each combination of density and genotype------------------------------------------------------------------------
with(turnip, tapply(yield, list(gen, density), mean))

#questions:
#1)is there a planting density effect? it seems so when inspecting the plot and the means: the yield increase with the planting density
#2)is there a genotype effect? the average barkant values seem to be higher than the average Marco values
#3)does the planting densiy effect (if there is one) vary among the 2 genotypes?
#several one-way ANOVA to anser the first 2 questions

## 7.2 Visualization------------------------------------------------------------------------
qplot(density, yield, data = turnip, 
      facets = ~block, shape = gen, size = I(2))

## ------------------------------------------------------------------------
ggplot(turnip, aes(x = density, linetype = gen, group = gen, y = yield)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

## a two-way ANOVA with interaction------------------------------------------------------------------------
turnip.full <- lm(yield ~ gen * density, data = turnip)
coef(summary(turnip.full))

## same results like above------------------------------------------------------------------------
with(turnip, tapply(yield, list(gen, density), mean))

## 7.4 F tests ------------------------------------------------------------------------
#question: model using the 2 factors and their interaction is any better than just predicting the overall mean for every observation.
#null model, that all coefficients alpha, beta and gama are equal to zero, which is tested the oerall F-test
summary(turnip.full)
## last row: F-statistic: 4.338 on 7 and 56 DF,  p-value: 0.0006739

## 7.4.2 The ANOVA table and the F tests for main and interaction effects ------------------------------------------------------------------------
anova(turnip.full)
library(car)
Anova(turnip.full, type = 2)

## ------------------------------------------------------------------------
turnip.main <- lm(yield ~ gen + density, data = turnip)

## 7.6 Model diagnostics and related topics------------------------------------------------------------------------
library(car)
qqPlot(resid(turnip.full), xlab = "Normal quantiles", ylab = "Residuals")

## 7.6.2 Testing normality------------------------------------------------------------------------
shapiro.test(resid(turnip.full))
#yields a p value of 0.0132. the null hypothesis of normality is rejected. and the residuals indeed have problems with normality

## 7.6.3 Accounting for non-normality------------------------------------------------------------------------
turnip.sqrt <- lm(sqrt(yield) ~ gen * density, data = turnip)
qqPlot(resid(turnip.sqrt))
shapiro.test(resid(turnip.sqrt))

## 7.6.4 Residual plots------------------------------------------------------------------------
plot(fitted(turnip.sqrt), resid(turnip.sqrt), xlab = "Fitted values", 
     ylab = "Residuals", las = 1); abline(h = 0)

## 7.6.5 Testing variance homogeneity------------------------------------------------------------------------
bartlett.test(sqrt(yield) ~ interaction(gen, density), data = turnip)
leveneTest(sqrt(yield) ~ interaction(gen, density), data = turnip)
#To perform Bartlett's test and the Levene test in two-way ANOVA
#We obtain p = 0.0074 for Bartlett's and p = 0.0072 for Levene's test, both clearly rejecting the null hypothesis of equal variances.

## 7.6.6 Accounting for heterogeneous variances ------------------------------------------------------------------------
library(car)
Anova(turnip.sqrt, white.adjust = "hc3")

## 7.7 Fitted values and confidence intervals------------------------------------------------------------------------
coef(turnip.sqrt)

## ------------------------------------------------------------------------
dat <- expand.grid(gen = levels(turnip$gen), 
                   density = levels(turnip$density))
pred <- predict(object = turnip.sqrt, newdata = dat,
                interval = "confidence")
(ci <- cbind(dat, pred^2))

## 7.8 Post hoc tests------------------------------------------------------------------------
#to compare the yield of the 2 genotypes
emmeans(turnip.sqrt, pairwise ~ gen)
#On average, over all the planting densities, the Barkant genotype produces significantly higher yields.

## ------------------------------------------------------------------------
turnip.sqrt.gen <- emmeans(turnip.sqrt, pairwise ~ gen | density)
turnip.sqrt.gen$contrasts

## ------------------------------------------------------------------------
emmeans(turnip.sqrt, pairwise ~ gen + density)

## 7.9 Unbalanced designs------------------------------------------------------------------------
anova(turnip.sqrt)
Anova(turnip.sqrt, type = 2)

## 7.10 Accounting for blocks and for heteroskedasticity------------------------------------------------------------------------
turnip.lme <- lme(sqrt(yield) ~ gen * density, random = ~ 1 | block,
                  data = turnip)

## ------------------------------------------------------------------------
plot(turnip.lme)                  
qqPlot(resid(turnip.lme.add))
shapiro.test(resid(turnip.lme.add))    

## ------------------------------------------------------------------------
turnip.lme.het <- lme(sqrt(yield) ~ gen * density, random = ~ 1 |block,
                      data = turnip,
                      weights = varIdent(form = ~ 1 | density))
turnip.lme.het
plot(turnip.lme.het)  

## ------------------------------------------------------------------------
anova(turnip.lme.het, type = "marginal")

## ------------------------------------------------------------------------
turnip.lme.het.add <- lme(sqrt(yield) ~ gen + density, random = ~ 1 |block,
                          data = turnip,
                          weights = varIdent(form = ~ 1 | density))
anova(turnip.lme.het.add, type = "marginal")



data("InsectSprays")

## Part III - 8 Classical analysis (ANOVA tables)------------------------------------------------------------------------
ins.aov <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(ins.aov) #produce analysis of variance table for analysis of a completly randomized block design. 

## 8.2 Complete blocked designs------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6))
summary(aov(sqrt(count) ~ spray + Error(block), data = InsectSprays)) #error= random block effect

## mixed model with nlme------------------------------------------------------------------------
library(nlme)
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)
anova(ins.lme, type = "marginal") #The numerator degrees of freedom correspond to the number of parameters we test.

## 9. Incomplete Block Design. ------------------------------------------------------------------------
install.packages("ibd")
library(ibd)
set.seed(1) # to always get the same design
bibd(7, 7, 3, 3, 1)$design

#We produce a design with v = 7 treatments, b = 7 blocks, r = 3 replications of each treatment, block size k = 3 and
#concurrence ?? = 1 (we directly extract the design):

## 9.2.2 The detergent data------------------------------------------------------------------------
detergent <- read.table("detergent.csv", header = TRUE, sep = ",")
head(detergent)
addmargins(with(detergent, table(treatment, block)))
library(ggplot2)
ggplot(detergent, aes(x = treatment, y = plates)) +
  geom_jitter(shape = 4, width = 0.1)
# block effects are not accounted for.

##9.2.3 Fixed effects model - intrablock analysis ------------------------------------------------------------------------
detergent.lm <- lm(plates ~ block + treatment, data = detergent)
anova(detergent.lm) #The treatment effect, adjusted for blocks, is highly significant.
library(car)
Anova(detergent.lm, type = 2) ## If you want Type II sums of squares

## ------------------------------------------------------------------------
theta.hat <- c(0, coef(detergent.lm)[2:12]) ## 0 for block 1 (reference)
theta.avg <- mean(theta.hat)
detergent$block.num <- as.numeric(detergent$block) ## for indexing
detergent$adj <- theta.hat[detergent$block.num] - theta.avg ## note use of []
detergent$plates.adj <- detergent$plates - detergent$adj
head(detergent)

## 9.2.4 Mixed effects model - interblock analysis------------------------------------------------------------------------
detergent.lme <- lme(plates ~ treatment, random = ~1 | block, 
                     data = detergent)
anova(detergent.lme)
#The significant treatment effect is confirmed with the mixed effects model.

## 10. Split-plot design / Visualization of the raw data ------------------------------------------------------------------------
ggplot(Oats, aes(x = nitro, y = yield, col = Variety)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Block)
ggplot(Oats, aes(x = nitro, y = yield, group = Block)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Variety)

## 10.2 Basic statistical model------------------------------------------------------------------------
Oats$nitroF <- factor(Oats$nitro)
Oats.lme <- lme(yield ~ Variety * nitroF, data = Oats,
                random = ~ 1 | Block / Variety)
summary(Oats.lme)
ranef(Oats.lme) ## look at results!

## ------------------------------------------------------------------------
anova(Oats.lme, type = "marginal")

## 10.3 Model reduction------------------------------------------------------------------------
Oats.lme <- lme(yield ~ Variety + nitroF, data = Oats,
                random = ~ 1 | Block / Variety)
anova(Oats.lme, type = "marginal")

## ------------------------------------------------------------------------
Oats.lme <- lme(yield ~ nitroF, data = Oats,
                random = ~ 1 | Block / Variety)

## 10.4 Exploiting order structure------------------------------------------------------------------------
Oats$nitroF <- factor(Oats$nitro, ordered = TRUE)
Oats.lme.o <- lme(yield ~ nitroF, data = Oats,
                  random = ~ 1 | Block / Variety)
coef(summary(Oats.lme.o))


## ------------------------------------------------------------------------
df <- data.frame(nitroF = levels(Oats$nitroF))
df$pred <- predict(Oats.lme, df, level = 0)
df 
coef(summary(Oats.lme)) ## to check

## ------------------------------------------------------------------------
Oats.lme.lin <- lme(yield ~ nitro, data = Oats,
                    random = ~ 1 | Block / Variety)
summary(Oats.lme.lin)

## ------------------------------------------------------------------------
Oats$yield.factor <- predict(Oats.lme)
Oats$yield.lin <- predict(Oats.lme.lin)
ggplot(Oats, aes(x = nitro, y = yield, col = Variety)) +
  geom_point() +
  geom_line(linetype = "solid") +
  geom_line(aes(y = yield.factor, col = Variety), linetype = "dotted") +
  geom_line(aes(y = yield.lin, col = Variety), linetype = "dashed") +
  facet_wrap(~Block)

## ------------------------------------------------------------------------
gasel <- read.table("gasel.csv", sep = ",", header = TRUE)
ggplot(gasel, aes(x = block, y = shannon, col = location)) +
  geom_point()

## ------------------------------------------------------------------------
friedman.test(shannon ~ location | block, data = gasel)

## ------------------------------------------------------------------------
gasel.blk <- lme(shannon ~ location, random = ~ 1 | block,
                 data = gasel)
coef(summary(gasel.blk))

## ------------------------------------------------------------------------
gasel$radiation <- gasel$radiation / 100000
ggplot(gasel, aes(x = radiation, y = shannon)) +
  geom_point(aes(col = location)) +
  geom_line(aes(group=block))

## ------------------------------------------------------------------------
gasel.lme <- lme(shannon ~ radiation * location, random = ~ 1 | block,
                 data = gasel)
coef(summary(gasel.lme))
ggplot(gasel, aes(x = radiation, y = shannon, col = location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## ------------------------------------------------------------------------
gasel.lme <- lme(shannon ~ radiation + location, random = ~ 1 | block,
                 data = gasel)
coef(summary(gasel.lme))
ggplot(gasel, aes(x = radiation, y = shannon, col = location)) +
  geom_point() +
  geom_smooth(aes(y = predict(gasel.lme, gasel)), method = "lm",
              se = FALSE)

## ------------------------------------------------------------------------
gasel.lme <- lme(shannon ~ radiation, random = ~ 1 | block,
                 data = gasel)
fixef(gasel.lme)
ggplot(gasel, aes(x = radiation, y = shannon)) +
  geom_point() +
  geom_smooth(aes(y = predict(gasel.lme, gasel)), method = "lm",
              se = FALSE)





