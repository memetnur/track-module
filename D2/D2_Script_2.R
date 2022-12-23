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

