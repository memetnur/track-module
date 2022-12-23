#bismillah
#nurdzane memeti

library("tidyverse")

data("InsectSprays")

view(data)
nrow(data)
ncol(data)
#data description: there are 7 blocks with 8 treatments with parameters "dose" and "height"
summary(data)

## 1 descriptive data------------------------------------------------------------------------
with(InsectSprays, tapply(count, spray, length)) #12 obs. per trt
with(InsectSprays, tapply(count, spray, summary)) #compare distr.
with(InsectSprays, tapply(count, spray, sd)) #standard deviations

## 2. visualize-----------------------------------------------------------------------
library(ggplot2)

ggplot(InsectSprays, aes(spray, count)) + 
  geom_point(shape = 1, position = position_jitter(width = 0.2, 
                                                   height = 0))
plot(count ~spray, data=InsectSprays, las=1)

## ------------------------------------------------------------------------
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1)) #try
plot(count ~ spray, data = InsectSprays, las = 1)

## ------------------------------------------------------------------------
library(sciplot)
bargraph.CI(spray, count, col = (gray(0.5)), data = InsectSprays,
            xlab = "spray", ylab = "count", ylim = c(0,30))
lineplot.CI(spray, count, type = "p", data = InsectSprays,
            xlab = "spray", ylab = "count", ylim = c(0,30))

## 6.1 data structure: square-root transformed.------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6)) ## design
head(InsectSprays)

s.o <- with(InsectSprays, reorder(spray, count, mean, order = TRUE))
InsectSprays$spray.o <- factor(InsectSprays$spray, levels = levels(s.o))
b.o <- with(InsectSprays, reorder(block, count, mean, order = TRUE))
InsectSprays$block.o <- factor(InsectSprays$block, levels = levels(b.o))
ggplot(InsectSprays, aes(spray.o, count)) + 
  geom_point() +
  facet_grid( ~ block.o) +
  xlab("spray")

## 3. parametric model overall F test------------------------------------------------------------------------
ins.lm <- lm(count ~ spray, data = InsectSprays)
anova(ins.lm)
aov(ins.lm)

## ------------------------------------------------------------------------
ins.aov <- aov(count ~ spray, data = InsectSprays)
summary(ins.aov)

## ------------------------------------------------------------------------
summary(ins.lm)$sigma

## ------------------------------------------------------------------------
summary(ins.lm)$r.squared

## ------------------------------------------------------------------------
ins.lm.noint <- lm(count ~ spray+0, data = InsectSprays) 
summary(ins.lm.noint)$coefficients

## ------------------------------------------------------------------------
with(InsectSprays, pairwise.t.test(count, spray, "holm"))
with(InsectSprays, pairwise.wilcox.test(count, spray, "holm"))

## ------------------------------------------------------------------------
TukeyHSD(ins.aov, "spray")
library(agricolae)
HSD.test(ins.lm, "spray", group = TRUE, console = TRUE)

## chapter 3------------------------------------------------------------------------
contrasts(InsectSprays$spray)
contrasts(InsectSprays$block)


 ## ------------------------------------------------------------------------
summary(ins.lm)$coefficients
summary(ins.lm)$coefficients

## ------------------------------------------------------------------------
with(InsectSprays, tapply(count, spray, mean))

## ------------------------------------------------------------------------
options("contrasts")

## ------------------------------------------------------------------------
ins.lm.sum <- lm(count ~ spray, data = InsectSprays,
                 contrasts = list(spray = "contr.sum"))
summary(ins.lm.sum)$coefficients

## ------------------------------------------------------------------------
InsectSprays$spray.sum <- InsectSprays$spray
contrasts(InsectSprays$spray.sum) <- "contr.sum"
contrasts(InsectSprays$spray.sum)

## ------------------------------------------------------------------------
library(multcomp)
summary(glht(ins.lm, mcp(spray = "Dunnett")))
summary(glht(ins.lm, mcp(spray = "GrandMean")))
summary(glht(ins.lm, mcp(spray = "Sequen")))
summary(glht(ins.lm, mcp(spray = "Tukey")))


## difference of treatments B and F to treatment A is the same, µB ??? µA = µF ??? µA ------------------------------------------------------------------------
K <- matrix(c(0, 1, 0, 0, 0, -1), nrow = 1)
summary(glht(ins.lm, linfct = K))

## ------------------------------------------------------------------------
summary(glht(ins.lm, mcp(spray = "GrandMean")))

## ------------------------------------------------------------------------
library(car)
qqPlot(resid(ins.lm))

## ------------------------------------------------------------------------
qqPlot(rnorm(72, mean = mean(resid(ins.lm)), sd = sd(resid(ins.lm))))

## ------------------------------------------------------------------------
shapiro.test(resid(ins.lm))
#yields a p value of 0.022. Because p < 0.05, we reject the null hypothesis of normality and conclude that the residuals do not come from a normal distribution.


## testing homoskedasticity-----------------------------------------------------------------------
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.6,3.6,1.1,1.1)) #try
plot(fitted(ins.lm), resid(ins.lm), las = 1,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)

## 4.2.2 Testing homoskedasticity (equal variances) ------------------------------------------------------------------------
bartlett.test(count ~ spray, data = InsectSprays) #Bartlett's test is very sensitive to non-normality
leveneTest(count ~ spray, data = InsectSprays)
#gives p = 9.085·10???5 for Bartlett's and p = 0.004 for Levene's test, both clearly rejecting the null hypothesis of equal variances.

## ------------------------------------------------------------------------
oneway.test(count ~ spray, data = InsectSprays)

## The Kruskal-Wallis test is used to test the null hypothesis------------------------------------------------------------------------
kruskal.test(count ~ spray, data = InsectSprays)
#gives p- value = 1.511e-10. The null hypothesis is clearly rejected for the insect sprays data.

## Wilcoxon rank sum tests for pairwise treatment comparisons------------------------------------------------------------------------
library(NSM3)
with(InsectSprays, pSDCFlig(x = count, g = as.numeric(spray), method=NA))

## 4.5 Robust method------------------------------------------------------------------------
library(asbio)
# Brunner-Dette-Munk It tests the null hypothesis that the distributions of the numeric variable are the same in each treatment.
with(InsectSprays, BDM.test(count, spray)) #Here, this null hypothesis is rejected.

## 4.5 Robust method, especially for data with outliers------------------------------------------------------------------------
with(InsectSprays, trim.test(count, spray, tr = 0.2))

## Permutation methods------------------------------------------------------------------------
library(lmPerm)
anova(lmp(count ~ spray, data = InsectSprays))


data("PlantGrowth")
library(ggplot2)

#for descriptive statistics: summary
with(PlantGrowth, tapply(weight, group, summary)) 
with(PlantGrowth, tapply(weight, group, sd)) 
#visualize
ggplot(PlantGrowth, aes(group, weight)) + 
  geom_point(shape = 1, position = position_jitter(width = 0.2, 
                                                   height = 0))
plot(weight ~group, data=PlantGrowth, las=1) #there are one control group and 2 treatments

#3. Perform the parametric F test and interpret the result
pp.lm <- lm(weight~group, data=PlantGrowth)
anova(pp.lm)
pp.aov <-aov(weight~group, data=PlantGrowth)
anova(pp.aov)
#t the null hypothesis that the three group means are the same can be rejected for a significance level of ?? = 0.05, because p = 0.0159.

# 4. Compare the three groups using Tukey's HSD (do not use the multcomp package
TukeyHSD(pp.aov, "group")
HSD.test(pp.lm, "group", group = TRUE, console = TRUE)

# 5. Now, use the multcomp package to perform Tukey's HSD. Compare with the previous problem.
library(multcomp)
summary(glht(pp.lm, mcp(group = "Tukey")))
summary(glht(pp.lm, mcp(group = "Tukey")))
# 6. Use the multcomp package to compare each of the two treatments to the control
summary(glht(pp.lm, mcp(group = "Dunnett")))

# 7. Judge residual normality visually and with a statistical test. What is your conclusion?
library(car)
qqPlot(resid(pp.lm), id=FALSE)
shapiro.test(resid(pp.lm))

# 8. Do the residuals have equal variances?
plot(pp.lm, which=1)
bartlett.test(weight ~ group, data = PlantGrowth)

# 9. Perform and interpret a Kruskal-Wallis test for the PlantGrowth data.
kruskal.test(weight~group, data=PlantGrowth)

#10. Use a robust method on this data set and interpret the results.
library(asbio)
# Brunner-Dette-Munk It tests the null hypothesis that the distributions of the numeric variable are the same in each treatment.
with(PlantGrowth, BDM.test(weight, group)) 

# 11. Use a permutation test to test for the equality of the three treatment means.
library(lmPerm)
anova(lmp(weight~group, data=PlantGrowth))

## 5. blocking
data("InsectSprays")

## data structure ------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6)) ## design
head(InsectSprays)
data
view(data)

## fitting the model------------------------------------------------------------------------
library(nlme)
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)

## ------------------------------------------------------------------------
summary(ins.lme)

## ------------------------------------------------------------------------
fixef(ins.lme)

## the random effects for prediction------------------------------------------------------------------------
ranef(ins.lme)

## the random effects for prediction------------------------------------------------------------------------
ranef(ins.lme)$`(Intercept)` #Negative values mean that this block has less insects than a typical block, positive values mean higher counts.

## ------------------------------------------------------------------------
blk.centred <- with(InsectSprays, tapply(sqrt(count), block, mean) -
                      mean(sqrt(count)))
blk.ranef <- ranef(ins.lme)$`(Intercept)`
cbind(blk.centred, blk.ranef, ratio = blk.centred/blk.ranef)

## ------------------------------------------------------------------------
head(fitted(ins.lme))
head(fitted(ins.lme, level = 0))

## ------------------------------------------------------------------------
head(resid(ins.lme))

## ------------------------------------------------------------------------
plot(ins.lme)

## normality------------------------------------------------------------------------
qqPlot(resid(ins.lme))
shapiro.test(resid(ins.lme))

## normality------------------------------------------------------------------------
shapiro.test(ranef(ins.lme)$`(Intercept)`)

## ------------------------------------------------------------------------
intervals(ins.lme)

## ------------------------------------------------------------------------
anova(ins.lme, type = "marginal")

## ------------------------------------------------------------------------
library(multcomp)
summary(glht(ins.lme, mcp(spray = "GrandMean")))

##  comparing treatments with least square means------------------------------------------------------------------------
library(emmeans)
ref_grid(ins.lme)
(ins.emm <- emmeans(ins.lme, "spray"))
pairs(ins.emm)

## ------------------------------------------------------------------------
plot(ins.emm, comparisons = TRUE)

## ------------------------------------------------------------------------
ins.lm <- lm(sqrt(count) ~ spray, data = InsectSprays)
sigma(ins.lm)
sigma(ins.lme)

## The intraclass correlation coefficient------------------------------------------------------------------------
(V <- VarCorr(ins.lme))
V <- as.numeric(V)
V[1] / (V[1] +V[2])

## The coefficient of determination------------------------------------------------------------------------
library(MuMIn)
r.squaredGLMM(ins.lme)

## example: immer data and the Friedman test------------------------------------------------------------------------
library(MASS)
library(ggplot2)
ggplot(immer, aes(x = Var, y = Y1)) +
  geom_point() +
  facet_grid(~Loc)

## ------------------------------------------------------------------------
anova(lm.immer <- lm(Y1 ~ Var, immer))
sigma(lm.immer)

## ------------------------------------------------------------------------
anova(lm(Y1 ~ Loc + Var, immer))

## ------------------------------------------------------------------------
immer.lme <- lme(Y1 ~ Var, random = ~ 1 | Loc, data = immer)
immer.lme
anova(immer.lme)

## ------------------------------------------------------------------------
plot(immer.lme)
qqPlot(resid(immer.lme))
shapiro.test(resid(immer.lme))

## a nonparametric analysis------------------------------------------------------------------------
friedman.test(Y1 ~ Var | Loc, immer)

## ------------------------------------------------------------------------
with(InsectSprays, interaction.plot(spray, block, sqrt(count)))


## ------------------------------------------------------------------------
ins.lme.x <- lme(sqrt(count) ~ spray, random = ~ 1 | block/spray,
                 data = InsectSprays)
ins.lme.x
ranef(ins.lme.x) ## run this code and look at the results!

## ------------------------------------------------------------------------
anova(ins.lme, ins.lme.x)
1 - pchisq(1.360439, df = 9 - 8) # show p value calculation

## ------------------------------------------------------------------------
library(agridat)
turnip <- mcconway.turnip
turnip$density <- as.factor(turnip$density)

## ------------------------------------------------------------------------
ggplot(turnip, aes(x = density, y = yield)) +
  geom_point() +
  facet_grid(~gen)

## ------------------------------------------------------------------------
turnip[c(1:4,61:64),]

## ------------------------------------------------------------------------
with(turnip, tapply(yield, list(gen, density), mean))

## ------------------------------------------------------------------------
qplot(density, yield, data = turnip, 
      facets = ~block, shape = gen, size = I(2))

## ------------------------------------------------------------------------
ggplot(turnip, aes(x = density, linetype = gen, group = gen, y = yield)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

## ------------------------------------------------------------------------
turnip.full <- lm(yield ~ gen * density, data = turnip)
coef(summary(turnip.full))

## ------------------------------------------------------------------------
with(turnip, tapply(yield, list(gen, density), mean))

## ------------------------------------------------------------------------
summary(turnip.full)
## #F-statistic: 4.338 on 7 and 56 DF,  p-value: 0.0006739

## ------------------------------------------------------------------------
anova(turnip.full)
library(car)
Anova(turnip.full, type = 2)

## ------------------------------------------------------------------------
turnip.main <- lm(yield ~ gen + density, data = turnip)

## ------------------------------------------------------------------------
library(car)
qqPlot(resid(turnip.full), xlab = "Normal quantiles", ylab = "Residuals")

## ------------------------------------------------------------------------
shapiro.test(resid(turnip.full))

## ------------------------------------------------------------------------
turnip.sqrt <- lm(sqrt(yield) ~ gen * density, data = turnip)
qqPlot(resid(turnip.sqrt))
shapiro.test(resid(turnip.sqrt))

## ------------------------------------------------------------------------
plot(fitted(turnip.sqrt), resid(turnip.sqrt), xlab = "Fitted values", 
     ylab = "Residuals", las = 1); abline(h = 0)

## ------------------------------------------------------------------------
bartlett.test(sqrt(yield) ~ interaction(gen, density), data = turnip)
leveneTest(sqrt(yield) ~ interaction(gen, density), data = turnip)

## ------------------------------------------------------------------------
library(car)
Anova(turnip.sqrt, white.adjust = "hc3")

## ------------------------------------------------------------------------
coef(turnip.sqrt)

## ------------------------------------------------------------------------
dat <- expand.grid(gen = levels(turnip$gen), 
                   density = levels(turnip$density))
pred <- predict(object = turnip.sqrt, newdata = dat,
                interval = "confidence")
(ci <- cbind(dat, pred^2))

## ------------------------------------------------------------------------
emmeans(turnip.sqrt, pairwise ~ gen)

## ------------------------------------------------------------------------
turnip.sqrt.gen <- emmeans(turnip.sqrt, pairwise ~ gen | density)
turnip.sqrt.gen$contrasts

## ------------------------------------------------------------------------
emmeans(turnip.sqrt, pairwise ~ gen + density)

## ------------------------------------------------------------------------
anova(turnip.sqrt)
Anova(turnip.sqrt, type = 2)

## ------------------------------------------------------------------------
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


