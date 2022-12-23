#name: Memeti Nurdzane
#Date: 27.11.2020
#script d2: Design and analysis of experiments
#anova2


data("InsectSprays")

## ------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6)) ## design
head(InsectSprays)

## ------------------------------------------------------------------------
library(nlme)
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)

## ------------------------------------------------------------------------
summary(ins.lme)

## ------------------------------------------------------------------------
fixef(ins.lme)

## ------------------------------------------------------------------------
ranef(ins.lme)

## ------------------------------------------------------------------------
ranef(ins.lme)$`(Intercept)`

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

## ------------------------------------------------------------------------
qqPlot(resid(ins.lme))
shapiro.test(resid(ins.lme))

## ------------------------------------------------------------------------
shapiro.test(ranef(ins.lme)$`(Intercept)`)

## ------------------------------------------------------------------------
intervals(ins.lme)

## ------------------------------------------------------------------------
anova(ins.lme, type = "marginal")

## ------------------------------------------------------------------------
library(multcomp)
summary(glht(ins.lme, mcp(spray = "GrandMean")))

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
(V <- VarCorr(ins.lme))
V <- as.numeric(V)
V[1] / (V[1] +V[2])

## ------------------------------------------------------------------------
library(MuMIn)
r.squaredGLMM(ins.lme)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

