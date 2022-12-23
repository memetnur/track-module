## ------------------------------------------------------------------------
soybean <- read.table("soybean.csv", header = TRUE, sep = ";")
str(soybean)

library(ggplot2)
ggplot(soybean, aes(y=yield, x=gen)) +
    geom_point() +
    facet_grid(block~loc)

## ------------------------------------------------------------------------
with(soybean, table(gen, loc, block))

## ------------------------------------------------------------------------
sort(with(soybean, tapply(yield, gen, mean)))

## ------------------------------------------------------------------------
soybean.lm <- lm(yield ~ gen * loc, data = soybean)
library(car)
Anova(soybean.lm, type = 2)

## ------------------------------------------------------------------------
library(agricolae)
soybean.add <- lm(yield ~ gen + loc, data = soybean)
HSD.test(soybean.add, "gen", group = TRUE, console = TRUE)

## ------------------------------------------------------------------------
coef(soybean.add)

## ------------------------------------------------------------------------
pred.df <- data.frame(gen = "Centennial", loc = "Plymouth")
predict(soybean.add, pred.df)

## ------------------------------------------------------------------------
var.yield <- var(soybean$yield)
r.squared <- var(fitted(soybean.add)) / var.yield
c(var.yield, r.squared)

## ------------------------------------------------------------------------
summary(soybean.add)$r.squared

## ------------------------------------------------------------------------
shapiro.test(resid(soybean.add))
bartlett.test(yield ~ interaction(gen, loc), data = soybean)
leveneTest(yield ~ interaction(gen, loc), data = soybean)
qqPlot(soybean.add)

## ------------------------------------------------------------------------
library(nlme)
soybean.lme <- lme(yield ~ gen * loc, data = soybean, random = ~ 1 | block)
anova(soybean.lme, type = "marginal")
soybean.lme

## ------------------------------------------------------------------------
soybean.add.lme <- lme(yield ~ gen + loc, data = soybean, random = ~ 1 | block)
library(emmeans)
emmeans(soybean.add.lme, pairwise ~ gen)

## ------------------------------------------------------------------------
CLD(emmeans(soybean.add.lme, "gen"))

## ------------------------------------------------------------------------
(fe <- fixef(soybean.add.lme))
as.numeric(fe["(Intercept)"] + fe["locPlymouth"])


## ------------------------------------------------------------------------
pred.df <- data.frame(gen = "Centennial", loc = "Plymouth")
predict(soybean.add.lme, pred.df, level = 0)

## ------------------------------------------------------------------------
library(MuMIn)
r.squaredGLMM(soybean.add.lme)

## ------------------------------------------------------------------------
plot(soybean.add.lme)
library(car)
shapiro.test(resid(soybean.add.lme))
qqPlot(resid(soybean.add.lme))
