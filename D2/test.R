install.packages(c("agricolae", "agridat", "asbio", "boot", "car", "coin", "emmeans", "exactRankTests", "ggplot2", "lattice", "MASS", "multcomp", "nlme", "lme4", "NSM3", "sciplot"))

data("InsectSprays")

## ------------------------------------------------------------------------
with(InsectSprays, tapply(count, spray, length)) #12 obs. per trt
with(InsectSprays, tapply(count, spray, summary)) #compare distr.
with(InsectSprays, tapply(count, spray, sd)) #standard deviations

## ------------------------------------------------------------------------
library(ggplot2)
g=ggplot(InsectSprays, aes(spray, count)) + 
  geom_point(shape = 1, position = position_jitter(width = 0.2, 
                                                   height = 0))
g
## ------------------------------------------------------------------------
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1)) #try
plot(count ~ spray, data = InsectSprays, las = 1)

## ------------------------------------------------------------------------
install.packages("sciplot")
library(sciplot)
bargraph.CI(spray, count, col = (gray(0.88)), data = InsectSprays,
            xlab = "spray", ylab = "count", ylim = c(0,20))
lineplot.CI(spray, count, type = "p", data = InsectSprays,
            xlab = "spray", ylab = "count", ylim = c(0,20))

## ------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6)) ## design
s.o <- with(InsectSprays, reorder(spray, count, mean, order = TRUE))
InsectSprays$spray.o <- factor(InsectSprays$spray, levels = levels(s.o))
b.o <- with(InsectSprays, reorder(block, count, mean, order = TRUE))
InsectSprays$block.o <- factor(InsectSprays$block, levels = levels(b.o))
ggplot(InsectSprays, aes(spray.o, count)) + 
  geom_point() +
  facet_grid( ~ block.o) +
  xlab("spray")

## Parametric models: one-way ANOVA
## Distributional assumptions
## ------------------------------------------------------------------------
ins.lm <- lm(count ~ spray, data = InsectSprays)
anova(ins.lm)

## ------------------------------------------------------------------------
ins.aov <- aov(count ~ spray, data = InsectSprays)
summary(ins.aov)

summary(ins.lm)
summary(ins.lm)$sigma

with(InsectSprays, pairwise.wilcox.test(count, spray, "holm"))

