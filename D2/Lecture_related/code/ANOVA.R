## CHAPTER 2 --------------------------------------------------------------

with(InsectSprays, tapply(count, spray, length)) #12 obs. per trt
with(InsectSprays, tapply(count, spray, summary)) #compare distr.
with(InsectSprays, tapply(count, spray, sd)) #standard deviations

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(InsectSprays, aes(spray, count)) + 
    geom_point(shape = 1, position = position_jitter(width = 0.2, 
                                                     height = 0))

## ------------------------------------------------------------------------
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1)) #try
plot(count ~ spray, data = InsectSprays, las = 1)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
ins.lm <- lm(count ~ spray, data = InsectSprays)
anova(ins.lm)

## ------------------------------------------------------------------------
ins.aov <- aov(count ~ spray, data = InsectSprays)
summary(ins.aov)

## ------------------------------------------------------------------------
summary(ins.lm)$sigma

## ------------------------------------------------------------------------
summary(ins.lm)$r.squared

## ------------------------------------------------------------------------
ins.lm.noint <- lm(count ~ spray - 1 , data = InsectSprays) 
summary(ins.lm.noint)$coefficients

## ------------------------------------------------------------------------
with(InsectSprays, pairwise.t.test(count, spray, "holm"))
with(InsectSprays, pairwise.wilcox.test(count, spray, "holm"))

## ------------------------------------------------------------------------
TukeyHSD(ins.aov, "spray")
library(agricolae)
HSD.test(ins.lm, "spray", group = TRUE, console = TRUE)

## CHAPTER 3 --------------------------------------------------------------
contrasts(InsectSprays$spray)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
K <- matrix(c(0, 1, 0, 0, 0, -1), nrow = 1)
summary(glht(ins.lm, linfct = K))

## ------------------------------------------------------------------------
summary(glht(ins.lm, mcp(spray = "GrandMean")))

## CHAPTER 4 --------------------------------------------------------------
library(car)
qqPlot(resid(ins.lm))

## ------------------------------------------------------------------------
qqPlot(rnorm(72, mean = mean(resid(ins.lm)), sd = sd(resid(ins.lm))))

## ------------------------------------------------------------------------
shapiro.test(resid(ins.lm))

## ------------------------------------------------------------------------
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.6,3.6,1.1,1.1)) #try
plot(fitted(ins.lm), resid(ins.lm), las = 1,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)

## ------------------------------------------------------------------------
bartlett.test(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays)

## ------------------------------------------------------------------------
oneway.test(count ~ spray, data = InsectSprays)

## ------------------------------------------------------------------------
kruskal.test(count ~ spray, data = InsectSprays)

## ------------------------------------------------------------------------
library(NSM3)
## with(InsectSprays, pSDCFlig(x = count, g = as.numeric(spray), method=NA))

## ------------------------------------------------------------------------
library(asbio)
with(InsectSprays, BDM.test(count, spray))

## ------------------------------------------------------------------------
with(InsectSprays, trim.test(count, spray, tr = 0.2))

## ------------------------------------------------------------------------
library(lmPerm)
anova(lmp(count ~ spray, data = InsectSprays))


## CHAPTER 6 --------------------------------------------------------------
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
library(ggplot2)
    ggplot(immer, aes(x = Var, y = Y1)) +
    geom_point() +
    facet_grid(~Loc)

## ------------------------------------------------------------------------
library(MASS)
anova(lm.immer <- lm(Y1 ~ Var, immer))
sigma(lm.immer)

## ------------------------------------------------------------------------
anova(lm(Y1 ~ Loc + Var, immer))

## ------------------------------------------------------------------------
immer.lme <- lme(Y1 ~ Var, random = ~ 1 | Loc, data = immer)
immer.lme
anova(immer.lme, type = "marginal")

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

## CHAPTER 7 --------------------------------------------------------------
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
#F-statistic: 4.338 on 7 and 56 DF,  p-value: 0.0006739

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
library(car)
Anova(turnip.sqrt, type = 2)

## ------------------------------------------------------------------------
turnip.lme <- lme(sqrt(yield) ~ gen * density, random = ~ 1 | block,
                  data = turnip)

## ------------------------------------------------------------------------
plot(turnip.lme)                  
qqPlot(resid(turnip.lme))
shapiro.test(resid(turnip.lme))    

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


## CHAPTER 8 --------------------------------------------------------------
ins.aov <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(ins.aov)

## ------------------------------------------------------------------------
InsectSprays$block <- factor(rep(rep(1:6, each=2), times = 6))
summary(aov(sqrt(count) ~ spray + Error(block), data = InsectSprays))

## ------------------------------------------------------------------------
library(nlme)
ins.lme <- lme(sqrt(count) ~ spray, random = ~ 1 | block,
               data = InsectSprays)
anova(ins.lme, type = "marginal")

## CHAPTER 9 --------------------------------------------------------------
library(ibd)
set.seed(1) # to always get the same design
bibd(7, 7, 3, 3, 1)$design

## ------------------------------------------------------------------------
detergent <- read.table("detergent.csv", header = TRUE, sep = ",")
head(detergent)
addmargins(with(detergent, table(treatment, block)))
ggplot(detergent, aes(x = treatment, y = plates)) +
    geom_jitter(shape = 4, width = 0.1)

## ------------------------------------------------------------------------
detergent.lm <- lm(plates ~ block + treatment, data = detergent)
anova(detergent.lm)
library(car)
Anova(detergent.lm, type = 2) ## If you want Type II sums of squares

## ------------------------------------------------------------------------
theta.hat <- c(0, coef(detergent.lm)[2:12]) ## 0 for block 1 (reference)
theta.avg <- mean(theta.hat)
detergent$block.num <- as.numeric(detergent$block) ## for indexing
detergent$adj <- theta.hat[detergent$block.num] - theta.avg ## note use of []
detergent$plates.adj <- detergent$plates - detergent$adj
head(detergent)

## ------------------------------------------------------------------------
detergent.lme <- lme(plates ~ treatment, random = ~1 | block, 
                     data = detergent)
anova(detergent.lme)

## CHAPTER 10 --------------------------------------------------------------
ggplot(Oats, aes(x = nitro, y = yield, col = Variety)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Block)
ggplot(Oats, aes(x = nitro, y = yield, group = Block)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Variety)

## ------------------------------------------------------------------------
library(nlme)
assignInNamespace("print.correlation", function(x, title) return(), ns="nlme")
Oats$nitroF <- factor(Oats$nitro)
Oats.lme <- lme(yield ~ Variety * nitroF, data = Oats,
                random = ~ 1 | Block / Variety)
summary(Oats.lme)
ranef(Oats.lme) ## look at results!

## ------------------------------------------------------------------------
anova(Oats.lme, type = "marginal")

## ------------------------------------------------------------------------
Oats.lme <- lme(yield ~ Variety + nitroF, data = Oats,
                random = ~ 1 | Block / Variety)
anova(Oats.lme, type = "marginal")

## ------------------------------------------------------------------------
Oats.lme <- lme(yield ~ nitroF, data = Oats,
                random = ~ 1 | Block / Variety)

## ------------------------------------------------------------------------
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

## CHAPTER 11 --------------------------------------------------------------
library(ggplot2)
gasel <- read.table("gasel.csv", sep = ",", header = TRUE)
ggplot(gasel, aes(x = block, y = shannon, col = location)) +
    geom_point()

## ------------------------------------------------------------------------
friedman.test(shannon ~ location | block, data = gasel)

## ------------------------------------------------------------------------
library(nlme)
gasel.blk <- lme(shannon ~ location, random = ~ 1 | block,
    data = gasel)
coef(summary(gasel.blk))

## ------------------------------------------------------------------------
library(nlme)
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


## CHAPTER 13 --------------------------------------------------------------
library(nlme)
compare.sim <- function(n.block, n.treatments, means, sds, sd.block, 
                        runs, sig.level = 0.05) {
    df <- cbind(block = paste0("B", rep(1:n.block, each = n.treatments)),
                treatment = paste0("T", rep(1:n.treatments)))
    df <- data.frame(df)
    p.val <- data.frame(matrix(NA, nrow = runs, ncol = 2))
    names(p.val) <- c("Friedman.p.value", "Mixed.model.pvalue")
    sig <- function(x) length(x[x < sig.level]) / length(x)
    for (i in 1: dim(p.val)[1]) {
        ## Simulate data
        df$y <- rnorm(n = dim(df)[1],
                      mean = rep(rnorm(n = n.block, mean = 0, sd = sd.block), 
                                 each = n.treatments) +
                          rep(rep(means), times = n.block),
                      sd = rep(rep(sds), times = n.block))
        ## Calculate p values
        p.val[i, ] <- c(friedman.test(y ~ treatment | block, 
                                          data = df)$p.value,
                        anova(lme(y ~ treatment, random = ~ 1 | block, 
                                      data = df))$`p-value`[2])
    }
    return(apply(p.val, MARGIN = 2, sig))
}

## ------------------------------------------------------------------------
set.seed(17)
compare.sim(n.block = 4, n.treatments = 3, means = c(1, 2.7, 3.2), 
            sds = c(1, 1, 1), sd.block = 1.5, runs = 100)

## ------------------------------------------------------------------------
for (bl in 4:10) {
    cat(paste("blocks: ", bl, sep = ""))
    print(compare.sim(n.block = bl, n.treatments = 3, means = c(1, 2.7, 3.2), 
            sds = c(1, 1, 1), sd.block = 1.5, runs = 100))
}

## ------------------------------------------------------------------------
## for (bl in 5:7) {
##     cat(paste("blocks: ", bl, sep = ""))
##     print(compare.sim(n.block = bl, n.treatments = 3, means = c(1, 2.7, 3.2),
##             sds = c(1, 1, 1), sd.block = 1.5, runs = 5000))
## }
