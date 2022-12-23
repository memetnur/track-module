#Ex.1
str(morley)
#3 variables, speed is count data
#one-way ANOVA
morley.lm <- lm(Speed ~ Expt, data = morley)
anova(morley.lm)
#p_value 0.00048 < alpha 0.05 therefore N0 gets rejected, five experiments give different speed means

ggplot(morley, aes(x = Expt, y = Speed, color = Run)) +geom_point() + geom_jitter(width = 0.1)
boxplot(Speed ~ Expt, data = morley)
#visualizing with boxplot or strip plot shows the different speed measurements

with(morley, tapply(Speed, list(Expt), mean))
#Experiment 1 has a significantly higher mean, Experiment 4 the slowest avg. speed of all five

library(car)
qqPlot(resid(morley.lm), las = 1)
##QQ-Plot doesn't look suspicious, only 2 outliers, the rest of the residuals fall between the confidence intervals
shapiro.test(resid(morley.lm))$p.value
#p > 0.05 --> N0 is accepted, therefore residuals should come from a normal distribution

morley$Expt = as.factor(morley$Expt)
morley$Speed = as.numeric(morley$Speed)
bartlett.test(Speed ~ Expt, data = morley)
#p-value = 0.02
leveneTest(Speed ~ Expt, data = morley)
#p-value = 0.16
#The Bartlett Test rejects N0, whereas the Levene's Test accepts N0. Maybe because the Bartlett test reacts very sensitive to outliers.

kruskal.test(Speed ~ Expt, data = morley)
#p-value = 0.004656
library(asbio)
with(morley, BDM.test(Speed, Expt))
#p-value = 0.0039
#both non-parametric tests clearly reject N0, therefore the experiments tend to give different results for the distribution of the speed of light