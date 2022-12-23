##D2: local teaching session 2:

mydata=data("PlantGrowth")
print(mydata)
nrow(mydata)
#1. Use suitable descriptive statistics to describe the data set. 
#there is one control group and there is treatment 1 and treatment 2 group
with(PlantGrowth, tapply(weight, group, length))
with(PlantGrowth, tapply(weight, group, summary))
with(PlantGrowth, tapply(weight, group, sd))


##2. Visualize the data suitably. What do you observe?
# jittered strip plot
install.packages("ggplot2")
library("ggplot2")

ggplot(PlantGrowth, aes(group, weight)) +
  geom_point(shape=1, position=position_jitter(width=0.2, height=0))
#box-and-whisker plot
plot(weight ~ group, data = PlantGrowth, las = 1)

library(sciplot)
bargraph.CI(group, weight, col = (gray(0.88)), data = PlantGrowth,
            xlab = "group", ylab = "weight", ylim = c(0,6))
lineplot.CI(group, weight, type = "p", data = PlantGrowth,
            xlab = "group", ylab = "weight", ylim = c(4,6))

##3. Perform the parametric F test and interpret the result.
#F test conlcusion: F value is = 0.01591 and smaller than the significane level = 0.05, 
#we can reject the null hypothesis and conclude that not all the treatments have the same theoretical means.

ins.lm<-lm(weight~group, data=PlantGrowth)
anova(ins.lm)

ins.aov <- aov(weight~group, data=PlantGrowth)
summary(ins.aov)

##4. Compare the three groups using Tukey's HSD (do not use the multcomp package yet).
TukeyHSD(ins.aov, "group")
library(agricolae)
HSD.test(ins.lm, "group", group=TRUE, console=TRUE)

#hypothesis is true, you cant reject because its biger than 0.5
# ctrl1 and trt2 are not significantly different. ctrl1 and trt1 are not significantly different. 
#But trt1 and trt2 are significantly different


# 5. Now, use the multcomp package to perform Tukey's HSD. Compare with the previous problem.
library(multcomp)
summary(glht(ins.lm, mcp(group = "Tukey")))

#significantly different in trt2 - trt1 == 0   0.8650     0.2788   3.103    0.012 *

#Use the multcomp package to compare each of the two treatments to the control
#only (but not to each other). Compare the results with the previous problem.
summary(glht(ins.lm, mcp(group = "Dunnett")))

# 7. Judge residual normality visually and with a statistical test. What is your conclusion?
library(car)
qqPlot(resid(ins.lm))

#question: what are numbers 17 and 15? outliers?
#i think there are normal distributed. 2 points shows also in 4. after doing saphior test you cant reject the test. 
shapiro.test(resid(ins.lm))

8.
library(asbio)
with(PlantGrowth, BDM.test(weight, group))
install.packages("lmPerm")
library(lmPerm)
anova(lmp(group ~ weight, data = PlantGrowth))

kruskal.test(weight ~ group, data = PlantGrowth)
plot(fitted(ins.lm), resid(ins.lm), las = 1, xlab = "Fitted values", ylab = "Residuals") +abline(h = 0)

        