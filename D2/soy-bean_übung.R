#13.03.2021
#D2 - Soy bean data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

library("tidyverse")
library("car") #for anova
library("nlme")
soy = read.delim('soybean.csv', header=TRUE, sep=';', dec='.')
str(soy)
#a)
soy$gen = factor(soy$gen)
soy$loc = factor(soy$loc)
soy$block = factor(soy$block)

str(soy)
with(soy, tapply(yield, loc, mean))
with(soy, tapply(yield, gen, mean))
summary(soy)
#komment: grundmenge ist yield und wir wollen wissen welcher Genotype den höchshten Durchschnitt produziert.
#solution: the genotype "N72-3148" produces the highest average yield of 1566.000 grams


#b) evidence= nachweis --> overall F- test
soy.lm=lm(yield~gen*loc, data=soy)
library(car)
Anova(soy.lm, type=2)
summary(soy.lm)#for the overall f-test, but not asked in this question

#c)
#solution: genotype and location variables have a significant values (4.639e-05 and < 2.2e-16, respectively). We do not find a significant interaction effect of genotype and location (p-value = 0.22). 
#the overall f-test shows a significant value (3.228e-12). unterschiedliche Varianzen.  

#d)
library(agricolae)
HSD.test(soy.lm, "loc", group = TRUE, console = TRUE)
library(ggplot2)
ggplot(soy, aes(y=yield, x=gen, col=loc)) +
  geom_point()   #+facet_grid(block~loc)
#comment: overall in the plot you see a systematically structure of first clinton,plymouth, clayton.  

#e)
soybean.add  =lm(yield~gen+loc, data=soy)
library(agricolae)
soybean.add <- lm(yield ~ gen + loc, data = soy)
HSD.test(soybean.add, "gen", group = TRUE, console = TRUE)
#solution: The only significant difference is between (N72-3058) and (N72-3148) .
#f) 
pred_f=data.frame(gen="Centennial", loc="Plymouth")
predict(soybean.add, pred_f)

pred.df <- data.frame(gen = "Centennial", loc = "Plymouth")
predict(soybean.add, pred.df)
#solution: 1381.861 average of yield in centennial is: 1394.778. the ration between the fitted and real value is in the confidence intervall of 0.05.
#overfitting by using soy.lm model
with(soy, tapply(yield, gen, mean))


#g)
summary(soybean.add)
yield.var=var(soy$yield)
yield.var
r.squared = var(fitted(soybean.add))/yield.var
r.squared

#h) check assumptions
#observations are mutually independent see str(soy)
#normalitiy distribution of residuls 
qqPlot(resid(soybean.add))
#Testing for normality - Shapiro Wilk
shapiro.test(resid(soybean.add)) #p-value= 0.233, the null hypothesis of normality is not rejected-> For small samples a nonsignificant Shapiro Wilk test does not imply no problems with normality are present.
#hier grösserer P-value um Normalität aufzuzeigen. 

#Homoskedasticity
plot(soybean.add, which=1)
abline(h=0)

###with block-effect

soy.lm2=lm(yield~gen*loc, data=soy, random=~1|block)
Anova(soy.lm2, type = 2)
soy.lm3=lm(yield~gen+loc, data=soy, random=~1|block)
Anova(soy.lm3, type = 2)




       