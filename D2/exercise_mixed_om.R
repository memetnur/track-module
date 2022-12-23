library("nlme")
data("Oats")

plot(Oats)
data
#for now the model consists just fixed effects
model1=lm(yield~Variety*nitro,data=Oats)
summary(model1)
model2=lme(yield~Variety*nitro,data=Oats, random=~1 | Block/Variety)
summary(model2)


#linear mixed effects model fit by REML = restricted maximum likelihood
#comparing model 1 and model 2 the estimate parametsr are identical.
#but it effects the std. error. by including random effects. 

coef(model1)
coef(model2)
#lets look at the random effects
plot(ranef(model2))
#random effects are large in this case and are needed, unexplained variation in the model
#now plot the residuals of model 2 to inspect
plot(model2)
#there are centred in zero, symmetrical 