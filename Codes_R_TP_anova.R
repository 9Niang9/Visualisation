
####### ANOVA A UN FACTEUR #########

base=read.table(file.choose(), header=T, sep=";")
attach(base)
str(base)
fix(base)
edit(base)
base

is.factor(niveau)
is.factor(ville)
niveau1=as.factor(niveau)
is.factor(niveau1)

plot(ventes~niveau1)

# test de comparaison de 2 variances 

var.test(x,y)

# test de Bartlett

bartlett.test(ventes,niveau1)

bartlett.test(ventes~niveau1,data=base)

# Modele ANOVA

m0=lm(ventes~niveau1,data=base)
anova(m0)

model=aov(ventes~niveau1, data=base)
model
summary(model)

### Verification des hypotheses sur les residus

par(mfrow=c(2,2))
plot(model,las=1)

residus=model$residuals

t.test(residus,mu=0)     # H0: moyenne nulle des residus

library(lmtest)
dwtest(model)   # H0: homoscedasticite

hmctest(model)    # H0: non autocorrelation des residus 

shapiro.test(residus)

par(mfrow=c(1,2))
plot(density(residus))
hist(residus,nclass=10,col=3)


####### ANOVA A DEUX FACTEURS #########

par(mfrow=c(1,2))
plot(ventes~niveau1)
plot(ventes~ville)

barplot(tapply(ventes,list(niveau1,ville),mean),beside=T,legend.text=T)

tapply(ventes,list(niveau1,ville),mean)        

# Modele ANOVA a 2 facteurs

model2=aov(ventes~niveau1+ville, data=base)
model2
summary(model2)

# Modele avec interaction

model3=aov(ventes~niveau1*ville, data=base)
summary(model3)

model4=aov(ventes~niveau1+ville+niveau1:ville, data=base)
summary(model4)

####### ANCOVA #########

plot(budget,ventes,pch=as.numeric(niveau1))
legend("topleft",c("niv1","niv2","niv3"),pch=c(1,2,3))

# Sous modeles lineaires

smodel1=lm(ventes[niveau1=="1"]~budget[niveau1=="1"])
summary(smodel1)
smodel2=lm(ventes[niveau1=="2"]~budget[niveau1=="2"])
summary(smodel2)
smodel3=lm(ventes[niveau1=="3"]~budget[niveau1=="3"])
summary(smodel3)

plot(budget,ventes,pch=as.numeric(niveau1))
abline(smodel1)
abline(smodel2)
abline(smodel3)

model5=aov(ventes~niveau1*budget, data=base)
summary(model5)

model6=lm(ventes~niveau1*budget, data=base)
summary(model6)
anova(model6)


