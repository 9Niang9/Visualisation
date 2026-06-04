
#################### REGRESSION LINEAIRE SIMPLE ###########################

# 1) Charger les packages

library(car)
library(lmtest)
library(ggplot2)
library(GGally)
library(performance)
library(olsrr)

# 2) Importer les donnée

base = read.table(file.choose(), header=TRUE, sep=";")
base
str(base)   # structure des données
attach(base)  # permet d'accéder directement aux variables
dim(base)	# dimension de la base
head(base)	# affiche l'entête des données
edit(base)	# affiche entièrement la base

summary(cout)	# statistiques descriptives de la variable
boxplot(cout)	# boite à moustaches de la variable


# 2) Evaluation de la linéarite

plot(age,cout,col=20,lwd=7,main='nuage de points', 
     xlab='Age des véhicules',ylab='Coût')

scatterplot(cout~age, data=base) 

# 3) Ajustement du modèle

model = lm(cout~age, data=base)
summary(model)

confint(model)	# Intervalles de Confiance des parametres
AIC(model)
BIC(model)

anova(model)	# tableau d'analyse de la variance

# 4) Etudes des valeurs atypiques

rstud=rstudent(model)    # residus studentisés
rstud

influenceIndexPlot(model)
ols_plot_cooksd_bar(model)
ols_plot_dfbetas(model)
ols_plot_resid_lev(model)

# comparaison des coefficients

model.bis = lm(cout~age, data=base[-13,])
summary(model.bis)
compareCoefs(model,model.bis) 

# 5) Droite de regression (droite des MCO)

plot(age,cout,col=4,lwd=2,
     main='Droite des moindres carres ordinaires',xlab='Age des véhicules',
     ylab='Coût de maintenance')

abline(model,col=10,lwd=3) 

segments(age,fitted(model),age,cout)

# 6) Adequation du modèle

check_model(model)

par(mfrow=c(2,2))
plot(model,las=1)

# graphes individuels 

plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)


residus = model$residuals	# valeurs residuelles
residus
summary(residus)

# Verification par test d'hypotheses

# test de nullite de la moyenne
# H0: la moyenne des residus est nulle
# H1: la moyenne des residus est non nulle

t.test(residus,mu=0)

# linearite de la relation, normalite et 
# homoscedasticite
# H0: les residus suivent une loi normale de meme 
# variance

hmctest(model) #Harrison-McCabe test p-value>alpha

# test d'homogeneite uniquement (H0: Homoscedasticite)

ncvTest(model)

# Non autocorrelation des residus
# Durbin-Watson test 
# H0: les residus sont non autocorreles

dwtest(model)

# test de normalite
# H0: les residus suivent une loi normale

shapiro.test(residus)

par(mfrow=c(1,2))

plot(density(residus),lwd=3,col=2)

# 7) Previsions

nd = data.frame(age=c(20,42,71,39,60))
nd
cout_pred=predict(model,interval="confidence",nd)
cout_pred

# 8) Representation finale de la regression avec 
# l'IC et l'IC de prevision

ggplot(base, aes(y=cout, x=age))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Co?t")+
  xlab("Age") +
  theme_classic()+
  annotate("text", x = 16, y = 100, label = "cout = 31.7 + 1.279*age") 


IC_pred = predict(model, interval="prediction")
head(IC_pred)
base2 = cbind(base, IC_pred)
head(base2)

ggplot(base2, aes(y=cout, x=age))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  geom_line(aes(y=lwr), color = "blue", linetype = "dashed", lwd=2)+
  geom_line(aes(y=upr), color = "blue", linetype = "dashed", lwd=2)+    
  ylab("Coût")+
  xlab("Age") +
  theme_classic()+
  annotate("text", x = 16, y = 100, label = "cout = 31.7 + 1.279*age") 

