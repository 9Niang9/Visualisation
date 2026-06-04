#################### REGRESSION LINEAIRE SIMPLE ###########################

install.packages(c("car","lmtest","ggplot2","GGally",
                   "performance","olsrr"),dep=T)
install.packages("lmtest",dependencies =TRUE)
install.packages("GGally",dependencies =TRUE)
install.packages("performance",dependencies =TRUE)
install.packages("olsrr",dependencies =TRUE)


# 1) Charger les packages

library(car)
library(lmtest)
library(ggplot2)
library(GGally)
library(performance)
library(olsrr)

# 2) Importer les donnéeS

base = read.table(file.choose(), header=TRUE, sep=";")
str(base)   # structure des données
attach(base)  # permet d'accéder directement aux variables suivie de base$LOC ou LOC apres attache
dim(base)	# dimension de la base
head(base)	# affiche l'entête des données
tail(base) #afficher les derniers éléments de la base
edit(base)	# affiche entièrement la base
print(base) # affiche entièrement la base
fix(base)   # affiche entièrement la base

loc=c(LOC,-100) #Ajout de valeur dans LOC
loc
boxplot(loc)
loc[27]=mean(LOC[1:26]) #Affiche moyenne 27éme position
loc[27]
loc=LOC[-c(0,0)] #Suppression d'élément 

summary(LOC)	# statistiques descriptives de la variable
boxplot(LOC)	# boite à moustaches de la variable (%des loyers)


# 2) Evaluation de la linéarite

plot(SURF,LOC) #Graphe sans explication
plot(SURF,LOC,col=20,lwd=7,main='nuage de points', 
     xlab='Supeficie',ylab='Location')##Variable SURF expliqué et LOC variable à expliqué

scatterplot(LOC~SURF) #cas d'une base (UNIQUE) sans spécifié data=nombase de la base 
scatterplot(LOC~SURF, data=base) ##Inversion des variable (LOC variable expliqué par la Superficie)

# 3) Ajustement du modèle

model = lm(LOC~SURF, data=base)
model
summary(model) #Affiches Estimateur et std.Error(ecart type),tvalue=Estimate/std.Error
residus=model$residuals
sum(residus)

##***********le variable SURF à un impact significative sur le prix du loyer 
##******car la p-value est strictment inférieur au seuil de 5% (3,44e-10).
##******Si la superficie augmente d'une unité le prix du loyer va augmenter de 28.239UM 
##******La superficie permet d'expliquer 81,21% de la variation total du prix du loyer .
##******Le reste (18,79%) est expliquer par la partie résiduel (l'erreur)
##******Le model est globalement significatif 
##******car la p-value associer au test de Ficher est inférieur au seuil de 5%

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

model.bis = lm(LOC~SURF, data=base[-13,]) #Supprimer ligne 13 de la base
model.bis
summary(model.bis)
AIC(model.bis)#AIC faible par rapport au model initial
compareCoefs(model,model.bis) #Compare coefficient voir les SE si c faible le modéle est bon
##******il est préferable de ne pas enlevé les 3individus car on passe de 81.21 à
##******76.58% ou sa diminue la valeur du FICHER(F-statistic)

# 5) Droite de regression (droite des MCO)

plot(SURF,LOC,col=4,lwd=2,
     main='Droite des moindres carres ordinaires',xlab='surface',
     ylab='Coût du loyer')#nuage de point

abline(model,col=10,lwd=3)#droite de régression 

segments(SURF,fitted(model),SURF,LOC)

residus = model$residuals	# valeurs residuelles
residus
summary(residus)
mean(residus)#Proche de 0:1.475316e-15
sum(residus)

#6Adequation du model
check_model(model) 
qqnorm(residus)
qqline(residus)    #Résidus Normale
par(mfrow=c(2,2))
plot(model,las=1)

# graphes individuels 
par(mfrow=c(2,2))
plot(model,las=1)

plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)


residus = model$residuals	# valeurs residuelles
residus
summary(residus)
# Verification par test d'hypotheses

# test de nullite de la moyenne(C1)
# H0: la moyenne des residus est nulle ******
# H1: la moyenne des residus est non nulle

t.test(residus,mu=0) #mu(moyenne) p-value=1>alpha et IntervalConf y'a 0

# linearite de la relation, normalite(C4) et 
# homoscedasticite (C2)
# H0: les residus suivent une loi normale de meme 
# variance

hmctest(model) #Harrison-McCabe test p-value>alpha=5%

# test d'homogeneite uniquement C2(H0: Homoscedasticite[homogene],H1:Heteroscedasticite[heterogene]) 

ncvTest(model) #Non-constant Variance Score Test p-value>alpha=5%


# Non autocorrelation des residus C3
# H0: les residus sont non autocorreles (rejet H0)
# H1: les residus sont autocorreles ************


dwtest(model) # Durbin-Watson test p-value>alpha=5%


# test de normalite C4
# H0: les residus suivent une loi normale *******
# H1: les residus suivent pas une loi normale


shapiro.test(residus) #	Shapiro-Wilk normality test p-value>alpha=5%


par(mfrow=c(1,2))

plot(density(residus),lwd=3,col=2)
abline(v=0,lwd=3,col=12)

hist(residus,col=3,nclass = 10,probability = T)
lines(density(residus),lwd=3,col=2)
abline(v=0,lwd=3,col=12)

# 7) Previsions

nd = data.frame(SURF=c(20,42,71,39,60))
nd
pred=predict(model,interval="confidence",nd) #commande predict pour des prediction
pred


# 8) Representation finale de la regression avec 
# l'IC et l'IC de prevision

ggplot(base, aes(y=LOC, x=SURF))+
  geom_point()+
  geom_smooth(colour="blue", method="lm", fill="red") +
  ylab("LOC")+
  xlab("SURF") +
  theme_classic()+
  annotate("text", x = 45, y = 1430, label = "LOC = -261.88 + 28.24*SURF") 


IC_pred = predict(model, interval="prediction")
IC_pred
head(IC_pred)
base2 = cbind(base, IC_pred)
head(base2)

ggplot(base2, aes(y=LOC, x=SURF))+
  geom_point()+
  geom_smooth(colour="blue", method="lm", fill="red") +
  ylab("LOC")+
  xlab("SURF") +
  theme_classic()+
  annotate("text", x = 45, y = 1430, label = "LOC = -261.88 + 28.24*SURF") 








