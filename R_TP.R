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
loc=LOC[-c(0,0)] #Suppression d'élément s

summary(LOC)	# statistiques descriptives de la variable
boxplot(LOC)	# boite à moustaches de la variable (%des loyers)


# 2) Evaluation de la linéarite

plot(SURF,LOC) #Graphe sans explication
plot(SURF,LOC,col=20,lwd=7,main='nuage de points', 
     xlab='Supeficie',ylab='Location')##Variable SURF expliqué et LOC variable à expliqué

scatterplot(LOC~SURF) #cas d'une base sans spécifié data=nombase de la base 
scatterplot(LOC~SURF, data=base) ##Inversion des vaiables (LOC variable expliqué par la Superficie)

# 3) Ajustement du modèle

model = lm(LOC~SURF, data=base)
model
summary(model) #Affiches Estimateur et std.Error(ecart type),tvalue=Estimate/std.Error
##***********le variable SURF à un impact significative sur le prix du loyer 
##******car la p-value est strictment inférieur au seuil de 5% (3,44e-10).
##******Si la superficie augmente d'une unité le prix du loyer va auagmenter de 28.239UM 
##******La superficie permet d'expliquer 81,21% de la variation total du prix du loyer .
##******Le reste (18,79%) est expliquer par la partie résiduel (l'erreur)
##******Le model est globalement significatif 
##******car la p-value associer au test de Ficher est inférieur au seuil de 5%

confint(model)	# Intervalles de Confiance des parametres
AIC(model)
BIC(model)

anova(model)	# tableau d'analyse de la variance



