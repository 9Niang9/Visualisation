  #**********************Examen_Modélisation_Linéaire**************************#

                        #*******Mamadou_Niang*******#
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(ggplot2)
library(car)
library(lmtest)
library(GGally)
library(performance)
library(olsrr)
library(finalfit)

#*******Exercice 1*******#


#Charger la base de données

base1=read.table(file.choose(), header = TRUE, sep=";")
base1
attach(base1)
#**1. Tracer	le	nuage	de	points	et	commenter.**#	

scatterplot(Revenu,Consommation,col="blue",
       xlab ="Revenu",
       ylab ="Consommation des menage",
       main ="Relation entre le revenu et la consommation des ménages")
abline(lm(Consommation ~ Revenu, data = base1), col = "red") #ligne de tendance linéaire 

#**Commentaire: Le nuage de point montre une relation croissante.   **#
#**L'axe des X représente le Revenu et l'axe Y represente la conssomation des menages.**
#**Le nuage de point montre que la conssomation des menages augmente en méme temps que le Revenu **
#**Les menages à faible revenu on une conssomation inférieure à 10000 et celle à forte revenu ont tendance*
#** à depasser une conssomation de 14000 **

#**2. Peut-on	utiliser	un	modèle de	régressions pour	expliquer	la	relation	entre	la	consommation	et	le	revenu ? Justifier**
#**3. Estimer	les	paramètres du	modèle.**
model = lm(Consommation~Revenu, data=base1)
model
summary(model)
coef(model)
confint(model)

#**4. En	déduire les	valeurs	estimées de	Ct.(Voir pdf)	**

#**5. Étudier la	présence de	valeurs	aberrantes**
plot(model, which = 1)

#**6. Étudier l’adéquation du	modèle.**
anova(model)
summary(model)$r.squared

#**7. Vérifier par	un	calcul	simple	la	propriété selon	laquelle	la	moyenne	des	résidus est	nulle.**
# Calcul de la moyenne des résidus
model.Bis = lm(Consommation~Revenu, data=base1[-13:-14,]) #suppression des valeurs abberantes
model.Bis
plot(model.Bis, which = 1)
residus<-resid(model.Bis)
residus
mean(residus)

#**8. Calculer	l’estimateur	de	la	variance	de	l’erreur**
length(coef(model.Bis))
length(residus)
variance_erreur <- sum(residus^2) / (length(residus) - length(coef(model.Bis)) - 1)
variance_erreur

#**9. Tester	la	significativité́de	la	pente**
coefficient_model.Bis<-coef(model.Bis)
coefficient_model.Bis
pente <- coefficient_model.Bis["Revenu"]
pente
ecart_type_pente <- summary(model.Bis)$coefficients["Revenu", "Std. Error"]
ecart_type_pente
statistique_test <- pente / ecart_type_pente
statistique_test
p_value <- 2 * pt(-abs(statistique_test), df = length(resid(model.Bis)) - 2)
p_value

#**10. Construire	l’intervalle	de	confiance	au	niveau de confiance	de 95% pour le paramètre β.**
# Obtention du quantile de la distribution de Student
seuil <- 0.05  # Niveau de confiance de 95%
n <- length(resid(model.Bis))  # Nombre d'observations
p <- length(coefficient_model.Bis) - 1  # Nombre de paramètres (incluant l'intercept)
quantile<- qt(1 - seuil/2, df = n - p - 1)

# Calculer l'intervalle de confiance
borne_inf <- pente - quantile * ecart_type_pente
borne_sup <- pente + quantile * ecart_type_pente

# Afficher l'intervalle de confiance
print(c(borne_inf, borne_sup))

#**11. Calculer	le	coefficient	de détermination et effectuer	le test	de Fisher permettant de déterminer si la régression est significative dans son ensemble.**
summary(model.Bis)

#**12. Écrire et vérifier l’équation d’analyse de la variance. Interpréter.	**
# Calculer la somme des carrés totaux (SCT)
data=base1[-13:-14,]
SCT <- sum((data$Consommation - mean(data$Consommation))^2)

# Calculer la somme des carrés de la régression (SSR)
SSR <- sum((predict(model.Bis) - mean(base1$Consommation))^2)

# Calculer la somme des carrés des résidus (SCR)
SCR <- sum(resid(model.Bis)^2)

# Vérifier l'équation ANOVA
SST_verifie <- SSR + SCR

# Afficher les résultats
print(SCT)
print(SSR)
print(SCR)
print(SST_verifie)

#**13. Après un	travail	minutieux,	un	étudiant de	L1	FASE	trouve	le	coefficient	de*	
#**corrélation linéaire entre	Ct	et	Rt	suivant	rXY	=	0.99789619.	Sans le moindre	calcul,	*
#**tester la significativité  de	ce	coefficient.	Argumenter**#
#**test de corrélation de Pearson
cor.test(Revenu,Consommation)

#**14. En	2024 et	2025,	on	prévoit respectivement	16800	et	17000	euros	pour*
#**	la	valeur	du	revenu.	Déterminer	les	valeurs	prévues de la	consommation*
#**pour	ces	deux	années,	ainsi	que	l’intervalle	de	prévision au	niveau	de*
#**	confiance	de	95%.	**#
nouvelles_donnes<-data.frame(Revenu = c(16800, 17000))
nouvelles_donnes
valeurs_prevues <- predict(model.Bis, nouvelles_donnes)
valeurs_prevues
# Calculer l'intervalle de prédiction au niveau de confiance de 95%

intervalle_prediction <- predict(model.Bis, nouvelles_donnes, interval = "prediction", level = 0.95)
intervalle_prediction



library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(ggplot2)
library(car)
library(lmtest)
library(GGally)
library(performance)
library(olsrr)
library(finalfit)
#*******Exercice 2*******#


#Charger la base de données

base2=read.table(file.choose(), header = TRUE, sep=";")
base2
attach(base2)
#**1Existe-t-il une corrélation entre les variables explicatives ?*
#** Si	oui, précisez quelles variables	et analyser les	résultats.(voir pdf)*
res.pca<-PCA(base2,scale.unit = TRUE,ncp=4,graph = TRUE) 
res.pca

#**2) Ajuster	un modèle de régression linéaire multiple et étudier l’adéquation du modèle.*
#**Analyser	les	résultats (Effectuez toutes les	étapes de	l’analyse).**

#  Etude de la linéarité entre les variables

ggpairs(base2)

#  Ajustement du modèle

model_1=lm(ozone ~. , data = base2)	# prends toutes les covariables
model_1
summary(model_1)

#  Etudes des valeurs atypiques

rstud=rstudent(model_1)    # residus studentisés
rstud

influenceIndexPlot(model_1)

ols_plot_resid_lev(model_1)

# comparaison des coefficients

modelbis = lm(ozone ~., data=base2[-c(13,35,2),])
modelbis
summary(modelbis)
compareCoefs(model_1,modelbis) 

AIC(model_1)
AIC(modelbis)

#Adéquation du modèle

par(mfrow=c(2,2))
plot(m3bis,las=1)

check_model(model_1)

check_normality(modelbis)

check_heteroscedasticity(modelbis)

residus = modelbis$residuals
summary(residus)

# Colinearite des variables explicatives

check_collinearity(modelbis)

vif1 = vif(modelbis)
vif1
