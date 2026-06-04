library(car)
library(lmtest)
library(ggplot2)
library(GGally)
library(performance)
library(olsrr)
base = read.table(file.choose(), header=TRUE, sep=";")
attach(base)
plot(SURF,SCHAMB)
boxplot(SCHAMB)
scatterplot(LOC~SCHAMB) #ETUDE D'UNE INDIVIDUS PARTICULIER scatterplot(LOC[-11]~SHAMB[-11])
# Ajustement du modèle

model = lm(LOC~SCHAMB, data=base) #13 valeur abérent hors distribution dans,le cas contraire on parle influent
model
summary(model)
residus=model$residuals
sum(residus)
# Intervalles de Confiance des parametres
confint(model)	
AIC(model)
BIC(model)
#Analyse de la variance
anova(model)
# residus studentisés
rstud=rstudent(model)    
rstud #RESIDUS CORRECT EST UN RESIDUS COMPRIS ENTRE -2 ET 2 prendre les valeurs en valeur absolue
a=c()
for(i in 1:26){
  a[i]=ifelse(abs(rstud[i])>2,i,0) #a[i] incrementation de a  
  
}
a
#utlisation des graphes
influenceIndexPlot(model)
ols_plot_cooksd_bar(model)
ols_plot_dfbetas(model)
ols_plot_resid_lev(model)


#xavier Guyon modéle linéaire
#mode bis sans valeur abérente meillere model 
model.Bis = lm(LOC~SCHAMB, data=base[-13,]) #13 valeur abérent hors distribution dans,le cas contraire on parle influent
model.Bis
summary(model.Bis)
AIC(model.Bis)
BIC(model.Bis)
compareCoefs(model,model.Bis)
# 5) Droite de regression (droite des MCO)

plot(SCHAMB[-13],LOC[-13],col=4,lwd=2,
     main='Droite des moindres carres ordinaires',xlab='SUPERFICIE DES CHAMBRES',
     ylab='Coût du loyer')  #nuage de point

abline(model.Bis,col=10,lwd=3)#droite de régression 

segments(SCHAMB[-13],fitted(model.Bis),SCHAMB[-13],LOC[-13])
residus = model.Bis$residuals	# valeurs residuelles
residus
summary(residus)
sum(residus) #Somme des residu est nulle oula moyenne des residus est nulle
check_model(model.Bis) #courbe de lowess(2,1),courbe(1,1) PARFAIT SUPERPOSITION,courbe(2,2) voir les val aabérente
qqnorm(residus)
qqline(residus)    #Résidus Normale
par(mfrow=c(2,2))
plot(model.Bis,las=1)
#mu(moyenne) p-value=1>alpha et IntervalConf y'a 0
t.test(residus,mu=0) 
#Harrison-McCabe test p-value>alpha=5% ,on accepte H0
hmctest(model.Bis)
# test d'homogeneite uniquement C2(H0: Homoscedasticite[homogene],H1:Heteroscedasticite[heterogene]) 

ncvTest(model.Bis) #Non-constant Variance Score Test p-value>alpha=5%, accepte H0
# Non autocorrelation des residus C3
# H0: les residus sont non autocorreles (rejet H0)
# H1: les residus sont autocorreles ************

dwtest(model.Bis) # Durbin-Watson test p-value>alpha=5%, accepte H1

# test de normalite C4
# H0: les residus suivent une loi normale *******
# H1: les residus suivent pas une loi normale


shapiro.test(residus) #	Shapiro-Wilk normality test p-value>alpha=5% model adéquat au donnée

# Previsions

nd = data.frame(SCHAMB=c(20,42,71,39,60))
nd
pred=predict(model.Bis,interval="confidence",nd) #commande predict pour des prediction
pred#VALEUR MARCHAND NE DOIT PAS BAISSER(lwr) et fit moyenne et upr pas bon pour le client

# 8) Representation finale de la regression avec 
# l'IC et l'IC de prevision
base1=base[-13,]
ggplot(base1, aes(y=LOC, x=SCHAMB))+
  geom_point()+
  geom_smooth(colour="blue", method="lm", fill="red") +
  ylab("LOC")+
  xlab("SCHAMB") +
  theme_classic()+
  annotate("text", x = 40, y = 1430, label = "LOC = 448.5 + 18.5*SCHAMB") 


IC_pred = predict(model.Bis, interval="prediction")
IC_pred
head(IC_pred)
base2 = cbind(base, IC_pred)
head(base2)

ggplot(base1, aes(y=LOC, x=SCHAMB))+
  geom_point()+
  geom_smooth(colour="blue", method="lm", fill="red") +
  ylab("LOC")+
  xlab("SCHAMB") +
  theme_classic()+
  annotate("text", x = 20, y = 1230, label = "LOC = 448.5 + 18.5*SCHAMB") 



