
### REGRESSION LINEAIRE MULTIPLE ET SELECTION DE VARIABLE

# 1) Charger les packages nécessaires

library(car)
library(lmtest)
library(ggplot2)
library(GGally)
library(performance)
library(olsrr)
library(finalfit)


# 2) Charger la base de données

base=read.table(file.choose(), header = TRUE, sep=";")
attach(base)
str(base)
head(base)
edit(base)

# 3) Etude de la linéarité entre les variables

ggpairs(base)

scatterplotMatrix(base)

# 4) Ajustement du modèle

m0=lm(LOC ~. , data = base)	# prends toutes les covariables
summary(m0)

m01=lm(LOC ~. , data = base[,-3])	# prends toutes les covariables
summary(m01)

m02=lm(LOC ~. , data = base[,-c(3,6)])	# prends toutes les covariables
summary(m02)

m1=lm(LOC ~ SURF+CHAMB+SCHAMB+SDB+SSDB+PARK+PLAGE+UNIV, data = base)	# prends toutes les covariables
summary(m1)

m2=lm(LOC ~ SURF+SCHAMB+SDB+SSDB+PARK+PLAGE+UNIV, data = base)	# prends toutes les covariables
summary(m2)

m3=lm(LOC ~ SURF+SCHAMB+SDB+PARK+PLAGE+UNIV, data = base)	# prends toutes les covariables
summary(m3)

m4=step(m1)
summary(m4)

# 5) Etudes des valeurs atypiques

rstud=rstudent(m3)    # residus studentisés
rstud

influenceIndexPlot(m3)

ols_plot_cooksd_bar(m3)

ols_plot_dfbetas(m3)
ols_plot_resid_lev(m3)

# pvalue de Bonferroni (H0: outlier = observation atypique)

outlierTest(m3)

# comparaison des coefficients

m3bis = lm(LOC ~ SURF+SCHAMB+SDB+PARK+PLAGE+UNIV, data=base[-11,])
summary(m3bis)
compareCoefs(m3,m3bis) 

summary(m3bis)
summary(m3)

AIC(m3bis)
AIC(m3)

# 6) Adéquation du modèle

par(mfrow=c(2,2))
plot(m3bis,las=1)

check_model(m3)

check_normality(m3)

check_heteroscedasticity(m3bis)

residus = m3bis$residuals
summary(residus)

# test de nullite de la moyenne
# H0: la moyenne des r?sidus est nulle

t.test(residus,mu=0)

# linéarité de la relation, normalité et homosc?dasticité / hétéroscédasticité
# H0: les r?sidus suivent une loi normale de m?me variance

hmctest(m3bis)

# Non autocorr?lation des r?sidus
# Durbin-Watson test 
# H0: les residus sont non autocorr?l?s

dwtest(m3bis)

# test de normalit? 
# H0: les r?sidus suivent une loi normale

shapiro.test(residus)

plot(density(residus),lwd=3,col=3)
hist(residus,probability = TRUE)
lines(density(residus),lwd=3,col=3)

# Colinearite des variables explicatives

check_collinearity(m3bis)

vif1 = vif(m3bis)
vif1


# 7) Présentation des résultats

explanatory = c("SURF","SCHAMB","SDB","PARK","PLAGE","UNIV")
explanatory
dependent = "LOC"

base %>%
lmmulti(dependent,explanatory)%>%
fit2df()

# 8) Prévisions

nd=data.frame(SURF=24,SCHAMB=7,SDB=2,PARK=10,PLAGE=5,UNIV=3)
nd
loyer_pred=predict(m3bis,interval="confidence",nd)
loyer_pred



###################
