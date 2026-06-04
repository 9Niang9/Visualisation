#########Commentaire:
#########Un jeu de donnée individus X variables
#####Description du jeu de donnée:
#Le jeu de donnée concerne les resultats aux epreuves du decathlon lors de deux competition d'athlétisme qui on eu lieux à un mois d'interval: 
#Les jeu olympique d'Athénes(Du 23 au 24 aout 2004) et le Decastar entre le (25 et 26 aout 2004).
#Lors d'une competition, les athlétes participe à 5 épreuves(100m,Longueur,Poids,Hauteur,400m) le 1er jour, 
#puis au épeuves restantes(110m,Disques,Perche,Javelot,1500m)le lendemain.
#Dans le tableau 1.1,nous avons recueillis pour chaque athléte ses performances dans chacunes des 10 epreuves,son classement final,
#son nombres de points final et la compétion à la quel il a participe

 
##...***Nettoyer la mémoire**...##
rm(list = ls())

##Installer plusieur Package##
install.packages(c("FactoMiner","ade4","Factoextra","Factoshiny",
                   "ExPosition","corrplot"),dep=T)

library(FactoMineR)
library(ad4)
library(factoextra)
library(Factoshiny)
library(ExPosition)
library(corrplot)

#Importer le jeu de donner:decathlon#
base_Decathlon<-read.table("C:/Users/touty diack/Desktop/L3 SID/Analyse_Donée_Statistique/TP_ACP_Decathlon/decathlon.csv", sep=";", dec = ".",
                    header = TRUE, row.names = 1, check.names=FALSE)

##Affiher les 6 premieres observations#
head(base_Decathlon)

##Visualiser les statistique élémentaires de bases:#
summary(base_Decathlon)

##Vérifiers type de colones#
str(base_Decathlon)

                   ##...*** Analyse en Correspondances Principales : ACP ***...##

##Calcul de l'ACP#
res.pca<-PCA(base_Decathlon,quanti.sup=11:12,quali.sup=13)
res.pca

##--1--****-Les valeurs propres***---##
eig.val<-get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels =TRUE, ylim=c(0,50) )


                   ##--2--****.Analyse des relations entre les variables#

var<-get_pca_var(res.pca)
var

##---***Coordonées des variables(Representation=correlation de la variable dans la dim)***---#
head(var$coord)

#Pour toute les variables#
var$coord

#Affichage COS2(Qualiter des representation)#
head(var$cos2)

#Pour toute les cos2#
var$cos2

#Contribution des variables#
head(var$contrib)
var$contrib

                   ##--3--****.Analyse des relations entre les individus#

ind<-get_pca_ind(res.pca)
ind
##Coordonner des individus#
head(ind$coord)
ind$coord

##Contibution des individus#
head(ind$contrid)
ind$contrib

##Cos2 des individus
head(ind$cos2)
ind$cos2

            ## ====================== Graphique des variables =========================== ##

#Cercle de correlation des variables(c-à-d les relation entre les variables)#
plot.PCA (res.pca,choix = "var", col.var = "green")

fviz_pca_var(res.pca,col.var = "black")



                  ##--3--***Analyse de la proximiter des individus***---#

ind<-get_pca_ind(res.pca)
ind

##Coordonner des individus#
head(ind$coord)
ind$coord

##Cos2 Qualiter des representations#
head(ind$cos2)
ind$cos2

##Contribution des individus#
head(ind$contrib)
ind$contrib
                     ## ====================== Graphique des individus =========================== ##


#Produire le graphique des indivoidus sur le plan Factorial#
fviz_pca_ind(res.pca)

#Colorer les individus en fonction du cos2#
fviz_pca_ind(res.pca,col.ind = "cos2",gradients.cols=c("#00AFBB","#E7B800","#FC4E07",repel=TRUE))

#Visualiser le cos2 des var sur toute les dimensions#
corrplot(var$cos2,is.core=FALSE)

##...*** Construire les graphiques sur les axes 3 et 4 ***...##
plot(res.pca, choix="ind", habillage=13, axes=3:4, cex=0.7)

#Calcul de la matrice de correlation
round(cor(base_Decathlon [1:10, 1:10]), 2)
