##Installer plusieur Package##
install.packages(c("FactoMiner","ade4","Factoextra","Factoshiny",
                   "ExPosition","corrplot"),dep=T)

library(FactoMineR)
library(ad4)
library(factoextra)
library(Factoshiny)
library(ExPosition)
library(corrplot)

##1.Importer le jeu de donner:decathlon#
base_Decathlon<-read.table("C:/Users/touty diack/Desktop/L3 SID/Analyse_Donée_Statistique/TP_ACP_Decathlon/decathlon.csv", sep=";", dec = ".",
                           header = TRUE, row.names = 1, check.names=FALSE)
#Afficher les 6 premiere observation de la base:
head(base_Decathlon)

##Calcul ou Visualisation des statistique élémentaires de bases:#
summary(base_Decathlon)

                           ##*** Analyse en Correspondances Principales : ACP ***...##
##2;Choix des individus et des variables actifs:
##Calcul de l'ACP#

res.pca<-PCA(base_Decathlon,quanti.sup=11:12,quali.sup=13)
res.pca
var<-get_pca_var(res.pca)
var
##4.Choisir le nombre d'axe:
barplot(res.pca$eig[,2],names=paste("Dim",
                                    1:nrow(res.pca$eig)))
##3. Standardiser ou non des variables (ACP norme) :
res.pca$eig[,2]    #***********Affiche la 2éme colonne des valeurs propres eigenvalue(%variance)*****#
res.pca$eig
summary(res.pca,npc=4,nbelement=4)

                        ##***********Graphique des individus**********##
                        
plot(res.pca,choix="ind",habillage = 13,cex=1.1,select = "cos2 0.6",title = "Graphique des individus")
###GRAPHIQUES DES INDIVIDUS SUR LES AXES 3 ET 4
plot(res.pca,choix="ind",habillage = 13,axe=3:4,cex=0.7,new.plot = TRUE)






