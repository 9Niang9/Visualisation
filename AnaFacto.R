##...*** Clear the memory ***...##
rm(list=ls())

##******************************************************************************
##...*** Analyse en Correspondances Principales : ACP ***...##
##******************************************************************************

##...*** Installer et charger en mémoire les packages ***...##
install.packages("FactoMineR", dependencies = TRUE)

##...*** Installer plusieurs packages en même temps ***...##
install.packages(c("ExPosition", "corrplot", "ade4", "Factoshiny"),
                 dependencies = TRUE)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(ade4)
library(ExPosition)
library("corrplot")

##...*** 1. Importer le jeu de données ***...##
decath <- read.table("https://r-stat-sc-donnees.github.io/decathlon.csv",sep=";",dec=".", header=TRUE,
                     row.names=1, check.names=FALSE)

##...*** Afficher les 6 premières observation du jeu de données ***...##
head(decath)

head(decath[, 1:6], 4)

##...*** Calculer des statistiques élémentaires de bases ***...## 
summary(decath)

##...*** 2. Choisir les variables et les individus actifs ***...##

##...*** 3. Standardiser ou non les variables ***...##
res.pca <- PCA(decath, quanti.sup=11:12, quali.sup=13)

##...*** Afficher la liste, contenant les éléments sur le résultat de l'ACP ***...##
print(res.pca)

##...*** 4. Choisir le nombre d'axes ***...##
eig.val <- get_eigenvalue(res.pca)
eig.val

##...*** Générer le graphique des valeurs propres = mesurent la quantité de variance expliquée par chaque axe principal
## il n'existe pas de méthode objective bien acceptée pour décider du nombre d'axes principaux qui suffisent
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

##...*** Explorer le graphique des valeurs propres ***...##
barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig)))

##...*** 5. Analyser les résultats ***...##
summary(res.pca, ncp=4, nbelements=3)

## ====================== Graphique des individus =========================== ##
ind <- get_pca_ind(res.pca)
ind

# Coordonnées des individus
ind$coord
head(ind$coord)

# Qualité des individus
head(ind$cos2)

# Contributions des individus
head(ind$contrib)

##...*** Modifier la lisibité des graphiques et colorier des individus en fonction des variables qualitatives ***...##
plot(res.pca, choix="ind", habillage=13, cex = 1.1,
     select="cos2 0.6", title="Graphe des individus")

##...*** Produire le graphique des individus ***...##
fviz_pca_ind (res.pca)

##...*** Colorer les individus en fonction de leurs valeurs de cos2 ***...##
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

##...*** Construire les graphiques sur les axes 3 et 4 ***...##
plot(res.pca, choix="ind", habillage=13, axes=3:4, cex=0.7)

## ====================== Graphique des variables =========================== ##
var <- get_pca_var(res.pca)
var

# Coordonnées
head(var$coord)
head(var$coord, 4)

##...*** Cos2 : qualité de répresentation = mesure la qualité de représentation des variables sur la carte de l'ACP
head(var$cos2)

##...*** visualiser le cos2 des variables sur toutes les dimensions
corrplot(var$cos2, is.corr=FALSE)

##...*** créer un bar plot du cosinus carré des variables
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)


##...*** Contributions aux composantes principales ***...##
head(var$contrib)

# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

## contribution totale à PC1 et PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

## ====================== Cercl de corrélation de variables ============================ ##
##...*** Visualiser le cercle de corrélation des variables ***...##
fviz_pca_var(res.pca, col.var = "black")
plot(res.pca, choix="var", habillage=13, axes=3:4, new.plot=TRUE)

##...*** Colorer en fonction du cos2 : qualité de représentation ***...##
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

##...*** Changer la transparence en fonction du cos2 ***...##
fviz_pca_var(res.pca, alpha.var = "cos2")


##...*** 6. Décrire de façon automatique les principales dimensions de variabilité ***...##
dimdesc(res.pca, proba = 0.2)

##...*** 7. Retour aux données brutes ***...##
round(scale(decath[,1:12]),2)

##...*** Calculer la matrice des corrélations ***...##
round(cor(decath[1:12,1:12]),2)

##...*** L'interface graphique Factoshiny ***...##
res.shiny <- PCAshiny(decath)

##...*** Pour aller plus loin ***...##
plotellipses(res.pca)


##******************************************************************************
##...*** ACP d'une matrice ***...##
##******************************************************************************

##...*** Lecture des données TP ***...##
A=matrix(c(2,2,1,-1,-1,1,-2,-2),nrow=4, byrow=TRUE)
Nframe=as.data.frame(A)
m1=c("x1", "x2", "x3", "x4")
m2=c("var1", "var2")
row.names(A)=m1
colnames(A)=m2
head(A, 4)

# Matrice des covariances
cov(A)*(3/4)

# Matrice des correlations
cor(A)

##...*** ACP non normée ***...##
res.pca <- PCA(A, graph = FALSE, scale.unit = FALSE)
print(res.pca)
res.pca$eig
res.pca$var
res.pca$ind
#ACP normée
res.pca <- PCA(A, graph = FALSE)

##******************************************************************************

A=matrix(c(9,12,10,15,9,10,5,10,8,11,13,14,11,13,8,3,15,10),nrow=6, byrow=TRUE)
Nframe=as.data.frame(A)
m1=c("Alexis", "Béa", "Claude","Damien", "Emilie", "Francis")
m2=c("Physi", "Maths", "Anglais")
row.names(A)=m1
colnames(A)=m2
head(A, 4)
mean(A[,1])
# Matrice des covariances
cov(A)*(5/6)
# Matrice des correlations
cor(A)

##...*** Fonction PCA ...***## 

## ACP non normée
res.pca <- PCA(A, graph = FALSE, scale.unit = FALSE)
## ACP normée
res.pca <- PCA(A, graph = FALSE)
print(res.pca)

##...*** Les valeurs propres ***...##
res.pca$eig
eig.val=res.pca$eig
eig.val
barplot(res.pca$eig[,2])
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

##...*** Ou
eig.val <- get_eigenvalue(res.pca)
eig.val

## Résultats pour les variables
var <- get_pca_var(res.pca)
var

##******************************************************************************
##...*** TP 2 : ACP jeu de données : fonction factoextra ***...##
##******************************************************************************

##...*** Installer et charger les packages pour l'analyse et la visualisation ***...##
install.packages(c("FactoMineR", "factoextra", "ExPosition",
                   "ade4", dependencies = TRUE))
library(FactoMineR)
library(factoextra)
library(ade4)
library(corrplot)
library(stats)
library(ExPosition)
## =================================================== ##

##...*** Importer les jeux de données disponibles dans R : package factoextra ***...##
data(decathlon2)

##...*** Afficher les six premières observations du jeu de données ***...## 
head(decathlon2)

##...*** Afficher toute la base de données ***...##
View(decathlon2)

##...*** Vérifier les types de colonnes ***...##
str(decathlon2)

## =================================================== ##

##...*** Extraire les individus actifs et les variables actives pour l'ACP ***...##
decathlon2.active <- decathlon2[1:23, 1:10]

##...*** Afficher les 4 premières ind pour les 6 premières var ***...##
head(decathlon2.active[, 1:6], 4)

## =================================================== ##

##...*** Calculer l'ACP sur les individus/variables actifs ***...##
res.pca <- PCA(decathlon2.active, graph = FALSE)

##...*** Afficher la iste contenant les éléments de la fonction PCA() ***...##
print(res.pca)

## =================================================== ##

##...*** Visualisation et interprétation ***...##

##...*** 1. Valeurs propres / Variances ***...##
eig.val <- get_eigenvalue(res.pca)
eig.val

##...*** Générer le graphique des valeurs propres ***...##
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

## =================================================== ##

##...*** Graphique des variables
##...*** 2. Extraire les résultats, pour les variables, à partir de l'ACP ***...##
var <- get_pca_var(res.pca)
var

## =================================================== ##
##...*** Cercle de corrélation ***...##

# Coordonnées des variables
head(var$coord, 4)

# Visualiser les variables
fviz_pca_var(res.pca, col.var = "black")

##...*** Qualité de représentation ***...##
head(var$cos2, 4)

##...*** Visualiser le cos2 des variables sur toutes les dimensions ***...##
corrplot(var$cos2, is.corr=FALSE)

##...*** Créer un bar plot du cosinus carré des variables ***...##
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

##...*** Colorer les variables en fonction de la valeur de leurs cos2 ***...##
# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

##...***  Modifier la transparence des variables en fonction de leurs valeurs de cos2 ***...##
# Changer la transparence en fonction du cos2
fviz_pca_var(res.pca, alpha.var = "cos2")

## =================================================== ##
##...*** Contributions des variables aux axes principaux ***...##
#Extraire la contribution des variables
head(var$contrib, 4)

##...*** Mettre en évidence les variables les plus contributives pour chaque dimension ***...##
corrplot(var$contrib, is.corr=FALSE)

##...*** Créer un bar plot de la contribution des variables ***...##
# Décider de ne montrer que les principales variables contributives
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

##...*** Obtenir la contribution totale des variables sur les deux axes ***...##
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

##...*** Mises en évidence sur le graphe de corrélation, des variables les plus importantes (ou, contributives) ***...##
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

##...*** Modifier la transparence des variables en fonction de leurs contributions ***...##
# Changez la transparence en fonction de contrib
fviz_pca_var(res.pca, alpha.var = "contrib")

## =================================================== ##
##...*** Colorer en fonction d'une variable continue quelconque ***...##
# Créer une variable aléatoire continue de longueur 10
set.seed (123)
my.cont.var <- rnorm (10)
# Colorer les variables en fonction de la variable continue
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

##...*** Colorer par groupes ***...##
# Créez une variable de regroupement en utilisant kmeans
# Créez 3 groupes de variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Colorer les variables par groupes
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

## =================================================== ##
##...*** Description des dimensions ***...##
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)

# Description de la dimension 1
res.desc$Dim.1

# Description de la dimension 2
res.desc$Dim.2

# Description de la dimension 3
res.desc$Dim.3

## =================================================== ##
##...*** Graphique des individus ***...##
ind <- get_pca_ind(res.pca)
ind

##...*** Accéder aux différents résultats pour les individus ***...##
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

##...*** Graphique : qualité et contribution ***...##

# Produire le graphique des individus
fviz_pca_ind (res.pca)

# Colorer les individus en fonction de leurs valeurs de cos2
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Modifier la taille des points en fonction du cos2 des individus
fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE)

##...*** Modifier la taille et la couleur des points en fonction du cos2
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

##...*** Créer un bar plot de la qualité de représentation (cos2) des individus ***...##
fviz_cos2(res.pca, choice = "ind")

##...*** Visualiser la contribution des individus aux deux premières composantes principales ***...##
# Contribution totale sur PC1 et PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

##...*** Colorer en fonction d'une variable continue quelconque ***...##
# Créez une variable continue aléatoire de longueur 23,
# Même longeur que le nombre d'individus actifs dans l'ACP
set.seed (123)
my.cont.var <- rnorm(23)
# Colorer les individus par la variable continue
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

##...*** Colorer par groupes ***...##
        ## Jeu de données iris 
head(iris, 3)

##...*** Calculer l'analyse en composantes principales ***...##
# La variable Species (index = 5) est supprimée
# avant l'ACP
iris.pca <- PCA(iris [, - 5], graph = FALSE)

##...*** Utiliser la variable "species" : variable de regroupement pour spécifier les groupes ***...##
fviz_pca_ind(iris.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = iris$Species, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)

## ellipses de confiance au lieu des ellipses de concentration
# Ajoutez des ellipses de confiance
fviz_pca_ind(iris.pca, geom.ind = "point", col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)

##...*** Utiliser la palette de couleurs jco (journal of clinical oncology)
fviz_pca_ind(iris.pca,
             label = "none", # Caché le texte des individus
             habillage = iris$Species, # colorer par groupes
             addEllipses = TRUE, # Ellipses de concentration
             palette = "jco"
)

##...*** Visualiser les ind/Var sur les autres axes factoriels ***...##
# Variables sur les dimensions 2 et 3
fviz_pca_var(res.pca, axes = c(2, 3))
# Individus sur les dimensions 2 et 3
fviz_pca_ind(res.pca, axes = c(2, 3))

##...*** Eléments graphiques: point, texte, flèche
# Afficher les points et l'annotation des variables
fviz_pca_var(res.pca, geom.var = c("point", "text"))

## geom.ind: un texte spécifiant la géométrie à utiliser pour les individus
# Afficher uniquement l'annotation des individus
fviz_pca_ind(res.pca, geom.ind = "text")

##...*** Taille et forme des éléments graphiques ***...##
# Changez la taille des flèches et du texte
fviz_pca_var(res.pca, arrowsize = 1, labelsize = 5,
             repel = TRUE)
# Modification de la taille, de la forme 
# et de la couleur de remplissage des points
# Modifier la taille du texte
fviz_pca_ind (res.pca,
              pointsize = 3, pointshape = 21, fill = "lightblue",
              labelsize = 5, repel = TRUE)

##...*** Ellipses ***...##
# Add confidence ellipses
fviz_pca_ind(iris.pca, geom.ind = "point", 
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
# Convex hull
fviz_pca_ind(iris.pca, geom.ind = "point",
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
)

##...*** Centre de gravité: Le point moyen des groupes ***...##
fviz_pca_ind (iris.pca,
              geom.ind = "point", # afficher les points seulement (pas de "texte")
              col.ind = iris$Species, # Couleur par groupes
              legend.title = "Groupes",
              mean.point = FALSE)

##...*** Axes : modifier le trait des axes ***...##
fviz_pca_var (res.pca, axes.linetype = "blank")

##...*** Paramètres graphiques ***...##
ind.p <- fviz_pca_ind(iris.pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "Iris data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

##...*** Biplot : représenter les ind/Var sur les deux premières dimensions ***...##
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)

##...*** TP jeu de données "iris" : biplot ind et var ***...##
fviz_pca_biplot (iris.pca,
                 col.ind = iris$Species, palette = "jco",
                 addEllipses = TRUE, label = "var",
                 col.var = "black", repel = TRUE,
                 legend.title = "Species")

##...*** Colorer les individus et les variables par groupes ***...##
##...*** Puis, personnaliser les couleurs des individus et des variables ***...##
fviz_pca_biplot(iris.pca, 
                # Colueur de remplissage des individdus par groupes
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                # Colorer les variables par groupes
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        # Evite le chévauchement du texte
)+
  ggpubr::fill_palette("jco")+      # Couleur des individus
  ggpubr::color_palette("npg")      # Couleur des variables

##...*** Colorer les individus par groupes (couleurs discrètes) 
## et les variables par leurs contributions aux composantes principales (gradient de couleurs) 
##...*** modifier la transparence des variables par leurs contributions en utilisant l'argument alpha.var ***...##
fviz_pca_biplot(iris.pca, 
                # Individus
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)



##******************************************************************************
##...*** TP Analyse des Correspondances ; AFC ***...##
##******************************************************************************


##******************************************************************************
##...*** TP ##...*** Analyse des Corespondances Multiples ACM ***## ***...##
##******************************************************************************

##...*** 1. Importer le jeu de données ***##

credit <- read.table("https://r-stat-sc-donnees.github.io/credit.csv",sep=";", header=TRUE)
summary(credit)

##...*** Transformer une variable numérique en facteur ***##

credit[,"Age"] <- factor(credit[,"Age"])

##...*** vérifier les modalités rares, i.e. avec un faible effectif ***##

for (i in 1:ncol(credit)){ # permet d'avoir les graphes un à un
  par(ask=TRUE) # cliquer sur la fenêtre graphique
  plot(credit[,i]) } # pour voir le graphe

##...*** regrouper cette modalité avec Moto ***##

levels(credit[,"Marche"])[5] <- "Moto"

##...*** 2. Choisir les variables et les individus actifs ***##

library(FactoMineR)
res.mca <- MCA(credit, quali.sup=6:11, level.ventil=0)

##...*** 3. Choisir le nombre d'axes ***##

barplot(res.mca$eig[,2],names=paste("Dim",1:nrow(res.mca$eig)))

##...*** 4. Analyser les résultats avoir uniquement les individus ***##

plot(res.mca, invisible=c("var","quali.sup"))

##...*** colorier les individus en fonction d'une variable ***##

plot(res.mca, invisible=c("var","quali.sup"),habillage="Marche")
plot(res.mca, invisible=c("var","quali.sup"),habillage=1)

##...*** graphique avec l'ensemble des modalités des variables qualitatives actives et illustratives ***##

plot(res.mca, invisible="ind",
     title="Graphe des modalités actives et illustratives")
