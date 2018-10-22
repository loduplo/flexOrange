# 25 mai 2018 - finalisation le 29 mai 2018
library(dplyr)
library(tidyverse)
library(ggplot2)
######################################################################################
# SPRINT 3 : COMMUNES je cherche une commune comparable
# 29 mai => clustering final
# Kmeans caractéristiques : population, surface, emploi
#        consommation : total / par activité / par energie
#        production   : Biomasse / hydraulique / Solaire Thermique / Solaire PV
######################################################################################
# 23 mai fichier caractéristiques + libelle Communes
pacaCommunes <-read.csv2(file="F:/Flexgrid/Sprint3/communeCaract2305.csv", fileEncoding = "UTF-8")
# 28 mai 2018 : retraitement des données de consommation => 10 variables
consoCommunesTaux <-read.csv2(file="F:/Flexgrid/Sprint3/comTauxConsommationCode.csv", fileEncoding = "UTF-8")
# activite seul
consoCommunesAct <-read.csv2(file="F:/Flexgrid/Sprint3/comTauxActConsommationCode.csv", fileEncoding = "UTF-8")
# energie seul
consoCommunesEn <-read.csv2(file="F:/Flexgrid/Sprint3/comTauxEnConsommationCode.csv", fileEncoding = "UTF-8")
# PRODUCTION
prodCommunes <-read.csv2(file="F:/Flexgrid/Sprint3/comTauxProductionCode.csv", fileEncoding = "UTF-8")

#28 mai - 29 mai production
# suppression des NA : 1 seul sur le tauxSalarie
pacaCommunes$tauxSalarie <- replace(pacaCommunes$tauxSalarie,is.na(pacaCommunes$tauxSalarie),75) #moyenne
# normalisation des datas : sauf codeCommune et libelleCommmune
carCommunesR <- pacaCommunes
carCommunesR[2:36] <- scale(pacaCommunes[2:36])
testcomcar <- carCommunesR[,2:36]
summary(testcomcar)
# Taux : normalisation des datas : sauf codeCommune
consoCommunesTauxR <- consoCommunesTaux
consoCommunesTauxR[2:10] <- scale(consoCommunesTaux[2:10])
testcom <- consoCommunesTauxR[,2:10]
# Activite
consoCommunesActR <- consoCommunesAct
consoCommunesActR[2:6] <- scale(consoCommunesAct[2:6])
testcomAct <- consoCommunesActR[,2:6]
# Energie
consoCommunesEnR <- consoCommunesEn
consoCommunesEnR[2:6] <- scale(consoCommunesEn[2:6])
testcomEn <- consoCommunesEnR[,2:6]
# production : 6 variables
prodCommunesR <- prodCommunes
prodCommunesR[2:6] <- scale(prodCommunes[2:6])
testcomProd <- prodCommunesR[,2:6]

##################################################################################
# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(123)
km2car = kmeans(testcomcar,6, nstart=100)
km2conso = kmeans(testcom,6, nstart=100)
km2consoAct = kmeans(testcomAct,6, nstart=100)
km2consoEn = kmeans(testcomEn,6, nstart=100)
km2prod = kmeans(testcomProd,8, nstart=100)
# Examine the result of the clustering algorithm
km2car
table(km2car$cluster)
km2conso
table(km2conso$cluster)
km2consoAct
table(km2consoAct$cluster)
km2consoEn
table(km2consoEn$cluster)
km2prod
table(km2prod$cluster)
# Constitution du fichier cluster + Geo avec le numéro de cluster

# Ajout de la densité
pacaCommunes <- mutate(pacaCommunes,densite=pop2013/superficie)
# Ajout du classement + pop2013 + nbEmploiTotal + densite
clusterData <- select(pacaCommunes,c(codeCommune,pop2013, nbEmploiTotal, densite)) #,dep,libelleCommune
clusterData <- add_column(clusterData,kmeanComCar=km2car$cluster)
clusterData$codeCommune <- as.character(clusterData$codeCommune)
#Ajout du champ total consommation
clusterDataConso <- select(consoCommunesTaux,c(codeCommune,totalConso)) #,dep,libelleCommune
clusterDataConso <- add_column(clusterDataConso,kmeanComConso=km2conso$cluster)
clusterDataConso$codeCommune <- as.character(clusterDataConso$codeCommune)
clusterData <- left_join(clusterData,clusterDataConso,by=c("codeCommune"))
#Ajout du cluster par activité
clusterDataAct <- select(consoCommunesAct,c(codeCommune)) #,dep,libelleCommune
clusterDataAct <- add_column(clusterDataAct,kmeanComConsoAct=km2consoAct$cluster)
clusterDataAct$codeCommune <- as.character(clusterDataAct$codeCommune)
clusterData <- left_join(clusterData,clusterDataAct,by=c("codeCommune"))
#Ajout du cluster par energie
clusterDataEn <- select(consoCommunesEn,c(codeCommune)) #,dep,libelleCommune
clusterDataEn <- add_column(clusterDataEn,kmeanComConsoEn=km2consoEn$cluster)
clusterDataEn$codeCommune <- as.character(clusterDataEn$codeCommune)
clusterData <- left_join(clusterData,clusterDataEn,by=c("codeCommune"))
#Ajout du champ total production
clusterDataProd <- select(prodCommunes,c(codeCommune,totalProd)) #,dep,libelleCommune
clusterDataProd <- add_column(clusterDataProd,kmeanComProd=km2prod$cluster)
clusterDataProd$codeCommune <- as.character(clusterDataProd$codeCommune)
clusterData <- left_join(clusterData,clusterDataProd,by=c("codeCommune"))

#ajout des coordonnées géographiques
refCom<-read.csv2("F:/Flexgrid/Sprint3/REF/referentielCommuneGeo.csv",dec=",",encoding = "UTF-8")
refCom$codeCommune <- as.character(refCom$codeCommune)
clusterData <- left_join(clusterData,refCom,by=c("codeCommune"))
#Ecriture du fichier kmean Caract avec Geo
write.csv2(clusterData, file="F:/Flexgrid/Sprint3/clusterCommunekmean.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#Fichier pour ES


#
library(FactoMineR)
# 1. ACP
res.pca <- PCA(testcomcar, ncp = 6, graph = FALSE)
# 2. HCPC
res.hcpc <- HCPC(res.pca, graph = FALSE)

table(res.hcpc$data.clust$clust)
