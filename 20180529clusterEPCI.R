library(dplyr)
library(tidyverse)
#########################################################################################################################
# CLUSTERISATION DES EPCI
# 24 mai 2018 : suppression des données absolument corrélées et normalisation des variables avant les traitements
# 29 mai finalisation
#########################################################################################################################
###############################################################################################################
# 27 avril 2018 : CLUSTERING Production et Consommation et All
# 24 mai 2018 : REDUCTION DU NB DE VARIABLES
# - caractéristiques : cf. cidessus
# - production
# - consommation
###############################################################################################################

#autres variables : 8 (7 production - 1 consommation)
#otherEpci <- read.csv2("F:/Flexgrid/Sprint3/20180524clusterEpciOthers.csv",encoding = "UTF-8")
#otherProdEpci <- select(otherEpci,-c(consoBoisTonnes))
#otherConsoEpci <- select(otherEpci,c(codeEpci,consoBoisTonnes))
#otherProdEpci$codeEpci <- as.factor(otherProdEpci$codeEpci)
#7 variables + 6 others
prodEpci <- read.csv2("F:/Flexgrid/Sprint2/epciProduction.csv",encoding = "UTF-8")
# Valeurs NA : GdeHydrau:34 - PteHydrau: 26 - prodCoge: 40
prodEpci <- mutate(prodEpci, prodGdeHydrau = ifelse(is.na(prodGdeHydrau),0,prodGdeHydrau))
prodEpci <- mutate(prodEpci, prodPteHydrau = ifelse(is.na(prodPteHydrau),0,prodPteHydrau))
prodEpci <- mutate(prodEpci, prodCoge = ifelse(is.na(prodCoge),0,prodCoge))
# 29 mai 2018 
# comme pour les communes => passage à des taux pour Biomasse / hydraulique / solaire thermique et Solaire PV
# + supression coge + regroupement hydrau
prodEpci <- mutate(prodEpci, hydrau=prodGdeHydrau+prodPteHydrau)
prodEpci <- mutate(prodEpci, totalProd=prodBiomasse+hydrau+prodSolTher+prodSolairePV+prodCoge)
prodEpci <- mutate(prodEpci, tauxHydrau = ifelse(totalProd==0,0,round(round(hydrau,0) / totalProd * 100,2)))
prodEpci <- mutate(prodEpci, tauxBiomasse = ifelse(totalProd==0,0,round(round(prodBiomasse,0) / totalProd * 100,2)))
prodEpci <- mutate(prodEpci, tauxSolairePV = ifelse(totalProd==0,0,round(round(prodSolairePV,0) / totalProd * 100,2)))
prodEpci <- mutate(prodEpci, tauxSolaireTher = ifelse(totalProd==0,0,round(round(prodSolTher,0) / totalProd * 100,2)))
prodEpci <- select(prodEpci,c(codeEpci,totalProd,tauxHydrau,tauxBiomasse,tauxSolairePV,tauxSolaireTher))
summary(prodEpci)
#ecrire le fichier traité
write.csv2(prodEpci, file="F:/Flexgrid/Sprint3/epciTauxProduction.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

# NE PAS AJOUTER CES DATA
#prodEpci <- left_join(prodEpci,otherProdEpci,by=c("codeEpci"))
#prodEpci$codeEpci <- as.factor(prodEpci$codeEpci)
#summary(prodEpci)

#16 variables : consommation par activité et consommation par energie + 1 other
consoEpci <- read.csv2("F:/Flexgrid/Sprint2/epciConsommation.csv",encoding = "UTF-8")
# 29 mai 2018 
# comme pour les communes => passage à des taux pour Biomasse / hydraulique / solaire thermique et Solaire PV
# + supression coge + regroupement hydrau
summary(consoEpci)
#Répartition par Activité : 
# Extraction (3 quartiles à 0)- Transports Autres ((717 NA))des vals à 0
# Industrie - Résidentiel - Tertiaire - Agriculture - Transport routiers - 
#Calcul des Taux : tous sauf Extraction et TA
consoEpci <- mutate(consoEpci, totalAct=consoExtraction+consoIndustrie+consoRes+consoTertiaire+consoAgri+consoTR+consoTA)
consoEpci <- mutate(consoEpci, actTauxIndustrie = ifelse(totalAct==0,0,round(round(consoIndustrie,0) / totalAct * 100,2)))
consoEpci <- mutate(consoEpci, actTauxResidentiel = ifelse(totalAct==0,0,round(round(consoRes,0) / totalAct * 100,2)))
consoEpci <- mutate(consoEpci, actTauxTertiaire = ifelse(totalAct==0,0,round(round(consoTertiaire,0) / totalAct * 100,2)))
consoEpci <- mutate(consoEpci, actTauxAgriculture = ifelse(totalAct==0,0,round(round(consoAgri,0) / totalAct * 100,2)))
consoEpci <- mutate(consoEpci, actTauxTransportR = ifelse(totalAct==0,0,round(round(consoTR,0) / totalAct * 100,2)))
summary(consoEpci)
# Répartition par Energie : 
# Chaleur Froid - Non EnR - Combustibles : 3 quartiles à 0 
# Petrole (0 NA)- Gaz Naturel (vals à 0) - - Electricité (0 NA) - Bois (0 NA) - Autres EnR (0 NA) - 
# Calcul des Taux : Pétrole, Electricite, Bois, AutreEnR
consoEpci <- mutate(consoEpci, totalEn=consoPetrole+consoGN+consoCombo+consoElec+consoBois+consoAutreEnr+consoChaleurFroid+consoNonEnr)
consoEpci <- mutate(consoEpci, enTauxPetrole = ifelse(totalEn==0,0,round(round(consoPetrole,0) / totalEn * 100,2)))
consoEpci <- mutate(consoEpci, enTauxElectricite = ifelse(totalEn==0,0,round(round(consoElec,0) / totalEn * 100,2)))
consoEpci <- mutate(consoEpci, enTauxBois = ifelse(totalEn==0,0,round(round(consoBois,0) / totalEn * 100,2)))
consoEpci <- mutate(consoEpci, enTauxAutreEnR = ifelse(totalEn==0,0,round(round(consoAutreEnr,0) / totalEn * 100,2)))

# NE PAS AJOUTER CES DATA
#consoEpci <- left_join(consoEpci,otherConsoEpci,by=c("codeEpci"))
#consoEpci$codeEpci <- as.factor(consoEpci$codeEpci)
#summary(consoEpci)

#consoAct <- select(consoCom,contains("act"))
consoTauxEpciAct <- select(consoEpci,c("codeEpci","totalAct","actTauxIndustrie","actTauxResidentiel","actTauxAgriculture","actTauxTransportR"))
consoTauxEpciEn <- select(consoEpci,c("codeEpci","totalEn","enTauxPetrole","enTauxElectricite","enTauxBois","enTauxAutreEnR"))
consoTauxEpci <- select(consoEpci,c("codeEpci","totalAct","actTauxIndustrie","actTauxResidentiel","actTauxAgriculture","actTauxTransportR","enTauxPetrole","enTauxElectricite","enTauxBois","enTauxAutreEnR"))
consoTauxEpci <- rename(consoTauxEpci,totalConso=totalAct)

#ecrire les fichiers traités
write.csv2(consoTauxEpciAct, file="F:/Flexgrid/Sprint3/epciTauxActConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(consoTauxEpciEn, file="F:/Flexgrid/Sprint3/epciTauxEnConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(consoTauxEpci, file="F:/Flexgrid/Sprint3/epciTauxConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

######################################################################################
# SPRINT 3 : COMMUNES je cherche une commune comparable
# 29 mai => clustering final
# Kmeans caractéristiques : population, surface, emploi
#        consommation : total / par activité / par energie
#        production   : Biomasse / hydraulique / Solaire Thermique / Solaire PV
######################################################################################
# 29 mai fichier caractéristiques + code Epci
# 15 variables déjà réduites cf. ci-dessus
pacaEpci <- read.csv2("F:/Flexgrid/Sprint3/20180524clusterEpciCaract.csv",encoding = "UTF-8")
#pacaEpci$codeEpci <- as.factor(pacaEpci$codeEpci)
# 29 mai 2018 : données de consommation => 10 variables
consoEpciTaux <-read.csv2(file="F:/Flexgrid/Sprint3/epciTauxConsommation.csv", fileEncoding = "UTF-8")
# activite seul => 6 variables
consoEpciAct <-read.csv2(file="F:/Flexgrid/Sprint3/epciTauxActConsommation.csv", fileEncoding = "UTF-8")
# energie seul => 6 variables
consoEpciEn <-read.csv2(file="F:/Flexgrid/Sprint3/epciTauxEnConsommation.csv", fileEncoding = "UTF-8")
# PRODUCTION => 6 variables
prodEpci <-read.csv2(file="F:/Flexgrid/Sprint3/epciTauxProduction.csv", fileEncoding = "UTF-8")

#normalisation des données
#CARACTERISTIQUES => 14 variables
pacaEpciR <- pacaEpci
pacaEpciR[,2:15] <- scale(pacaEpci[,2:15])
testcar <- pacaEpciR[,2:15]

#PRODUCTION => 5 variables
prodEpciR <- prodEpci
prodEpciR[,2:6] <- scale(prodEpci[,2:6])
testprod <- prodEpciR[,2:6]

#CONSOMMATION => 9 variables
consoEpciR <- consoEpciTaux
consoEpciR[,2:10] <- scale(consoEpciTaux[,2:10])
testconso <- consoEpciR[,2:10]

#Consommation Activité
consoEpciActR <- consoEpciAct
consoEpciActR[,2:6] <- scale(consoEpciAct[,2:6])
testconsoAct <- consoEpciActR[,2:6]

#Consommation Energie
consoEpciEnR <- consoEpciEn
consoEpciEnR[,2:6] <- scale(consoEpciEn[,2:6])
testconsoEn <- consoEpciEnR[,2:6]

#TOUS LES JEUX DE DONNEES => 5 classifications
#pacaEpciR - prodEpciR - testProdEpciR - testConsoEpciR - testAllEpciR

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(123)
#CARACTERISTIQUES seul
km2Car = kmeans(testcar, 6, nstart=100)
table(km2Car$cluster)
#PRODUCTION seul
km2Prod = kmeans(testprod, 6, nstart=100)
table(km2Prod$cluster)
#CONSOMMATION seul
km2Conso = kmeans(testconso, 6, nstart=100)
table(km2Conso$cluster)
#CONSOMMATION activite
km2ConsoAct = kmeans(testconsoAct, 6, nstart=100)
table(km2ConsoAct$cluster)
#CONSOMMATION energie
km2ConsoEn = kmeans(testconsoEn, 6, nstart=100)
table(km2ConsoEn$cluster)

# Examine the result of the clustering algorithm
km2Car       #between/total = 59,9%
km2Prod      #between/total = 85,1%
km2Conso     #between/total = 66,8%
km2ConsoAct  #between/total = 77,3%
km2ConsoEn   #between/total = 79,8%

#Répartition
table(km2Car$cluster)
table(km2Prod$cluster)
table(km2Conso$cluster)
table(km2ConsoAct$cluster)
table(km2ConsoEn$cluster)

#Constitution du fichier cluster + Geo avec le numéro de cluster
#Ajout du classement + pop2013 + nbEmploiTotal : pas la densité ?
clusterData <- select(pacaEpci,c(codeEpci,pop2013, nbEmploiTotal,nbCommune,densite)) 
clusterData <- add_column(clusterData,kmeanEpciCar=km2Car$cluster)
#clusterData$codeEpci <- as.character(clusterData$codeEpci)
#Ajout du champ total consommation
clusterDataConso <- select(consoEpciTaux,c(codeEpci,totalConso)) 
clusterDataConso <- add_column(clusterDataConso,kmeanEpciConso=km2Conso$cluster)
clusterData <- left_join(clusterData,clusterDataConso,by=c("codeEpci"))
#Ajout du cluster par activité
clusterDataAct <- select(consoEpciAct,c(codeEpci)) 
clusterDataAct <- add_column(clusterDataAct,kmeanEpciConsoAct=km2ConsoAct$cluster)
clusterData <- left_join(clusterData,clusterDataAct,by=c("codeEpci"))
#Ajout du cluster par energie
clusterDataEn <- select(consoEpciEn,c(codeEpci)) 
clusterDataEn <- add_column(clusterDataEn,kmeanEpciConsoEn=km2ConsoEn$cluster)
clusterData <- left_join(clusterData,clusterDataEn,by=c("codeEpci"))
#Ajout du champ total production
clusterDataProd <- select(prodEpci,c(codeEpci,totalProd)) #,dep,libelleCommune
clusterDataProd <- add_column(clusterDataProd,kmeanEpciProd=km2Prod$cluster)
clusterData <- left_join(clusterData,clusterDataProd,by=c("codeEpci"))

#ajouter le referentiel
epciref<-read.csv2("F:/Flexgrid/Sprint2/referentielEpciGeo.csv",encoding = "UTF-8")
clusterEpci <- left_join(clusterData,epciref,by=c("codeEpci"))
clusterEpci$lat <- str_replace(as.character(clusterEpci$lat),",",".")
clusterEpci$lon <- str_replace(as.character(clusterEpci$lon),",",".")

#stocker le clustering des epci avec Geo
#write.csv2(clusterEpci, file="F:/Flexgrid/Sprint3/20180524clusterEpciGeoComplet.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(clusterEpci, file="F:/Flexgrid/Sprint3/20180529clusterEpci.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
