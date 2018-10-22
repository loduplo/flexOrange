library(dplyr)
library(tidyverse)
######################################################################################################################
##################################################################################################
#### DONNEES AIR PACA
##################################################################################################
# DONNEES de production
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airpacaProd <- read.csv2("dumpProduction.csv", dec= ".", sep=",",encoding = "UTF-8")
#type factor => numeric
#replace by reading with dec='.' 
#airPacaConso$production<-gsub(".", ",", airPacaConso$production, fixed=TRUE)
summary(airpacaProd)
levels(airpacaProd$detail_filiere_cigale)
colnames(airpacaProd)
#changement de nom des colonnes
airpacaProd <- rename(airpacaProd,codeCommune = id_comm)
airpacaProd <- rename(airpacaProd,grandeFiliere = grande_filiere_cigale)
airpacaProd <- rename(airpacaProd,detailFiliere = detail_filiere_cigale)
airpacaProd <- rename(airpacaProd,productionMwh = val)
airpacaProd <- rename(airpacaProd,annee = an)
airpacaProd <- rename(airpacaProd,typeProduction = lib_type_prod)
#airpacaProd <- rename(airpacaProd,unite = id_unite)
airpacaProd$id_unite <- NULL
airpacaProd$siren_epci_2017 <- NULL
airpacaProd$color_grande_filiere_cigale <- NULL
airpacaProd$color_detail_filiere_cigale <- NULL
airpacaProd$id_grande_filiere_cigale <- NULL
airpacaProd$id_detail_filiere_cigale <- NULL
airpacaProd$nom_epci_2017 <- NULL
airpacaProd$dep <- NULL
airpacaProd$id_type_prod <- NULL
head(airpacaProd)
str(airpacaProd)
#type factor => numeric
#replace by reading with dec='.' 
#airpacaProd$production<-gsub(".", ",", airpacaProd$production, fixed=TRUE)
#changement de la precision MWh avec 4 chiffres après la virgule
airpacaProd <- mutate(airpacaProd, productionMwh = round(productionMwh,4))
#ajouter les libelle commune dans le referentiel => regroupement des données par libelle (+dep)
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comRef <- select(comRef, c(codeCommune,libelleCommune,codeDep))
airpacaProd <- left_join(airpacaProd, comRef, "codeCommune")
airpacaProd <- mutate(airpacaProd,libelle = paste(libelleCommune,codeDep,sep=""))
airpacaProd$libelle <- as.factor(airpacaProd$libelle)
str(airpacaProd)
###########################################################################################
#regroupement des données sur les communes ayant même libelle + mm codeDep (cause doublons)# agregation des données par EPCI => valeur EPCI
#agregation des données par commune 2017
#29 mai : MODIFICATION libelle(+Dep) => remplacé par codeCommune
#dataProd <- filter(airpacaProd, annee==anprod)
#comProdAn <- group_by(dataProd,libelle,detailFiliere)
#perCommune <- summarize(comProdAn,productionMwh=sum(productionMwh))
#table(perCommune$detailFiliere)
#transformer une ligne en colonne
addComFiliereProduction <- function(data,filiere,annomcol) {
  msg <- paste(filiere,annomcol)
  print(msg)
  maprod <- filter(data, detailFiliere==filiere)
  maprod <- select(maprod,c(codeCommune,productionMwh))
  colnames(maprod) <- c("codeCommune",annomcol)
  return(maprod)
}
# fonction de traitement des donnees de production sur une annee
# le fichier completeData contient toutes les années : airPacaProd
dataProdAnnee <- function (date,anprod)
{
  dataProd <- filter(airpacaProd, annee==anprod)
  comProdAn <- group_by(dataProd,codeCommune,detailFiliere)
  perCommune <- summarize(comProdAn,productionMwh=sum(productionMwh))

  #detail filiere
  colBiomasse <- paste(date,"prodBiomasse",sep="")
  colGdeHydrau <- paste(date,"prodGdeHydrau",sep="")
  colPteHydrau <- paste(date,"prodPteHydrau",sep="")
  colSolTher <- paste(date,"prodSolTher",sep="")
  colSolPV <- paste(date,"prodSolPV",sep="")
  colCoge <- paste(date,"prodCoge",sep="")

  laprodCom <- addComFiliereProduction(perCommune,"Biomasse",colBiomasse) 
  #grande hydraulique
  newProd0 <- addComFiliereProduction(perCommune,"Grande hydraulique",colGdeHydrau) 
  laprodCom <- left_join(laprodCom,newProd0,by=c("codeCommune"))
  laprodCom[[colGdeHydrau]] <- ifelse(is.na(laprodCom[[colGdeHydrau]]),0,laprodCom[[colGdeHydrau]])
  #petite hydraulique
  newProd1 <- addComFiliereProduction(perCommune,"Petite hydraulique",colPteHydrau) 
  laprodCom <- left_join(laprodCom,newProd1,by=c("codeCommune"))
  laprodCom[[colPteHydrau]] <- ifelse(is.na(laprodCom[[colPteHydrau]]),0,laprodCom[[colPteHydrau]])
  #solaire thermique
  newProd2 <- addComFiliereProduction(perCommune,"Solaire thermique", colSolTher) 
  laprodCom <- left_join(laprodCom,newProd2,by=c("codeCommune"))
  #solaire PV
  newProd3 <- addComFiliereProduction(perCommune,"Solaire photovoltaïque", colSolPV) 
  laprodCom <- left_join(laprodCom,newProd3,by=c("codeCommune"))
  laprodCom[[colSolPV]] <- ifelse(is.na(laprodCom[[colSolPV]]),0,laprodCom[[colSolPV]])
  #cogénération
  newProd4 <- addComFiliereProduction(perCommune,"Cogénération", colCoge) 
  laprodCom <- left_join(laprodCom,newProd4,by=c("codeCommune"))
  laprodCom[[colCoge]] <- ifelse(is.na(laprodCom[[colCoge]]),0,laprodCom[[colCoge]])
  return (laprodCom)
}
#selection des données par année
#airpacaProd contient 2007 - 2010 - 2012 - 2013 - 2014 - 2015
summary(airpacaProd)
fileProd2007 <- dataProdAnnee("2007",2007)
summary(fileProd2007)
fileProd2010 <- dataProdAnnee("2010",2010)
summary(fileProd2010)
fileProdCom <- left_join(fileProd2007,fileProd2010,by=c("codeCommune"))
fileProd2012 <- dataProdAnnee("2012",2012)
summary(fileProd2012)
fileProdCom <- left_join(fileProdCom,fileProd2012,by=c("codeCommune"))
fileProd2013 <- dataProdAnnee("2013",2013)
summary(fileProd2013)
fileProdCom <- left_join(fileProdCom,fileProd2013,by=c("codeCommune"))
fileProd2014 <- dataProdAnnee("2014",2014)
summary(fileProd2014)
fileProdCom <- left_join(fileProdCom,fileProd2014,by=c("codeCommune"))
fileProd2015 <- dataProdAnnee("2015",2015)
summary(fileProd2015)
fileProdCom <- left_join(fileProdCom,fileProd2015,by=c("codeCommune"))
summary(fileProdCom)
#ecrire le fichier traite : années 2007, 2010, 2012 à 2015 -> 37 variables
write.csv2(fileProdCom, file="F:/Flexgrid/Sprint3/20180529communeProductionAP.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
# 29 mai 2018 : selection de 2015 seulement (comme pour la consommation) => fileProd2015
# modifcation MW => taux
# regroupement de gdeHydro + pteHydrau => hydrau / suppression de coge => prodTotale 
# + 4 taux : Biomasse,Hydra,SolaireThermique,SolairePV 
summary(fileProd2015)
fileProd2015 <- rename(fileProd2015,gdeHydrau="2015prodGdeHydrau")
fileProd2015 <- rename(fileProd2015,pteHydrau="2015prodPteHydrau")
fileProd2015 <- mutate(fileProd2015, hydrau=gdeHydrau+pteHydrau)
fileProd2015 <- rename(fileProd2015,solairePV="2015prodSolPV")
fileProd2015 <- rename(fileProd2015,solaireTher="2015prodSolTher")
fileProd2015 <- rename(fileProd2015,coge="2015prodCoge")
fileProd2015 <- rename(fileProd2015,biomasse="2015prodBiomasse")
fileProd2015 <- mutate(fileProd2015, totalProd=biomasse+hydrau+solaireTher+solairePV+coge)
fileProd2015 <- mutate(fileProd2015, tauxHydrau = ifelse(totalProd==0,0,round(round(hydrau,0) / totalProd * 100,2)))
fileProd2015 <- mutate(fileProd2015, tauxBiomasse = ifelse(totalProd==0,0,round(round(biomasse,0) / totalProd * 100,2)))
#fileProd2015 <- mutate(fileProd2015, tauxPteHydrau = ifelse(totalProd==0,0,round(round(pteHydrau,0) / totalProd * 100,2)))
fileProd2015 <- mutate(fileProd2015, tauxSolairePV = ifelse(totalProd==0,0,round(round(solairePV,0) / totalProd * 100,2)))
fileProd2015 <- mutate(fileProd2015, tauxSolaireTher = ifelse(totalProd==0,0,round(round(solaireTher,0) / totalProd * 100,2)))
prodCom <- select(fileProd2015,c(codeCommune,totalProd,tauxHydrau,tauxBiomasse,tauxSolairePV,tauxSolaireTher))
#ecrire le fichier traite : année 2015 -> 10 variables
write.csv2(prodCom, file="F:/Flexgrid/Sprint3/comTauxProduction.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
###########################################################
# OT : TRAITEMENT DU FICHIER ENERGIE => ajouter à la production #
###########################################################
setwd("F:/Flexgrid/Sprint3")
#Modification du fichier qui contient les communes 2016 cad 953 communes
# St Vincent les Forts (codeCommune 4033) + la Breole (4198) => devient Ubaye-Serre-Ponçon (codeCommune 4033)
# Pelvoux (codeCommune 5175) + Vallouisse (5101) => devient Valouisse-Pelvoux (codeCommune 5101)
comEnergie <- read.csv2("ExportEnergiesCom2017.csv",encoding = "UTF-8")
summary(comEnergie)
colnames(comEnergie)
#changement de nom des colonnes
comEnergie <- rename(comEnergie,codeCommune = Code)
comEnergie <- rename(comEnergie,nbChaufferies = nombre_chaufferies)
comEnergie <- rename(comEnergie,puissanceChaufferiekw = puiss_kw)
comEnergie <- rename(comEnergie,consoBoisTonnes = conso_bois_tonnes)
comEnergie <- rename(comEnergie,nbPVBati = nb_pv)
comEnergie <- rename(comEnergie,puissancePVmw = puiss_mw)
comEnergie <- rename(comEnergie,nbInstalPV = nb_inst)
comEnergie <- rename(comEnergie,surfacePVm2 = surface_m2)
head(comEnergie)

#nbPV factor => integer
comEnergie$nbPVBati <- as.integer(comEnergie$nbPVBati)
#comEnergie$surfacePVm2 <- as.integer(comEnergie$surfacePVm2)
#changement d'unite pour les puissances
comEnergie <- mutate(comEnergie, puissanceChaufferiemw = ifelse(puissanceChaufferiekw=="NA","NA",round(puissanceChaufferiekw / 1000,4)))
comEnergie <- mutate(comEnergie, puissancePVmw = ifelse(puissancePVmw=="NA","NA",round(puissancePVmw,3)))

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comRef <- select(comRef,c(codeCommune,libelleCommune,codeDep))
comEnergie <- left_join(comEnergie, comRef, "codeCommune")
comEnergie <- mutate(comEnergie,libelle = paste(libelleCommune,codeDep,sep=""))
comEnergie$libelle <- as.factor(comEnergie$libelle)
summary(comEnergie)
head(comEnergie)
comEnergie$com2016 <- NULL
comEnergie$codeDep <- NULL
comEnergie$libelleCommune <- NULL
comEnergie$codeCommune <- NULL
comEnergie$puissanceChaufferiekw <- NULL
#Remplacement des NA par 0
comEnergie$nbChaufferies <- replace(comEnergie$nbChaufferies,is.na(comEnergie$nbChaufferies),0)
comEnergie$nbPVBati <- replace(comEnergie$nbPVBati,is.na(comEnergie$nbPVBati),0)
comEnergie$nbInstalPV <- replace(comEnergie$nbInstalPV,is.na(comEnergie$nbInstalPV),0)
comEnergie$puissancePVmw <- replace(comEnergie$puissancePVmw,is.na(comEnergie$puissancePVmw),0)
comEnergie$puissanceChaufferiemw <- replace(comEnergie$puissanceChaufferiemw,is.na(comEnergie$puissanceChaufferiemw),0)
comEnergie$surfacePVm2 <- replace(comEnergie$surfacePVm2,is.na(comEnergie$surfacePVm2),0)
comEnergie$consoBoisTonnes <- replace(comEnergie$consoBoisTonnes,is.na(comEnergie$consoBoisTonnes),0)
summary(comEnergie)
#ecrire le fichier traite
write.csv2(comEnergie, file="F:/Flexgrid/Sprint3/communeProductionOT.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

##################################################################################################
# 26 avril 2018
# agregation des données par EPCI => valeur EPCI
# selection des données de l'année 2015
prod2015 <- filter(airpacaProd, annee==2015)
prod2015 <- select(prod2015, c(codeCommune,typeProduction,grandeFiliere,detailFiliere,productionMwh,codeEPCI,libelleEPCI))
epciProd2015 <- group_by(prod2015,codeEPCI,detailFiliere)
perEpci <- summarize(epciProd2015,productionMwh=sum(productionMwh))
##### AUTRE ECRITURE ###################
#tab <- prod2015
#tab2015 <- tab %>% group_by(codeEPCI,detailFiliere) %>% summarise(production=sum(productionMwh))
######################################################################
levels(epciProd2015$detailFiliere)
# biomasseEpci <- filter(perEpci,detailFiliere=="Biomasse")
# biomasseEpci <- select(biomasseEpci,c(codeEPCI,productionMwh))
# colnames(biomasseEpci) <- c("codeEPCI","prodBiomasse")
# biogazEpci <- filter(perEpci,detailFiliere=="Biogaz")
# biogazEpci <- select(biogazEpci,c(codeEPCI,productionMwh))
# colnames(biogazEpci) <- c("codeEPCI","prodBiogaz")
# laprodEpci <- left_join(biomasseEpci,biogazEpci,by=c("codeEPCI"))
# #Solaire thermique
# solthEpci <- filter(perEpci,detailFiliere=="Solaire thermique")
# solthEpci <- select(solthEpci,c(codeEPCI,productionMwh))
# colnames(solthEpci) <- c("codeEPCI","prodSolTh")
# laprodEpci <- left_join(laprodEpci,solthEpci,by=c("codeEPCI"))
#Transformer les lignes en colonnes
table(epciProd2015$detailFiliere)
addFiliereProduction <- function(data,filiere,nomcol) {
  msg <- paste(filiere,nomcol)
  print(msg)
  maprod <- filter(data, detailFiliere==filiere)
  maprod <- select(maprod,c(codeEPCI,productionMwh))
  colnames(maprod) <- c("codeEPCI",nomcol)
  return(maprod)
}
#detailFiliere
# Cogénération, solaire thermique, Biomasse, Grande hydraulique, Petite hydraulique, Solaire photovoltaïque
laprodEpci <- addFiliereProduction(perEpci,"Biomasse","prodBiomasse") 
newProd0 <- addFiliereProduction(perEpci,"Grande hydraulique","prodGdeHydrau") 
laprodEpci <- left_join(laprodEpci,newProd0,by=c("codeEPCI"))
newProd1 <- addFiliereProduction(perEpci,"Petite hydraulique","prodPteHydrau") 
laprodEpci <- left_join(laprodEpci,newProd1,by=c("codeEPCI"))
newProd2 <- addFiliereProduction(perEpci,"Solaire thermiques","prodSolTher") 
laprodEpci <- left_join(laprodEpci,newProd2,by=c("codeEPCI"))
newProd3 <- addFiliereProduction(perEpci,"Solaire photovoltaïque","prodSolairePV") 
laprodEpci <- left_join(laprodEpci,newProd3,by=c("codeEPCI"))
newProd4 <- addFiliereProduction(perEpci,"Cogénération","prodCoge") 
laprodEpci <- left_join(laprodEpci,newProd4,by=c("codeEPCI"))
#Sauvegarde des données de production par Epci
write.csv2(laprodEpci, file="F:/Flexgrid/Sprint3/epciProduction.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
# DONNEES de consommation
##########################
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaConsommation <- read.csv2("dumpConsommation.csv", dec= ".", sep=",",encoding = "UTF-8")
airPacaConso <-select(airPacaConsommation,c("an","id_comm","id_secten1","code_cat_energie","val"))
summary(airPacaConso)
###################################################################
#TRAITEMENT DES VARIABLES FACTOR : secten1, catégorie d'energie
##################################################################
#Mettre ces variables numériques => factor et recoder les modalites
#id_secten1 : 8 modalités 
#1:Energie, 2:Industrie/déchets, 3: Résidentiel, 4: Tertiaire
#5:Agriculture, 6:Transports routiers 7: Autres transports, 8: Non inclus
table(airPacaConso$id_secten1)
# 1      2      3      4      5      6      7 
# 440  21148  84861 412078  27986  11340   2337
airPacaConso$id_secten1 <- as.factor(airPacaConso$id_secten1)
str(airPacaConso$id_secten1)
levels(airPacaConso$id_secten1)<-c("Extraction","Industrie","Résidentiel","Tertiaire","Agriculture","Transport routier","Transports autres")
head(airPacaConso)

#code_cat_energie : 8 modalités 
#1:Gaz Naturel, 2:Produits pétroliers, 3: Combustibles Minéraux Solides, 4: Bois-énergie
#5:Autres EnR, 6:Chaleur et froid 7:Autres non EnR, 8: Electricité
table(airPacaConso$code_cat_energie)
#1      2      3      4      5      6      7      8 
#55290 171307     10  89679  11447    920    170 231367
airPacaConso$code_cat_energie <- as.factor(airPacaConso$code_cat_energie)
str(airPacaConso$code_cat_energie)
levels(airPacaConso$code_cat_energie)<-c("Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Chaleur et froid","Autres non EnR","Electricité")
head(airPacaConso)
#l'unite id_unite=1 pour toutes les lignes cad tep
#changement de la precision MWh avec 4 chiffres après la virgule
airPacaConso <- mutate(airPacaConso, consommationMwh = round((val*11.63),4))
airPacaConso$val <- NULL
#changement de nom des colonnes
colnames(airPacaConso)
airPacaConso <- rename(airPacaConso,codeCommune = id_comm)
airPacaConso <- rename(airPacaConso,activite = id_secten1)
airPacaConso <- rename(airPacaConso,energie = code_cat_energie)
airPacaConso <- rename(airPacaConso,date = an)
head(airPacaConso)

#ajouter les libelle commune dans le referentiel => regroupement des données par libelle (+dep)
#25 mai : modification libelle(+Dep) => remplacé par codeCommune
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
comRef <- select(comRef, c(codeCommune,libelleCommune,codeDep))
airPacaConso <- left_join(airPacaConso, comRef, "codeCommune")
airPacaConso <- mutate(airPacaConso,libelle = paste(libelleCommune,codeDep,sep=""))
airPacaConso$libelle <- as.factor(airPacaConso$libelle)
#airPacaConso$libelleCommune <- NULL
#airPacaConso$codeDep <- NULL

## Consommation par activite
#transformer une ligne en colonne
addColActiviteConso <- function(data,valAct,nomCol) {
  msg <- paste(valAct,nomCol)
  print(msg)
  maconso <- filter(data, activite==valAct)
  maconso <- select(maconso,c(codeCommune,consommation))
  colnames(maconso) <- c("codeCommune",nomCol)
  return(maconso)
}
dataConso <- filter(airPacaConso, date==2015)
comProdActivite <- group_by(dataConso,codeCommune,activite)
perCommuneAct <- summarize(comProdActivite,consommation=sum(consommationMwh))
table(perCommuneAct$activite)
#transformer les lignes activite en colonnes
laconsCom <- addColActiviteConso(perCommuneAct,"Tertiaire","consoTertiaire")
newConso1 <- addColActiviteConso(perCommuneAct,"Industrie","consoIndustrie") 
laconsCom <- left_join(laconsCom,newConso1,by=c("codeCommune"))
newConso2 <- addColActiviteConso(perCommuneAct,"Résidentiel","consoRes") 
laconsCom <- left_join(laconsCom,newConso2,by=c("codeCommune"))
newConso3 <- addColActiviteConso(perCommuneAct,"Extraction","consoExtraction")
laconsCom <- left_join(laconsCom,newConso3,by=c("codeCommune"))
newConso4 <- addColActiviteConso(perCommuneAct,"Agriculture","consoAgri") 
laconsCom <- left_join(laconsCom,newConso4,by=c("codeCommune"))
newConso5 <- addColActiviteConso(perCommuneAct,"Transport routier","consoTR") 
laconsCom <- left_join(laconsCom,newConso5,by=c("codeCommune"))
newConso6 <- addColActiviteConso(perCommuneAct,"Transports autres","consoTA") 
laconsCom <- left_join(laconsCom,newConso6,by=c("codeCommune"))
summary(laconsCom)

#Consommation par energie
#On filtre energie=valType et on ajoute les données dans une nouvelle colonne : nomCol
addColEnergieConso <- function(data,valType,nomCol) {
  msg <- paste(valType,nomCol)
  print(msg)
  maconso <- filter(data, energie==valType)
  maconso <- select(maconso,c(codeCommune,consommation))
  colnames(maconso) <- c("codeCommune",nomCol)
  return(maconso)
}
comConso2015Energie <- group_by(dataConso,codeCommune,energie)
perComEnergie <- summarize(comConso2015Energie,consommation=sum(consommationMwh))
table(perComEnergie$energie)
levels(perComEnergie$energie)
#[1] "Gaz Naturel"                   "Produits pétroliers"           "Combustibles Minéraux Solides" "Bois-énergie"                 
#[5] "Autres EnR" 
 
newConso0 <- addColEnergieConso(perComEnergie,"Gaz Naturel","consoGN") 
laconsCom <- left_join(laconsCom,newConso0,by=c("codeCommune"))
newConso1 <- addColEnergieConso(perComEnergie,"Combustibles Minéraux Solides","consoCombo") 
laconsCom <- left_join(laconsCom,newConso1,by=c("codeCommune"))
newConso2 <- addColEnergieConso(perComEnergie,"Bois-énergie","consoBois") 
laconsCom <- left_join(laconsCom,newConso2,by=c("codeCommune"))
newConso3 <- addColEnergieConso(perComEnergie,"Autres EnR","consoAutreEnr") 
laconsCom <- left_join(laconsCom,newConso3,by=c("codeCommune"))
newConso4 <- addColEnergieConso(perComEnergie,"Chaleur et froid","consoChaleurFroid") 
laconsCom <- left_join(laconsCom,newConso4,by=c("codeCommune"))
newConso5 <- addColEnergieConso(perComEnergie,"Autres non EnR","consoNonEnr") 
laconsCom <- left_join(laconsCom,newConso5,by=c("codeCommune"))
newConso6 <- addColEnergieConso(perComEnergie,"Electricité","consoElec") 
laconsCom <- left_join(laconsCom,newConso6,by=c("codeCommune"))
newConso7 <- addColEnergieConso(perComEnergie,"Produits pétroliers","consoPetrole")
laconsCom <- left_join(laconsCom,newConso7,by=c("codeCommune"))

#RAF : remplacer tous les NA par la valeur 0

#ecrire le fichier traite
write.csv2(laconsCom, file="F:/Flexgrid/Sprint3/comConsommationCode.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

# PBairPacaConso <-select(airPacaConso, c(codeEPCI,codeCommune))
# PBairPacaConso <-filter(PBairPacaConso, is.na(codeEPCI))
# doublons <- which(duplicated(PBairPacaConso$codeCommune))
# pbcommunes<-PBairPacaConso[-doublons,]
# airPacaConso$codeEPCI <- replace(airPacaConso$codeEPCI,is.na(airPacaConso$codeEPCI),0)
#communes pas dans un EPCI ? 5002, 5005, 5020, 5069, 4100

##################################################################################################
# 28 mai 2018 : modification MW => taux
# séparation répartition par Activité / répartition par Energie
consoCom <- read.csv2("F:/Flexgrid/Sprint3/comConsommationCode.csv",encoding = "UTF-8")
summary(consoCom)
#Répartition par Activité : Extraction (935 NA)- Industrie (1 NA) - Résidentiel (0 NA) - Tertiaire (0 NA) - Agriculture (1 NA) - Transport routiers (6 NA) - Transports Autres (717 NA)
consoCom$consoExtraction <- replace(consoCom$consoExtraction,is.na(consoCom$consoExtraction),0) #0
consoCom$consoIndustrie <- replace(consoCom$consoIndustrie,is.na(consoCom$consoIndustrie),0) #0 pour calcul du total
consoCom$consoAgri <- replace(consoCom$consoAgri,is.na(consoCom$consoAgri),0) #0
consoCom$consoTR <- replace(consoCom$consoTR,is.na(consoCom$consoTR),0) #0 
consoCom$consoTA <- replace(consoCom$consoTA,is.na(consoCom$consoTA),0) #0
#Calcul des Taux : tous sauf Extraction et TA
consoCom <- mutate(consoCom, totalAct=consoExtraction+consoIndustrie+consoRes+consoTertiaire+consoAgri+consoTR+consoTA)
consoCom <- mutate(consoCom, actTauxIndustrie = ifelse(totalAct==0,0,round(round(consoIndustrie,0) / totalAct * 100,2)))
consoCom <- mutate(consoCom, actTauxResidentiel = ifelse(totalAct==0,0,round(round(consoRes,0) / totalAct * 100,2)))
consoCom <- mutate(consoCom, actTauxTertiaire = ifelse(totalAct==0,0,round(round(consoTertiaire,0) / totalAct * 100,2)))
consoCom <- mutate(consoCom, actTauxAgriculture = ifelse(totalAct==0,0,round(round(consoAgri,0) / totalAct * 100,2)))
consoCom <- mutate(consoCom, actTauxTransportR = ifelse(totalAct==0,0,round(round(consoTR,0) / totalAct * 100,2)))
summary(consoCom)
#Répartition par Energie : Petrole (0 NA)- Gaz Naturel (656 NA) - Combustibles (950 NA) - Electricité (0 NA) - Bois (0 NA) - Autres EnR (0 NA) - Chaleur Froid (939 NA) - Non EnR (937 NA)
#NA -> 0 pour calcul du total
consoCom$consoGN <- replace(consoCom$consoGN,is.na(consoCom$consoGN),0) 
consoCom$consoCombo <- replace(consoCom$consoCombo,is.na(consoCom$consoCombo),0) 
consoCom$consoChaleurFroid <- replace(consoCom$consoChaleurFroid,is.na(consoCom$consoChaleurFroid),0) 
consoCom$consoNonEnr <- replace(consoCom$consoNonEnr,is.na(consoCom$consoNonEnr),0) 
#Calcul des Taux : Pétrole, Electricite, Bois, AutreEnR
consoCom <- mutate(consoCom, totalEn=consoPetrole+consoGN+consoCombo+consoElec+consoBois+consoAutreEnr+consoChaleurFroid+consoNonEnr)
consoCom <- mutate(consoCom, enTauxPetrole = ifelse(totalEn==0,0,round(round(consoPetrole,0) / totalEn * 100,2)))
consoCom <- mutate(consoCom, enTauxElectricite = ifelse(totalEn==0,0,round(round(consoElec,0) / totalEn * 100,2)))
consoCom <- mutate(consoCom, enTauxBois = ifelse(totalEn==0,0,round(round(consoBois,0) / totalEn * 100,2)))
consoCom <- mutate(consoCom, enTauxAutreEnR = ifelse(totalEn==0,0,round(round(consoAutreEnr,0) / totalEn * 100,2)))

#consoAct <- select(consoCom,contains("act"))
consoTauxComAct <- select(consoCom,c("codeCommune","totalAct","actTauxIndustrie","actTauxResidentiel","actTauxAgriculture","actTauxTransportR"))
consoTauxComEn <- select(consoCom,c("codeCommune","totalEn","enTauxPetrole","enTauxElectricite","enTauxBois","enTauxAutreEnR"))
consoTauxCom <- select(consoCom,c("codeCommune","totalAct","actTauxIndustrie","actTauxResidentiel","actTauxAgriculture","actTauxTransportR","enTauxPetrole","enTauxElectricite","enTauxBois","enTauxAutreEnR"))
consoTauxCom <- rename(consoTauxCom,totalConso=totalAct)

#ecrire les fichiers traités
write.csv2(consoTauxComAct, file="F:/Flexgrid/Sprint3/comTauxActConsommationCode.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(consoTauxComEn, file="F:/Flexgrid/Sprint3/comTauxEnConsommationCode.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(consoTauxCom, file="F:/Flexgrid/Sprint3/comTauxConsommationCode.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)


##################################################################################################
# 27 avril 2018
# agregation des données par EPCI => valeur EPCI
# selection des données de l'année 2015
conso2015 <- filter(airPacaConso, date==2015)
conso2015 <- select(conso2015, c(codeCommune,activite,energie,consommationMwh,codeEPCI,libelleEPCI))
#Consommation par activité et par energie
# epciConso2015 <- group_by(conso2015,codeEPCI,activite,energie)
# perEpci <- summarize(epciConso2015,consommation=sum(consommationMwh))
# table(perEpci$activite)
# table(perEpci$energie)

#On filtre energie=valType et on ajoute les données dans une nouvelle colonne : nomCol
addColEnergieConso <- function(data,valType,nomCol) {
  msg <- paste(valType,nomCol)
  print(msg)
  maconso <- filter(data, energie==valType)
  maconso <- select(maconso,c(codeEPCI,consommation))
  colnames(maconso) <- c("codeEPCI",nomCol)
  return(maconso)
}

#Consommation par energie
epciConso2015Energie <- group_by(conso2015,codeEPCI,energie)
perEpciEnergie <- summarize(epciConso2015Energie,consommation=sum(consommationMwh))
table(perEpciEnergie$energie)
levels(perEpciEnergie$energie)
#[1] "Gaz Naturel"                   "Produits pétroliers"           "Combustibles Minéraux Solides" "Bois-énergie"                 
#[5] "Autres EnR" 
laconsoEpci <- addColEnergieConso(perEpciEnergie,"Produits pétroliers","consoPetrole") 
newConso0 <- addColEnergieConso(perEpciEnergie,"Gaz Naturel","consoGN") 
laconsoEpci <- left_join(laconsoEpci,newConso0,by=c("codeEPCI"))
newConso1 <- addColEnergieConso(perEpciEnergie,"Combustibles Minéraux Solides","consoCombo") 
laconsoEpci <- left_join(laconsoEpci,newConso1,by=c("codeEPCI"))
newConso2 <- addColEnergieConso(perEpciEnergie,"Bois-énergie","consoBois") 
laconsoEpci <- left_join(laconsoEpci,newConso2,by=c("codeEPCI"))
newConso3 <- addColEnergieConso(perEpciEnergie,"Autres EnR","consoAutreEnr") 
laconsoEpci <- left_join(laconsoEpci,newConso3,by=c("codeEPCI"))
newConso4 <- addColEnergieConso(perEpciEnergie,"Chaleur et froid","consoChaleurFroid") 
laconsoEpci <- left_join(laconsoEpci,newConso4,by=c("codeEPCI"))
newConso5 <- addColEnergieConso(perEpciEnergie,"Autres non EnR","consoNonEnr") 
laconsoEpci <- left_join(laconsoEpci,newConso5,by=c("codeEPCI"))
newConso6 <- addColEnergieConso(perEpciEnergie,"Electricité","consoElec") 
laconsoEpci <- left_join(laconsoEpci,newConso6,by=c("codeEPCI"))

#Consommation par activité
epciConso2015Act <- group_by(conso2015,codeEPCI,activite)
perEpciAct <- summarize(epciConso2015Act,consommation=sum(consommationMwh))
table(perEpciAct$activite)
levels(perEpciAct$activite)
#[1] "Extraction"        "Industrie"         "Résidentiel"       "Tertiaire"         "Agriculture"       "Transport routier"
#[7] "Transports autres"
#On filtre activite=valActivite et on ajoute les données dans une nouvelle colonne : nomCol
addColActiviteConso <- function(data,valAct,nomCol) {
  msg <- paste(valAct,nomCol)
  print(msg)
  maconso <- filter(data, activite==valAct)
  maconso <- select(maconso,c(codeEPCI,consommation))
  colnames(maconso) <- c("codeEPCI",nomCol)
  return(maconso)
}
newConso0 <- addColActiviteConso(perEpciAct,"Extraction","consoExtraction") 
laconsoEpci <- left_join(laconsoEpci,newConso0,by=c("codeEPCI"))
newConso1 <- addColActiviteConso(perEpciAct,"Industrie","consoIndustrie") 
laconsoEpci <- left_join(laconsoEpci,newConso1,by=c("codeEPCI"))
newConso2 <- addColActiviteConso(perEpciAct,"Résidentiel","consoRes") 
laconsoEpci <- left_join(laconsoEpci,newConso2,by=c("codeEPCI"))
newConso3 <- addColActiviteConso(perEpciAct,"Tertiaire","consoTertiaire") 
laconsoEpci <- left_join(laconsoEpci,newConso3,by=c("codeEPCI"))
newConso4 <- addColActiviteConso(perEpciAct,"Agriculture","consoAgri") 
laconsoEpci <- left_join(laconsoEpci,newConso4,by=c("codeEPCI"))
newConso5 <- addColActiviteConso(perEpciAct,"Transport routier","consoTR") 
laconsoEpci <- left_join(laconsoEpci,newConso5,by=c("codeEPCI"))
newConso6 <- addColActiviteConso(perEpciAct,"Transports autres","consoTA") 
laconsoEpci <- left_join(laconsoEpci,newConso6,by=c("codeEPCI"))

#Sauvegarde des données de consommation par Epci
write.csv2(laconsoEpci, file="F:/Flexgrid/Sprint3/epciConsommation.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
##################################################################################################
#
#       AIR PACA : GES : CO2, NH4eqCO2, N2OeqCO2, PRG100
#
##################################################################################################
# GES : CO2 - id_polluant 15 unite 21005 cad kg
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES1 <- read.csv2("dumpCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES1)
airPacaGES <-select(airPacaGES1,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : N2O eq CO2 - id_polluant 124
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES2 <- read.csv2("dumpN2OeqCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES2)
airPacaGES <-select(airPacaGES2,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : CH4 eq CO2 - id_polluant 123
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES3 <- read.csv2("dumpCH4eqCO2.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES3)
airPacaGES <-select(airPacaGES3,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################################################################################################
# GES : PRG100 - id_polluant 128
setwd("F:/Flexgrid/Sprint1/R/AIRPACA")
airPacaGES4 <- read.csv2("dumpPRG100.csv", dec= ".", sep=",",encoding = "UTF-8")
summary(airPacaGES4)
airPacaGES <-select(airPacaGES4,c("id_polluant","an","id_comm","id_secten1","code_cat_energie","val","ss","ss_epci"))
summary(airPacaGES)
##################)
###################################################################
#TRAITEMENT DES VARIABLES FACTOR : secten1, catégorie d'energie
##################################################################
#Mettre ces variables numériques => factor et recoder les modalites
table(airPacaGES$id_polluant)
airPacaGES$id_polluant <- as.factor(airPacaGES$id_polluant)
#levels(airPacaGES$id_polluant)<-c("CO2")
levels(airPacaGES$id_polluant)<-c("N2OeqCO2")
levels(airPacaGES$id_polluant)<-c("CH4eqCO2")
#levels(airPacaGES$id_polluant)<-c("PRG100")
#id_secten1 : 8 modalités 
#1:Energie, 2:Industrie/déchets, 3: Résidentiel, 4: Tertiaire
#5:Agriculture, 6:Transports routiers 7: Autres transports, 8: Non inclus
#setModalites <- function(airPACAGES) {
table(airPacaGES$id_secten1)
# 1      2      3      4      5      6      7     8
# 253  19623  38955 251842  19246   5670   1319   1359
# 2015  19716  38955 246136  18515   6954   1319   2187 
# 2180  25681 101899 425749  28742   7384   2356   2187 
airPacaGES$id_secten1 <- as.factor(airPacaGES$id_secten1)
str(airPacaGES$id_secten1)
levels(airPacaGES$id_secten1)<-c("Extraction","Industrie","Résidentiel","Tertiaire","Agriculture","Transport routier","Transports autres", "Non inclus")
table(airPacaGES$id_secten1)

#code_cat_energie : 9 modalités 
#0: Aucune Energie 1:Gaz Naturel, 2:Produits pétroliers, 3: Combustibles Minéraux Solides, 
#4: Bois-énergie, 5:Autres EnR, 6:Chaleur et froid 7:Autres non EnR, 8: Electricité
table(airPacaGES$code_cat_energie)
#0      1     2      3      4      5      6      7      8 
#33344  56218 170894  7  89588     67    840    132 231533 #### CO2 de 0 à 8
#40982  56308 170965  8  89595   5808    840    139 231533 #### PRG100 de 0 à 8
#0      1      2      3      4      5      7 
#16980  54957 170891  7  89591     5775   66               #### N2OeqCO2 de 0-1-2-3-4-5-7 
#13223  56252 170897  2  89592     5783   48               #### CH4eqCO2 de 0-1-2-3-4-5-7
airPacaGES$code_cat_energie <- as.factor(airPacaGES$code_cat_energie)
str(airPacaGES$code_cat_energie)
#CO2 de 0 à 8 et PRG100 de 0 à 8
levels(airPacaGES$code_cat_energie)<-c("Aucune énergie", "Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Chaleur et froid","Autres non EnR","Electricité")
# N2OeqCO2 et CH4eqCO2 de 0-1-2-3-4-5-7
levels(airPacaGES$code_cat_energie)<-c("Aucune énergie", "Gaz Naturel","Produits pétroliers","Combustibles Minéraux Solides","Bois-énergie","Autres EnR","Autres non EnR")

#l'unite id_unite=21005 cad kg 
#on laisse en kg
#changement de nom des colonnes
colnames(airPacaGES)
airPacaGES <- rename(airPacaGES,codeCommune = id_comm)
airPacaGES <- rename(airPacaGES,activite = id_secten1)
airPacaGES <- rename(airPacaGES,energie = code_cat_energie)
airPacaGES <- rename(airPacaGES,date = an)
airPacaGES <- rename(airPacaGES,typeGES = id_polluant)
airPacaGES <- rename(airPacaGES,emissionkg = val)
airPacaGES <- rename(airPacaGES,ssEpci = ss_epci)
head(airPacaGES)

#ajouter le referentiel
comRef <- read.csv2("F:/Flexgrid/Sprint1/R/REF/referentielCommunes.csv",encoding = "UTF-8")
airPacaGES <- addRef(airPacaGES, comRef, "codeCommune")
summary(airPacaGES)
#VERIFIER qu'il n'y a pas de NA dans les codes EPCI
#airPacaGES$codeEPCI <- replace(airPacaGES$codeEPCI,is.na(airPacaGES$codeEPCI),0)
head(airPacaGES)

#ecrire le fichier traite
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESN2OeqCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESCH4eqCO2.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
write.csv2(airPacaGES, file="F:/Flexgrid/Sprint1/airPacaGESPRG100.csv", row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)

#SI IL Y A DES NA DANS LES CODES EPCI
PBairPacaGES <-select(airPacaGES, c(codeEPCI,codeCommune))
PBairPacaGES <-filter(PBairPacaGES, is.na(codeEPCI))
doublons <- which(duplicated(PBairPacaGES$codeCommune))
pbcommunes<-PBairPacaGES[-doublons,]
airPacaGES$codeEPCI <- replace(airPacaGES$codeEPCI,is.na(airPacaGES$codeEPCI),0)
#communes pas dans un EPCI ? 5002, 5005, 5020, 5069, 4100
