library(tidyverse)
library(questionr)
library(ggplot2)
library(readr)
library(scales)

setwd("C:/Users/abdel/Desktop/Cours Master/Git_dossier/traitement-donnee-systeme-soin/data/processed")

## On charge la base de l'Autriche et Danemark

Autriche <- read_csv("AT_people.csv")
Danemark <- read_csv("DK_people.csv")

## Je renomme les variables car sinon je les comprends pas du tout ##
## PL040 : Stattut 

Autriche <- rename(Autriche,
                   ID = RB030,
                   Poids = RB050,
                   SEXE = RB090,
                   ANAI = RB080,
                   ANN_REC = PB010,
                   STATUT_MAR = PB190,
                   SANTE_PERC = PH010,
                   MALADIE_CHRO =PH020,
                   HANDI = PH030,
                   UNMET_MED = PH040,
                   UNMET_MED_REAS = PH050,
                   UNMET_DENT = PH060,
                   UNMET_DENT_REAS = PH070,
                   ANREC = ANREC,
                   DIPL = PE040)

Danemark <- rename(Danemark,
                   ID = RB030,
                   Poids = RB050,
                   SEXE = RB090,
                   ANAI = RB080,
                   ANN_REC = PB010,
                   STATUT_MAR = PB190,
                   SANTE_PERC = PH010,
                   MALADIE_CHRO =PH020,
                   HANDI = PH030,
                   UNMET_MED = PH040,
                   UNMET_MED_REAS = PH050,
                   UNMET_DENT = PH060,
                   UNMET_DENT_REAS = PH070,
                   ANREC = ANREC,
                   DIPL = PE040)

## On regarde la distribution des variables en fonction de l'année de réponse

Sante_At <- table(Autriche$SANTE_PERC , Autriche$ANREC)
View(Sante_At)                   
                   
Handi_At <- table(Autriche$HANDI , Autriche$ANREC)
View(Handi_At)

Chronique_At <- table(Autriche$MALADIE_CHRO , Autriche$ANREC)
View(Chronique_At)

Non_medic_At <- table(Autriche$UNMET_MED , Autriche$ANREC) ## Ici il y a assez peu de non recours ~250 par annnées
View(Non_medic_At)

Raison_medic_At <- table(Autriche$UNMET_MED_REAS , Autriche$ANREC) ## Pareil, très peu de réponses
View(Raison_medic_At)

Non_dent_At<- table(Autriche$UNMET_DENT , Autriche$ANREC) ## Déjà un peu plus de réponses, ça peut être intéressant (+ Augmentation durant le temps)
View(Non_dent_At)

Raison_dent_At <-table(Autriche$UNMET_DENT_REAS , Autriche$ANREC)
View(Raison_dent_At)

## La même pour le Danemark

Sante_Dk <- table(Danemark$SANTE_PERC , Danemark$ANREC)
View(Sante_Dk)                   

Handi_Dk <- table(Danemark$HANDI , Danemark$ANREC)
View(Handi_Dk)

Chronique_Dk <- table(Danemark$MALADIE_CHRO , Danemark$ANREC)
View(Chronique_Dk)

Non_medic_Dk <- table(Danemark$UNMET_MED , Danemark$ANREC) ## Ici il y a assez peu de non recours ~250 par annnées
View(Non_medic_Dk)

Raison_medic_Dk <- table(Danemark$UNMET_MED_REAS , Danemark$ANREC) ## Pareil, très peu de réponses
View(Raison_medic_Dk)

Non_dent_Dk<- table(Danemark$UNMET_DENT , Danemark$ANREC) ## Déjà un peu plus de réponses, ça peut être intéressant (+ Augmentation durant le temps)
View(Non_dent_Dk)

Raison_dent_Dk <-table(Danemark$UNMET_DENT_REAS , Danemark$ANREC)
View(Raison_dent_Dk)
## On peut potentiellement partir sur la dégradation de la perception de santé dans les pop en fonction du handicap (ou non), 
## de l'existence d'une maladie chronique et potentiellement lier cela avec les non-recours ## 


## Réalisation d'un modèle linéaire pour expliquer l'état de santé perçu ## 

## On crée une table regroupant l'état de santé en 3 modalités "Bonne, Très bonne" (1),"Neutre" et "Mauvaise, très mauvaise" (2) 

Autriche$SANTE_PERC[Autriche$SANTE_PERC %in% c("1","2")] <- "Bonne ou très bonne"
Autriche$SANTE_PERC[Autriche$SANTE_PERC %in% c("4","5")] <- "Mauvaise ou très mauvaise"
Autriche$SANTE_PERC[Autriche$SANTE_PERC %in% "3"] <- "Ni bonne ni mauvaise"

Danemark$SANTE_PERC[Danemark$SANTE_PERC %in% c("1","2")] <- "Bonne ou très bonne"
Danemark$SANTE_PERC[Danemark$SANTE_PERC %in% c("4","5")] <- "Mauvaise ou très mauvaise"
Danemark$SANTE_PERC[Danemark$SANTE_PERC %in% "3"] <- "Ni bonne ni mauvaise"

#Valeurs manquantes
freq.na(Danemark$SANTE_PERC)
freq.na(Autriche$SANTE_PERC)

#Voir les différentes perceptions selon les années
Sante_Dk <- as.data.frame(wtd.table(Danemark$SANTE_PERC , Danemark$ANREC,w=Danemark$Poids))
View(Sante_Dk)    
Sante_At <- as.data.frame(wtd.table(Autriche$SANTE_PERC , Autriche$ANREC,w=Autriche$Poids))
View(Sante_At)    

#On renomme les variables
Sante_Dk <- rename(Sante_Dk, "Sante_perc"="Var1","Annee"="Var2","Effectifs_pond"="Freq")
Sante_At <- rename(Sante_At, "Sante_perc"="Var1","Annee"="Var2","Effectifs_pond"="Freq")



### On créer une variable "Variation" pour faire le graphique comparatif (2006 = Base 100)

Sante_Dk <- Sante_Dk %>% group_by(Sante_perc) %>% mutate(Variation = ifelse(Annee == 2006, 100 ,(100 +((Effectifs_pond-Effectifs_pond[1]))/(Effectifs_pond[1])*100)))

Sante_At <- Sante_At %>% group_by(Sante_perc) %>% mutate(Variation = ifelse(Annee == 2006, 100 ,(100+((Effectifs_pond-Effectifs_pond[1]))/(Effectifs_pond[1])*100))) 

## Intervalle de confiance 

### On trace les graphiques de variation de l'état de perçu 

ggplot(Sante_Dk, aes(x = Annee, y = Variation, color = Sante_perc, group = Sante_perc )) +
  geom_point() +
  geom_line()+
  xlab("Annee") +
  ylab("Variation") +
  theme_light() #+
  #theme(legend.position= "none")

ggplot(Sante_At, aes(x = Annee, y = Variation, color = Sante_perc, group = Sante_perc )) +
  geom_point() +
  geom_line()+
  xlab("Annee") +
  ylab("Variation") +
  theme_light() #+
#theme(legend.position= "none")



## On refait le même graphique avec les pourcentages 

Sante_Dk <- Sante_Dk %>% group_by(Annee) %>% mutate(Pourcentage = Effectifs_pond/sum(Effectifs_pond))

Sante_At <- Sante_At %>% group_by(Annee) %>% mutate(Pourcentage = Effectifs_pond/sum(Effectifs_pond))

## Danemark en effectifs pondérés

ggplot(Sante_Dk, aes(x = Annee, y = Effectifs_pond, color = Sante_perc, group = Sante_perc )) +
  geom_point() +
  geom_line()+
  xlab("Annee") +
  ylab("Effectifs") +
  theme_light() #+
#theme(legend.position= "none")

## Autriche en effectifs pondérés

ggplot(Sante_At, aes(x = Annee, y = Effectifs_pond, color = Sante_perc, group = Sante_perc )) +
  geom_point() +
  geom_line()+
  xlab("Annee") +
  ylab("Effectifs") +
  theme_light() #+


ggplot() + 
  geom_point(data= Sante_At,aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc )) +
  geom_point(data= Sante_Dk,aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc )) +
  geom_point() +
  geom_line(data= Sante_At,aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc,linetype ="dotdash"), size = 1.5) +
  geom_line(data= Sante_Dk,aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc,linetype ="dotted"), size = 1.5) +
  scale_y_continuous(labels = percent) +
  xlab("Annee") +
  ylab("Effectifs") +
  theme_light() #+






Autriche_1=filter(Autriche,UNMET_MED==1)
freq.na(Autriche_1$UNMET_MED_REAS)

prop.table(wtd.table(Autriche$SANTE_PERC,Autriche$UNMET_MED,w=Autriche$Poids),margin=1)*100
prop.table(wtd.table(Danemark$SANTE_PERC,Danemark$UNMET_MED,w=Danemark$Poids),margin=1)*100


## Prendre d'autres caractéristiques
## Checker si c'est pertinent avec l'indice de confiance pour les tableaux Met / santé perçu 
## Voir les évolutions de manières annuelles
## Voir les structures de pop (sexe, age, diplome/statut, santé générale (maladie chro / handicap))

## Le faire en descriptif ( chi 2)


Model_Aut <- glm(relevel(as.factor(UNMET_MED),ref=2)~(relevel(as.factor(SEXE),ref=1))+as.factor(ANN_REC),
                 family = binomial(logit),
                 data = Autriche)
odds.ratio(Model_Aut)

Model_Dk <- glm(relevel(as.factor(UNMET_MED),ref=2)~(relevel(as.factor(SEXE),ref=1))+ as.factor(ANN_REC),
                 family = binomial(logit),
                 data = Danemark)
odds.ratio(Model_Dk)

## Années confondues
## Pour l'Autriche, il y'a 11 % de chances de se tromper en affirmant que les femmes ont 1,07 fois plus de chances 
## de ne pas avoir recours à un soin médical comparé aux hommes. Pour le Danemark ces probas sont identiques (non significatives)



## On regarde en fonction du niveau de diplôme pour l'Autriche 

## On regroupe les modalités DIPL
Autriche$DIPL[Autriche$DIPL %in% c("0","1","2")] <- "Collège ou avant"
Autriche$DIPL[Autriche$DIPL %in% c("3","4")] <- "Lycée"
Autriche$DIPL[Autriche$DIPL %in% c("5","6")] <- "Diplôme du supérieur"

Danemark$DIPL[Danemark$DIPL %in% c("0","1","2")] <- "Collège ou avant"
Danemark$DIPL[Danemark$DIPL %in% c("3","4")] <- "Lycée"
Danemark$DIPL[Danemark$DIPL %in% c("5","6")] <- "Diplôme du supérieur"



## On voit pour l'autriche 

Model_Aut_2 <- glm(relevel(as.factor(UNMET_MED),ref=2)~as.factor(ANN_REC)+DIPL,
                   family = binomial(logit),
                   data = Autriche)
odds.ratio(Model_Aut_2)                                                        

## On fait pareil pour le Danemark 
Model_Dk_2 <- glm(relevel(as.factor(UNMET_MED),ref=2)~as.factor(ANN_REC)+DIPL,
                   family = binomial(logit),
                   data = Danemark)
odds.ratio(Model_Dk_2)               


## Stats descriptive
Sante_Dk <- as.data.frame( wtd.table(Danemark$SANTE_PERC , Danemark$ANREC,w=Danemark$Poids))
View(Sante_Dk)    
Sante_At <- as.data.frame( wtd.table(Autriche$SANTE_PERC , Autriche$ANREC,w=Autriche$Poids))
View(Sante_At)   

Dan <- as.data.frame( wtd.table(Danemark$SANTE_PERC, Danemark$UNMET_MED, Danemark$DIPL, w=Danemark$Poids))
Dan <- rename(Dan, "Sante_perc"="Var1","UNMET_MED"="Var2","Effectifs_pond"="Freq")
View(Dan)


Aut <- as.data.frame( wtd.table( Autriche$SANTE_PERC, Autriche$UNMET_MED, Autriche$DIPL, w=Autriche$Poids))
Aut <- rename(Aut, "Sante_perc"="Var1","UNMET_MED"="Var2","Effectifs_pond"="Freq")

## Test du chi 2 
drop1(Model_Aut, test="Chisq")
drop1(Model_Dk, test="Chisq")

drop1(Model_Aut_2, test= "Chisq")
drop1(Model_Dk_2, test="Chisq")
## Les changements pour UNMET en fonction de l'année sont significatifs mais pas pour le SEXE (0,10 pour Autriche et 0,8 pour le Danemark)