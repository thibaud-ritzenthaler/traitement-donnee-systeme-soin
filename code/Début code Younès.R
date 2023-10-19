library(tidyverse)
library(questionr)
library(ggplot)
library(readr)

setwd("C:/Users/abdel/Desktop/Cours Master/Git_dossier/traitement-donnee-systeme-soin/data/processed")

## On charge la base de l'Autriche et Danemark

Autriche <- read_csv("AT_people.csv")
Danemark <- read_csv("DK_people.csv")

## Je renomme les variables car sinon je les comprends pas du tout ##

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
                   ANREC = ANREC)

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
                   ANREC = ANREC)

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
