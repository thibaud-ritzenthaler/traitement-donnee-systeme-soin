library(tidyverse)
library(questionr)
library(readr)
library(scales)

setwd("C:/Users/abdel/Desktop/Cours Master/Git_dossier/traitement-donnee-systeme-soin/data/processed")
#setwd("C:/Users/tibo/Documents/demographie/traitement-donnee-systeme-soin/data/processed")

red_a <- "#A63D40"
green_a <- "#90A959"
yellow_a <- "#E9B872"

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
Danemark<- Danemark %>% mutate(Age=ANN_REC-ANAI)
Sante_At <- table(Danemark$Age)
View(Sante_At)                   
                   
Handi_At <- table(Autriche$HANDI , Autriche$ANN_REC)
View(Handi_At)

Chronique_At <- table(Autriche$MALADIE_CHRO , Autriche$ANREC)
View(Chronique_At)

Non_medic_At <- table(Autriche$UNMET_MED) ## Ici il y a assez peu de non recours ~250 par annnées
View(Non_medic_At)

Raison_medic_At <- table(Autriche$UNMET_MED_REAS , Autriche$ANREC) ## Pareil, très peu de réponses
View(Raison_medic_At)

Non_dent_At<- table(Autriche$UNMET_DENT , Autriche$ANREC) ## Déjà un peu plus de réponses, ça peut être intéressant (+ Augmentation durant le temps)
View(Non_dent_At)

Raison_dent_At <-table(Autriche$UNMET_DENT_REAS , Autriche$ANREC)
View(Raison_dent_At)

## La même pour le Danemark

Sante_Dk <- table(Danemark$SEXE)
View(Sante_Dk)                   

Handi_Dk <- table(Danemark$HANDI , Danemark$ANREC)
View(Handi_Dk)

Chronique_Dk <- table(Danemark$MALADIE_CHRO , Danemark$ANREC)
View(Chronique_Dk)

Non_medic_Dk <- table(Danemark$UNMET_MED) ## Ici il y a assez peu de non recours ~250 par annnées
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
iorder()

## Réordonnancement de Sante_Dk$Sante_perc
Sante_Dk$Sante_perc <- Sante_Dk$Sante_perc %>%
  fct_relevel(
    "Bonne ou très bonne", "Ni bonne ni mauvaise", "Mauvaise ou très mauvaise"
  )

## Réordonnancement de Sante_At$Sante_perc
Sante_At$Sante_perc <- Sante_At$Sante_perc %>%
  fct_relevel(
    "Bonne ou très bonne", "Ni bonne ni mauvaise", "Mauvaise ou très mauvaise"
  )
## Intervalle de confiance 

### On trace les graphiques de variation de l'état de perçu 

ggplot(Sante_Dk, aes(x = Annee, y = Variation, color = Sante_perc, group = Sante_perc )) +
  geom_point(shape = 3, size = 3) +
  geom_line(size = 1.5)+
  scale_color_manual("Santé perçue", values=c(green_a, yellow_a, red_a))+
  xlab("Année") +
  ylab("Variation (points)") +
  theme_light() +
  theme(legend.position= c(.15, .9))

ggplot(Sante_At, aes(x = Annee, y = Variation, color = Sante_perc, group = Sante_perc )) +
  geom_point(shape = 3, size = 3) +
  geom_line(size = 1.5)+
  scale_color_manual("Santé perçue", values=c(green_a, yellow_a, red_a))+
  xlab("Année") +
  ylab("Variation (points)") +
  theme_light() +
  theme(legend.position= c(.15, .9))



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

Sante_join <- bind_rows(Autriche = Sante_At, Danemark = Sante_Dk, .id = "Pays")
Sante_At <- mutate(Sante_At, Pays = "Autriche")
Sante_Dk <- mutate(Sante_Dk, Pays = "Danemark")
ggplot() + 
  geom_line(data= Sante_At, aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc,linetype =Pays), size = 1.5) +
  geom_line(data= Sante_Dk, aes(x = Annee, y = Pourcentage, color = Sante_perc, group = Sante_perc,linetype =Pays), size = 1.5) +
  scale_y_continuous(n.breaks=14, labels = percent) +
  scale_color_manual("Santé perçue", values=c(green_a, yellow_a, red_a))+
  # scale_linetype_manual("Pays", values =c("dotdash", "dotted"))+
  xlab("Annee") +
  ylab("Effectifs") +
  theme_light()  +
  theme(legend.position= c(.1, .7))





Autriche_1=filter(Autriche,UNMET_MED==1)
freq.na(Autriche_1$UNMET_MED_REAS)

prop.table(wtd.table(Autriche$SANTE_PERC,Autriche$UNMET_MED,w=Autriche$Poids),margin=1)*100
prop.table(wtd.table(Danemark$SANTE_PERC,Danemark$UNMET_MED,w=Danemark$Poids),margin=1)*100


## Prendre d'autres caractéristiques
## Checker si c'est pertinent avec l'indice de confiance pour les tableaux Met / santé perçu 
## Voir les évolutions de manières annuelles
## Voir les structures de pop (sexe, age, diplome/statut, santé générale (maladie chro / handicap))

## Le faire en descriptif ( chi 2)
Autriche <- Autriche %>% filter(UNMET_MED %in% c(1,2))
Danemark <- Danemark %>% filter(UNMET_MED %in% c(1,2))

Model_Aut <- glm(relevel(as.factor(UNMET_MED),ref=2)~as.factor(ANN_REC),
                 family = binomial(logit),
                 data = Autriche)
odds.ratio(Model_Aut)

Model_Dk <- glm(relevel(as.factor(UNMET_MED),ref=2)~ as.factor(ANN_REC),
                 family = binomial(logit),
                 data = Danemark)
odds.ratio(Model_Dk)

Mod_Dk_2 <- glm(relevel(as.factor(UNMET_MED),ref=2)~as.factor(SEXE),
                 family = binomial(logit),
                 data = Danemark)
odds.ratio(Mod_Dk_2)
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

Diplo <- table(Autriche$STATUT_MAR)
View(Diplo)
## On voit pour l'autriche 

Model_Aut_2 <- glm(relevel(as.factor(UNMET_MED),ref=2)~ relevel(as.factor(DIPL),ref="Diplôme du supérieur"),
                   family = binomial(logit),
                   data = Autriche)
odds.ratio(Model_Aut_2)                                                        

## On fait pareil pour le Danemark 
Model_Dk_2 <- glm(relevel(as.factor(UNMET_MED),ref=2)~relevel(as.factor(DIPL),ref="Diplôme du supérieur"),
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

drop1(Mod_Aut_2, test= "Chisq")
drop1(Model_Dk_2, test="Chisq")
## Les changements pour UNMET en fonction de l'année sont significatifs mais pas pour le SEXE (0,10 pour Autriche et 0,8 pour le Danemark)

## Handicap
Autriche$HANDI[Autriche$HANDI %in% "1"] <- "Handicap lourd"
Autriche$HANDI[Autriche$HANDI %in% "2"] <- "Handicap léger"
Autriche$HANDI[Autriche$HANDI %in% "3"] <- "Pas de handicap"

Danemark$HANDI[Danemark$HANDI %in% "1"] <- "Handicap lourd"
Danemark$HANDI[Danemark$HANDI %in% "2"] <- "Handicap léger"
Danemark$HANDI[Danemark$HANDI %in% "3"] <- "Pas de handicap"


Mod_Aut_3 <- glm(relevel(as.factor(UNMET_MED),ref=2)~ ANREC + relevel(as.factor(HANDI),ref="Pas de handicap"),
                 family = binomial(logit), 
                 data=Danemark)
                 
odds.ratio(Mod_Aut_3)
drop1(Mod_Aut_3,test="Chisq")


## On fait le statut marital
Autriche$STATUT_MAR[Autriche$STATUT_MAR %in% "1"] <- "Celib"
Autriche$STATUT_MAR[Autriche$STATUT_MAR %in% "2"] <- "Marié"
Autriche$STATUT_MAR[Autriche$STATUT_MAR %in% c("3","4","5")] <- "Divorcé/Veuf"

Danemark$STATUT_MAR[Danemark$STATUT_MAR %in% "1"] <- "Celib"
Danemark$STATUT_MAR[Danemark$STATUT_MAR %in% "2"] <- "Marié"
Danemark$STATUT_MAR[Danemark$STATUT_MAR %in% c("3","4","5")] <- "Divorcé/Veuf"


Mod_Aut_4 <- glm(relevel(as.factor(UNMET_MED),ref=2)~ relevel(as.factor(STATUT_MAR),ref="Marié")+SEXE,
                 family = binomial(logit), 
                 data=Danemark)
odds.ratio(Mod_Aut_4)
drop1(Mod_Aut_4,test="Chisq")

## ACP si ça marche

Autriche <- Autriche %>% filter(UNMET_MED %in% c("1","2") & ANN_REC =="2009") 
Danemark <- Danemark %>% filter(UNMET_MED %in% c("1","2") & ANN_REC =="2009")
Test<- Danemark %>% column_to_rownames(var="ID")

ACP <- PCA(Test,scale.unit=TRUE)

##On va essayer de caractériser les joueurs en fonction de leur capacités physiques et sportives : est ce que certaines caractéristiques sont 
## liées et existe t-il différents groupes de joueurs? 

## Matrice des corrélations 
round(cor(R),digits=2)

##Tableau des valeurs propres
get_eig(ACP)

##Représentation des valeurs propres
fviz_eig(ACP,
         addlabels=T)
##Taux d'inertie moyen : le nombre d'axe à conserver :ceux qui sont au dessus de 16,67
100/6