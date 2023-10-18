library(tidyverse)
library(questionr)
library(ggrepel)
library(scales)

# setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE4 - Traitement de donnees/traitement-donnee-systeme-soin/code")
# setwd("C:/Users/tibo/Documents/demographie/traitement-donnee-systeme-soin/code")

red_a <- "#A63D40"
green_a <- "#90A959"
yellow_a <- "#E9B872"

DK_people <- read_csv("../data/processed/DK_people.csv")
DK_household <- read_csv("../data/processed/DK_household.csv")
AT_people <- read_csv("../data/processed/AT_people.csv")
AT_household <- read_csv("../data/processed/AT_household.csv")

DK_people_agregate_PH040_unmet <- DK_people %>% filter(PH040 == 1) %>%
  group_by(ANREC, RB050) %>% summarise(nb = n()) %>% mutate(nb_pond = RB050*nb) %>%
  ungroup() %>% group_by(ANREC) %>% summarise(POND_NB_UNMET = sum(nb_pond))
DK_people_agregate_PH040_met <- DK_people %>% filter(PH040 == 2) %>%
  group_by(ANREC, RB050) %>% summarise(nb = n()) %>% mutate(nb_pond = RB050*nb) %>%
  ungroup() %>% group_by(ANREC) %>% summarise(POND_NB_MET = sum(nb_pond))

DK_people_agregate_PH040 <- DK_people_agregate_PH040_unmet %>%
  left_join(DK_people_agregate_PH040_met) %>%
  mutate(UNMET_RATE = POND_NB_UNMET/(POND_NB_UNMET + POND_NB_MET))

AT_people_agregate_PH040_unmet <- AT_people %>% filter(PH040 == 1) %>%
  group_by(ANREC, RB050) %>% summarise(nb = n()) %>% mutate(nb_pond = RB050*nb) %>%
  ungroup() %>% group_by(ANREC) %>% summarise(POND_NB_UNMET = sum(nb_pond))
AT_people_agregate_PH040_met <- AT_people %>% filter(PH040 == 2) %>%
  group_by(ANREC, RB050) %>% summarise(nb = n()) %>% mutate(nb_pond = RB050*nb) %>%
  ungroup() %>% group_by(ANREC) %>% summarise(POND_NB_MET = sum(nb_pond))

AT_people_agregate_PH040 <- AT_people_agregate_PH040_unmet %>%
  left_join(AT_people_agregate_PH040_met) %>%
  mutate(UNMET_RATE = POND_NB_UNMET/(POND_NB_UNMET + POND_NB_MET))

people_agregate_PH040 <- bind_rows(DK = DK_people_agregate_PH040, AT = AT_people_agregate_PH040, .id = "COUNTRY") %>%
  mutate(LABEL = case_when(ANREC == 2010 & COUNTRY == "DK" ~ "Danemark", ANREC == 2010 & COUNTRY == "AT" ~ "Autriche", TRUE ~ NA_character_)) %>%
  mutate(CONDIDENCE_INTERVAL_MIN = POND_NB_UNMET - (POND_NB_UNMET * 0.05)) %>%
  mutate(CONDIDENCE_INTERVAL_MAX = POND_NB_UNMET + (POND_NB_UNMET * 0.05)) %>%
  mutate(CONDIDENCE_INTERVAL_MIN_RATE = CONDIDENCE_INTERVAL_MIN/ (POND_NB_UNMET + POND_NB_MET)) %>%
  mutate(CONDIDENCE_INTERVAL_MAX_RATE = CONDIDENCE_INTERVAL_MAX/ (POND_NB_UNMET + POND_NB_MET))


# PLOTS
ggplot(people_agregate_PH040) +
  geom_line(aes(x = ANREC, y = UNMET_RATE, color= COUNTRY, linetype = COUNTRY)) +
  scale_color_manual("Countries", values=c(red_a, green_a))+
  geom_label_repel(aes(x = ANREC, y = UNMET_RATE, label = LABEL),
    parse = TRUE,
    na.rm = TRUE,
    nudge_x = 0.5,
    nudge_y = 0.005,
    min.segment.length = 0
  ) +
  geom_ribbon(aes(x = ANREC, y = UNMET_RATE,ymin=CONDIDENCE_INTERVAL_MIN_RATE,
                  ymax=CONDIDENCE_INTERVAL_MAX_RATE, fill = COUNTRY), linetype="dotted", alpha=0.1) +
  scale_y_continuous(labels = percent) +
  expand_limits(x=c(2006,2013), y=c(0, 0.08)) +
  xlab("Ann\u00e9e") +
  ylab("Taux de soin non-satisfait (\u0025)") +
  theme_light() +
  theme(legend.position= "none")

ggplot(people_agregate_PH040) +
  geom_line(aes(x = ANREC, y = POND_NB_UNMET, linetype= COUNTRY), color = yellow_a) +
  geom_line(aes(x = ANREC, y = POND_NB_MET/10, linetype= COUNTRY), color  = red_a) +
  scale_y_continuous(name = "Besoins m\u00e9dicaux non-satisfaits",
                     labels = label_comma(big.mark = " "),
                     sec.axis = sec_axis( trans=~.*10,
                                          name="Besoins m\u00e9dicaux satisfaits",
                                          labels = label_comma(big.mark = " "))) +
  xlab("Ann\u00e9e") +
  theme_light() +
  theme(
    axis.title.y = element_text(color = yellow_a, size=13),
    axis.title.y.right = element_text(color = red_a, size=13))


