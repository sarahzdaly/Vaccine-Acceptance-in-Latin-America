### The shot, the message, and the messenger - Replication File for Argote et al. 2021b ####
library(devtools)
library(car)
library(estimatr)
library(haven)
library(lfe)
library(tidyverse)
library(stargazer)
library(coefplot)

#### Analysis of Conjoint Experiment #### 
hesitancy_long <- read_dta("vaccine_long.dta")  
source("00_table_functions_conjoint_all.R")

hesitancy_long$quickly_post_1_text_reversed <- hesitancy_long$months_vaccine_reversed

#### Cleaning ####

`%notin%` <- Negate(`%in%`)

hesitancy_long$distrib_health <- ifelse(hesitancy_long$distribution_ %in% c("el sistema nacional de salud", "o sistema nacional de saúde"), 1, 0)

hesitancy_long$distrib_army <- ifelse(hesitancy_long$distribution_ %in% c("el ejército", "os militares"), 1, 0)

hesitancy_long$distrib_ngo <- ifelse(hesitancy_long$distribution_ %in% c("una organización de la sociedad civil",  "uma organização de sociedade civil"), 1, 0)

# Endorser Indicators
hesitancy_long$endorse_mayor <- ifelse(hesitancy_long$endorser_  %in% c("el alcalde de su comuna", "el alcalde de su municipio", "el intendente de su municipio", "o prefeito da sua prefeitura"), 1, 0) 

hesitancy_long$endorse_pres <- ifelse(hesitancy_long$endorser_  %in% c("o Presidente Bolsonaro", "el Presidente Piñera", "el Presidente Sagasti", "el Presidente Duque", "el Presidente López Obrador", "el Presidente Fernández"), 1, 0) 

hesitancy_long$endorse_medic <- ifelse(hesitancy_long$endorser_ %in% c("o Conselho Federal de Medicina", "la Asociación Médica Argentina", "la Academia Nacional de Medicina", "la Federación Médica Colombiana", "el Colegio Médico del Perú", "el Colegio Médico de Chile"), 1, 0)

hesitancy_long$endorse_relig <- ifelse(hesitancy_long$endorser_ %in% c(" a Aliança Cristã Evangélica Brasileira", "o Cardeal Sérgio da Rocha", "Alianza Cristiana de Iglesias Evangélicas de la República de Argentina", "el Arzobispo Carlos Aguiar Retes", "el Arzobispo Carlos Castillo Mattasoglio", "el Arzobispo Celestino Aós",  "el Arzobispo Luis José Rueda", "el Cardenal Mario Aurelio Pol", "la Confederación Evangélica de Colombia", "la Confraternidad Evangélica de Mexico", "Mesa Ampliada Unión Nacional Evangélica",  "la Unión Nacional de Iglesias Cristianas Evangélicas del Perú"), 1, 0) 

hesitancy_long$endorse_leftnews <- ifelse(hesitancy_long$endorser_ %in% c("eel diario El Clar?n", "o jornal Folha de São Paulo", "el periódico La Jornada", "el periódico La República", "el diario La Tercera", "el periódico El Espectador"), 1, 0)

hesitancy_long$endorse_rightnews <- ifelse(hesitancy_long$endorser_ %in% c("el diario La Nación", "o jornal O Globo", "el periódico Reforma", "el periódico El Comercio", "el diario El Mercurio", "el periódico El Tiempo"), 1, 0)

hesitancy_long$endorse_political <- ifelse(hesitancy_long$endorser_  %in% c("o Presidente Bolsonaro", "el Presidente Piñera", "el Presidente Sagasti", "el Presidente Duque", "el Presidente López Obrador", "el Presidente Fernández", "el alcalde de su comuna", "el alcalde de su municipio", "el intendente de su municipio", "o prefeito da sua prefeitura"), 1, 0) 

# Which Vaccine Indicators 
hesitancy_long$vaccine_astrazeneca <- ifelse(hesitancy_long$vaccine_ %in% c("a vacina produzida pela empresa farmacêutica AstraZeneca e a Universidade de Oxford com sede no Reino Unido", "la vacuna producida por la farmacéutica AstraZeneca y la Universidad de Oxford en el Reino Unido"), 1, 0) 

hesitancy_long$vaccine_pfizer <- ifelse(hesitancy_long$vaccine_ %in% c("la vacuna producida por la farmacéutica Pfizer basada en los Estados Unidos", "a vacina produzida pela empresa farmacêutica Pfizer com sede nos Estados Unidos"), 1, 0)

hesitancy_long$vaccine_sinovac <- ifelse(hesitancy_long$vaccine_ %in% c("a vacina produzida pela empresa farmacêutica Sinovac com sede na China", "la vacuna producida por la farmacéutica Sinovac basada en China"), 1, 0)

hesitancy_long$vaccine_gamaleya <- ifelse(hesitancy_long$vaccine_ %in% c("a vacina produzida pela Gamaleya Research Instituto com sede na Rússia", "la vacuna producida por el Instituto Gamaleya de Investigación en Rusia"), 1, 0)

hesitancy_long$producers_western <- ifelse(hesitancy_long$vaccine_ %in% c("a vacina produzida pela empresa farmacêutica AstraZeneca e a Universidade de Oxford com sede no Reino Unido", "la vacuna producida por la farmacéutica AstraZeneca y la Universidad de Oxford en el Reino Unido", "la vacuna producida por la farmacéutica Pfizer basada en los Estados Unidos", "a vacina produzida pela empresa farmacêutica Pfizer com sede nos Estados Unidos"), 1, 0)

# Uptake Indicators 
hesitancy_long$uptake_1 <- ifelse(hesitancy_long$uptake_ %in% c("y hasta ahora 1% de personas en su comunidad se han vacunado.", "e, até agora, 1% da sua comunidade já tomou a vacina."), 1, 0)

hesitancy_long$uptake_25 <- ifelse(hesitancy_long$uptake_ %in% c("y hasta ahora 25% de personas en su comunidad se han vacunado.", "e, até agora, 25% da sua comunidade já tomou a vacina."), 1, 0)

hesitancy_long$uptake_50 <- ifelse(hesitancy_long$uptake_ %in% c("y hasta ahora 50% de personas en su comunidad se han vacunado.", "e, até agora, 50% da sua comunidade já tomou a vacina."), 1, 0)

hesitancy_long$uptake_75 <- ifelse(hesitancy_long$uptake_ %in% c("y hasta ahora 75% de personas en su comunidad se han vacunado.", "e, até agora, 75% da sua comunidade já tomou a vacina."), 1, 0)

# Uptake Efficacy Indicators  
hesitancy_long$efficacy_50 <- ifelse(hesitancy_long$efficacy_ %in% c(". Foi demonstrado que esta vacina previne 50% das infecções por COVID-19.", ". Se ha demostrado que esta vacuna previene el 50% de infecciones de COVID-19."), 1, 0)

hesitancy_long$efficacy_70 <- ifelse(hesitancy_long$efficacy_ %in% c(". Foi demonstrado que esta vacina previne 70% das infecções por COVID-19.", ". Se ha demostrado que esta vacuna previene el 70% de infecciones de COVID-19."), 1, 0)

hesitancy_long$efficacy_78 <- ifelse(hesitancy_long$efficacy_ %in% c(". Se ha demostrado que esta vacuna previene el 78% de infecciones de COVID-19."), 1, 0)

hesitancy_long$efficacy_91 <- ifelse(hesitancy_long$efficacy_ %in% c(". Foi demonstrado que esta vacina previne 91% das infecções por COVID-19.", ". Se ha demostrado que esta vacuna previene el 91% de infecciones de COVID-19."), 1, 0)

hesitancy_long$efficacy_95 <- ifelse(hesitancy_long$efficacy_ %in% c(". Foi demonstrado que esta vacina previne 95% das infecções por COVID-19.", ". Se ha demostrado que esta vacuna previene el 95% de infecciones de COVID-19."), 1, 0)

# Weights  
hesitancy_long$w_distribution  <- ifelse(hesitancy_long$distribution_ == "", 0, 3)

hesitancy_long$w_endorser <- ifelse(hesitancy_long$endorser_ == "", 0, 6)

hesitancy_long$w_vaccine <- ifelse(hesitancy_long$vaccine_ %in% c("uma vacuna", "una vacuna") & hesitancy_long$round == 1 , 2.3333, 
                                   ifelse(hesitancy_long$vaccine_ %notin% c("uma vacuna", "una vacuna") & hesitancy_long$round == 1 , 7, 4)) 

hesitancy_long$w_uptake <-  ifelse(hesitancy_long$uptake_ %in% c(".") & hesitancy_long$round == 1 , 2.3333, 
                                   ifelse(hesitancy_long$uptake_ %notin% c(".") & hesitancy_long$round == 1 , 7, 4)) 

hesitancy_long$w_efficacy <- ifelse(hesitancy_long$efficacy_ %in% c(".") & hesitancy_long$round == 1, 1.4, 
                                    ifelse(hesitancy_long$efficacy_ %notin% c(".") & hesitancy_long$round == 1 , 3.5, 2)) 

hesitancy_long$w_conjoint <- hesitancy_long$w_distribution * hesitancy_long$w_endorser * hesitancy_long$w_vaccine * hesitancy_long$w_uptake * hesitancy_long$w_efficacy 

hesitancy_long <- subset(hesitancy_long, sample_causal == 1 & !is.na(willing_vaccine))

## Clean Pre-treatment covars
hesitancy_long$vote_mayor_clean <- ifelse(hesitancy_long$vote_mayor == 0, 0, 
                                          ifelse(hesitancy_long$vote_mayor == 2, 0, 
                                                 ifelse(hesitancy_long$vote_mayor == 1, 1, 0)))  

hesitancy_long$vote_president_clean <- ifelse(hesitancy_long$vote_president == 0, 0, 
                                              ifelse(hesitancy_long$vote_president == 2, 0, 
                                                     ifelse(hesitancy_long$vote_president == 1, 1, 0)))  

## Create Relative Trust Scales 
hesitancy_long$trust_east <- (hesitancy_long$trust_country_govs_1 + hesitancy_long$trust_country_govs_5)/2 

hesitancy_long$trust_west <- (hesitancy_long$trust_country_govs_2 + hesitancy_long$trust_country_govs_3 + hesitancy_long$trust_country_govs_4)/3 

hesitancy_long$relative_trust_west <- hesitancy_long$trust_west - hesitancy_long$trust_east 

## Create News Index
hesitancy_long$covid_news_index <- hesitancy_long$covid_news_1 + hesitancy_long$covid_news_2  + hesitancy_long$covid_news_3  + hesitancy_long$covid_news_4  + hesitancy_long$covid_news_5  + hesitancy_long$covid_news_6 + hesitancy_long$covid_news_7

## Code ideology
hesitancy_long$ideology <- ifelse(hesitancy_long$left_right_scale_1 < 5, -1, 
                                  ifelse(hesitancy_long$left_right_scale_1 == 5, 0,
                                         ifelse(hesitancy_long$left_right_scale_1 > 5, 1, 0)))

# Create Trust Dummies 
hesitancy_long$trust_china_dummy <- ifelse(hesitancy_long$trust_country_govs_1 > 2, 1, 0)
hesitancy_long$trust_trump_dummy <- ifelse(hesitancy_long$trust_country_govs_2 > 2, 1, 0)
hesitancy_long$trust_biden_dummy <- ifelse(hesitancy_long$trust_country_govs_3 > 2, 1, 0)
hesitancy_long$trust_uk_dummy <- ifelse(hesitancy_long$trust_country_govs_4 > 2, 1, 0)
hesitancy_long$trust_russia_dummy <- ifelse(hesitancy_long$trust_country_govs_5 > 2, 1, 0)

# Gender
hesitancy_long$male <- ifelse(hesitancy_long$gender_0 == "man", 1, 0) 

## Remove don't know options or median

### Median

hesitancy_long <- hesitancy_long %>%
  mutate(covid_economic_situ = ifelse(covid_economic_situ == -8, median(covid_economic_situ), covid_economic_situ),
         gov_vaccine_priority = ifelse(gov_vaccine_priority == -8, median(gov_vaccine_priority), gov_vaccine_priority),
         vote_president = ifelse(vote_president == -8, 0, ifelse(vote_president == 2, 0, vote_president)),
         vote_mayor = ifelse(vote_mayor == -8, 0, ifelse(vote_mayor == 2, 0, vote_mayor)),
         trust_persons_insts_1 = ifelse(trust_persons_insts_1 == -8, median(trust_persons_insts_1), trust_persons_insts_1),
         trust_persons_insts_2 = ifelse(trust_persons_insts_2 == -8, median(trust_persons_insts_2), trust_persons_insts_2),
         trust_persons_insts_3 = ifelse(trust_persons_insts_3 == -8, median(trust_persons_insts_3), trust_persons_insts_3),
         trust_persons_insts_4 = ifelse(trust_persons_insts_4 == -8, median(trust_persons_insts_4), trust_persons_insts_4),
         trust_persons_insts_5 = ifelse(trust_persons_insts_5 == -8, median(trust_persons_insts_5), trust_persons_insts_5),
         trust_persons_insts_6 = ifelse(trust_persons_insts_6 == -8, median(trust_persons_insts_6), trust_persons_insts_6),
         trust_persons_insts_7 = ifelse(trust_persons_insts_7 == -8, median(trust_persons_insts_7), trust_persons_insts_7),
         trust_orgs_1 = ifelse(trust_orgs_1 == -8, median(trust_orgs_1), trust_orgs_1),
         trust_orgs_2 = ifelse(trust_orgs_2 == -8, median(trust_orgs_2), trust_orgs_2),
         trust_orgs_3 = ifelse(trust_orgs_3 == -8, median(trust_orgs_3), trust_orgs_3),
         trust_country_govs_1 = ifelse(trust_country_govs_1 == -8, median(trust_country_govs_1), trust_country_govs_1),
         trust_country_govs_2 = ifelse(trust_country_govs_2 == -8, median(trust_country_govs_2), trust_country_govs_2),
         trust_country_govs_3 = ifelse(trust_country_govs_3 == -8, median(trust_country_govs_3), trust_country_govs_3),
         trust_country_govs_4 = ifelse(trust_country_govs_4 == -8, median(trust_country_govs_4), trust_country_govs_4),
         trust_country_govs_5 = ifelse(trust_country_govs_5 == -8, median(trust_country_govs_5), trust_country_govs_5),
         indoor_contrib_covid = ifelse(indoor_contrib_covid == -8, median(indoor_contrib_covid), indoor_contrib_covid),
  )

## Remove or make 0 'don't know'

hesitancy_long_het <- hesitancy_long

hesitancy_long_het <- hesitancy_long_het %>%
  mutate(covid_economic_situ = ifelse(covid_economic_situ == -8, NA, covid_economic_situ),
         gov_vaccine_priority = ifelse(gov_vaccine_priority == -8, NA, gov_vaccine_priority),
         vote_president = ifelse(vote_president == -8, 0, ifelse(vote_president == 2, 0, vote_president)),
         vote_mayor = ifelse(vote_mayor == -8, 0, ifelse(vote_mayor == 2, 0, vote_mayor)),
         trust_persons_insts_1 = ifelse(trust_persons_insts_1 == -8, NA, trust_persons_insts_1),
         trust_persons_insts_2 = ifelse(trust_persons_insts_2 == -8, NA, trust_persons_insts_2),
         trust_persons_insts_3 = ifelse(trust_persons_insts_3 == -8, NA, trust_persons_insts_3),
         trust_persons_insts_4 = ifelse(trust_persons_insts_4 == -8, NA, trust_persons_insts_4),
         trust_persons_insts_5 = ifelse(trust_persons_insts_5 == -8, NA, trust_persons_insts_5),
         trust_persons_insts_6 = ifelse(trust_persons_insts_6 == -8, NA, trust_persons_insts_6),
         trust_persons_insts_7 = ifelse(trust_persons_insts_7 == -8, NA, trust_persons_insts_7),
         trust_orgs_1 = ifelse(trust_orgs_1 == -8, NA, trust_orgs_1),
         trust_orgs_2 = ifelse(trust_orgs_2 == -8, NA, trust_orgs_2),
         trust_orgs_3 = ifelse(trust_orgs_3 == -8, NA, trust_orgs_3),
         trust_country_govs_1 = ifelse(trust_country_govs_1 == -8, NA, trust_country_govs_1),
         trust_country_govs_2 = ifelse(trust_country_govs_2 == -8, NA, trust_country_govs_2),
         trust_country_govs_3 = ifelse(trust_country_govs_3 == -8, NA, trust_country_govs_3),
         trust_country_govs_4 = ifelse(trust_country_govs_4 == -8, NA, trust_country_govs_4),
         trust_country_govs_5 = ifelse(trust_country_govs_5 == -8, NA, trust_country_govs_5),
         indoor_contrib_covid = ifelse(indoor_contrib_covid == -8, NA, indoor_contrib_covid),
  )

hesitancy_long <- hesitancy_long_het 

hesitancy_long <- subset(hesitancy_long, hesitancy_pre > 0) 

#### Figure 1 and Figure 2 ####  
RHS_conjoint_full <- "distrib_ngo + distrib_army + endorse_relig + endorse_mayor + endorse_pres + endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"
 
conjoint_outcomes <- c("willing_vaccine", "quickly_post_1_text_reversed")

conjoint_full <- lapply(1:length(conjoint_outcomes), function(y)
  felm(as.formula(paste0(conjoint_outcomes[y], " ~ ",
                         RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
       data = hesitancy_long,
       weights = hesitancy_long$w_conjoint,
       cmethod = 'reghdfe')
)

predictornames_figure <- rev(c(efficacy_951 = "95% Efficacy",
                               efficacy_911 = "91% Efficacy",
                               efficacy_781 = "78% Efficacy",
                               efficacy_701 = "70% Efficacy",
                               efficacy_501 = "50% Efficacy",
                               uptake_751 = "75% Uptake",
                               uptake_501 = "50% Uptake",
                               uptake_251 = "25% Uptake",
                               uptake_11 = "1% Uptake",
                               vaccine_gamaleya1 = "Producer: Gamaleya",
                               vaccine_pfizer1 = "Producer: Pfizer",
                               vaccine_astrazeneca1 = "Producer: Astrazeneca",
                               vaccine_sinovac1 = "Producer: Sinovac",
                               endorse_leftnews1 = "Endorser: Left Newspaper",
                               endorse_rightnews1 = "Endorser: Right Newspaper",
                               endorse_pres1 = "Endorser: President",
                               endorse_mayor1 = "Endorser: Mayor",
                               endorse_relig1 = "Endorser: Religious Leader",
                               distrib_army1 = "Distributor: Armed Forces",
                               distrib_ngo1 = "Distributor: Civil Society"
))

conjoint_outcomes_label_long <- c("Willingness to Take this Vaccine",
                                  "Months Would Wait Before Taking this Vaccine (Rev)")

conjoint_main_plots <- lapply(1:length(conjoint_outcomes), function(y)
  coefplot(conjoint_full[[y]],
           sds = summary(conjoint_full[[y]], robust = TRUE)$coefficients[,'Cluster s.e.'],
           intercept = TRUE, color = "blue", factors = c("distrib_ngo", "distrib_army")) + 
    scale_y_discrete(labels = predictornames_figure) +
    ggtitle(conjoint_outcomes_label_long[y])
)

for (y in 1:length(conjoint_main_plots)) {
  pdf(paste0("main_conjoint_blue_0", 5+y, "_", conjoint_outcomes[y], ".pdf"))
  print(conjoint_main_plots[[y]])
  dev.off()
}

conjoint_main_plots <- lapply(1:length(conjoint_outcomes), function(y)
  coefplot(conjoint_full[[y]],
           sds = summary(conjoint_full[[y]], robust = TRUE)$coefficients[,'Cluster s.e.'],
           intercept = TRUE, color = "purple", factors = c("distrib_ngo", "distrib_army")) + 
    scale_y_discrete(labels = predictornames_figure) +
    ggtitle(conjoint_outcomes_label_long[y])
)

for (y in 1:length(conjoint_main_plots)) {
  pdf(paste0("main_conjoint_purple_0", 5+y, "_", conjoint_outcomes[y], ".pdf"))
  print(conjoint_main_plots[[y]])
  dev.off()
}


#### Figure 3 #### 
RHS_trust_conjoint <- "trust_orgs_3*distrib_ngo + trust_orgs_1*distrib_army + trust_persons_insts_7*endorse_relig + trust_persons_insts_2*endorse_mayor + trust_persons_insts_1*endorse_pres + trust_persons_insts_6*endorse_rightnews + trust_persons_insts_5*endorse_leftnews + trust_country_govs_1*vaccine_sinovac + trust_country_govs_4*vaccine_astrazeneca + trust_country_govs_2*vaccine_pfizer + trust_country_govs_3*vaccine_pfizer + trust_country_govs_5*vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"

conjoint_full_trust <- lapply(1:length(conjoint_outcomes), function(y)
  felm(as.formula(paste0(conjoint_outcomes[y], " ~ ",
                         RHS_trust_conjoint, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
       data = hesitancy_long,
       weights = hesitancy_long$w_conjoint,
       cmethod = 'reghdfe')
)


conjoint_outcomes_label_long <- c("Heterogeneous Effects on Willingness by Trust",
                                  "Heterogeneous Effects on Months to Vaccination by Trust")

conjoint_main_trust_plots <- lapply(1:length(conjoint_outcomes), function(y)
  coefplot(conjoint_full_trust[[y]], coefficients=c("trust_orgs_3:distrib_ngo",
                                                    "trust_orgs_1:distrib_army", "trust_persons_insts_7:endorse_relig", "trust_persons_insts_2:endorse_mayor", 
                                                    "trust_persons_insts_1:endorse_pres", "trust_persons_insts_6:endorse_rightnews", "trust_persons_insts_5:endorse_leftnews", "trust_country_govs_1:vaccine_sinovac", 
                                                    "trust_country_govs_4:vaccine_astrazeneca", "trust_country_govs_2:vaccine_pfizer", "vaccine_pfizer:trust_country_govs_3", "trust_country_govs_5:vaccine_gamaleya"),
           sds = summary(conjoint_full_trust[[y]], robust = TRUE)$coefficients[,'Cluster s.e.'],
           intercept = TRUE, color = "blue", ylab = "Coefficient on the Interaction Term") + 
    scale_y_discrete(labels = c("Civil Society x Trust", "Armed Forces x Trust", "Religious Leader x Trust","Mayor x Trust","President x Trust", "Right-Wing Newspaper x Trust",
                                "Left-Wing Newspaper x Trust", "Sinovac x Trust in China", "Astrazenica x Trust in UK", "Pfizer x Trust in Biden", "Pfizer x Trust in Trump",
                                "Gamaleya x Trust in Russia")) + 
    ggtitle(conjoint_outcomes_label_long[y])
)


for (y in 1:length(conjoint_main_trust_plots)) {
  pdf(paste0("main_conjoint_blue_0", 5+y, "_", conjoint_outcomes[y], ".pdf"))
  print(conjoint_main_trust_plots[[y]])
  dev.off()
}


conjoint_main_trust_plots <- lapply(1:length(conjoint_outcomes), function(y)
  coefplot(conjoint_full_trust[[y]], coefficients=c("trust_orgs_3:distrib_ngo",
                                                    "trust_orgs_1:distrib_army", "trust_persons_insts_7:endorse_relig", "trust_persons_insts_2:endorse_mayor", 
                                                    "trust_persons_insts_1:endorse_pres", "trust_persons_insts_6:endorse_rightnews", "trust_persons_insts_5:endorse_leftnews", "trust_country_govs_1:vaccine_sinovac", 
                                                    "trust_country_govs_4:vaccine_astrazeneca", "trust_country_govs_2:vaccine_pfizer", "vaccine_pfizer:trust_country_govs_3", "trust_country_govs_5:vaccine_gamaleya"),
           sds = summary(conjoint_full_trust[[y]], robust = TRUE)$coefficients[,'Cluster s.e.'],
           intercept = TRUE, color = "purple", ylab = "Coefficient on the Interaction Term") + 
    scale_y_discrete(labels = c("Civil Society x Trust", "Armed Forces x Trust", "Religious Leader x Trust","Mayor x Trust","President x Trust", "Right-Wing Newspaper x Trust",
                                "Left-Wing Newspaper x Trust", "Sinovac x Trust in China", "Astrazenica x Trust in UK", "Pfizer x Trust in Biden", "Pfizer x Trust in Trump",
                                "Gamaleya x Trust in Russia")) +
    ggtitle(conjoint_outcomes_label_long[y])
  
)


for (y in 1:length(conjoint_main_trust_plots)) {
  pdf(paste0("main_conjoint_purple_0", 5+y, "_", conjoint_outcomes[y], ".pdf"))
  print(conjoint_main_trust_plots[[y]])
  dev.off()
}



#### All Appendix Tables ####

## Table A2 
hesitancy_long <- subset(hesitancy_long, hesitancy_pre > 0) 
RHS_conjoint_balance <- "distrib_ngo + distrib_army + endorse_relig + endorse_mayor + endorse_pres + 
endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_gamaleya +
uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"

predictornames_balance <- c("Distributor: Civil Society",
                            "Distributor: Armed Forces",
                            "Endorser: Religious Leader",
                            "Endorser: Mayor",
                            "Endorser: President",
                            "Endorser: Right Newspaper",
                            "Endorser: Left Newspaper", 
                            "Producer: Sinovac", 
                            "Producer: Astrazeneca", 
                            "Producer: Pfizer",  
                            "Producer: Gamaleya",
                            "1\\% Uptake",
                            "25\\% Uptake",
                            "50\\% Uptake",
                            "75\\% Uptake", 
                            "Efficacy Concern", 
                            "50\\% Efficacy",
                            "70\\% Efficacy",
                            "78\\% Efficacy",
                            "91\\% Efficacy",
                            "95\\% Efficacy") 


conjoint_outcomes <- c("age_bin_num", "male", "education_enc", "hesitancy_pre", "std_months_pre")
RHS_list <- list(RHS_conjoint_balance)
rounds_list <- c("1", "1", "1", "1", "1") 
predictor_names_list <- list(predictornames_balance)
conjoint_outcomes_label <- c("Age Bin", "Gender", "Education", "Pre-Treatment Hesitancy", "Pre-Treatment Months")
conjoints_het_names <- c("_A2_balance")

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint_balance(RHS = RHS_list[[i]],
                      outcome_vars = conjoint_outcomes,
                      round = rounds_list[i],
                      outcome_labels = conjoint_outcomes_label,
                      treatment_labels = predictor_names_list[[i]],
                      table_name = paste0("table_", i, "_", conjoints_het_names[[i]]))
}

## Table A6 
RHS_trust_conjoint <- "trust_orgs_3*distrib_ngo + trust_orgs_1*distrib_army + trust_persons_insts_7*endorse_relig + trust_persons_insts_2*endorse_mayor + trust_persons_insts_1*endorse_pres + trust_persons_insts_6*endorse_rightnews + trust_persons_insts_5*endorse_leftnews + trust_country_govs_1*vaccine_sinovac + trust_country_govs_4*vaccine_astrazeneca + trust_country_govs_2*vaccine_pfizer + trust_country_govs_3*vaccine_pfizer + trust_country_govs_5*vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"

predictornames_trust <- c(  "Trust Civil Society",
                            "Distributor: Civil Society",
                            "Trust Armed Forces", 
                            "Distributor: Armed Forces",
                            "Trust Religious Leader", 
                            "Endorser: Religious Leader",
                            "Trust Mayor", 
                            "Endorser: Mayor",
                            "Trust President",
                            "Endorser: President",
                            "Trust R News", 
                            "Endorser: Right Newspaper",
                            "Trust L News", 
                            "Endorser: Left Newspaper", 
                            "Trust China", 
                            "Producer: Sinovac", 
                            "Trust UK", 
                            "Producer: Astrazeneca", 
                            "Trust US - Biden", 
                            "Producer: Pfizer", 
                            "Trust US - Trump",
                            "Trust Russia", 
                            "Producer: Gamaleya",
                            "1\\% Uptake",
                            "25\\% Uptake",
                            "50\\% Uptake",
                            "75\\% Uptake", 
                            "50\\% Efficacy",
                            "70\\% Efficacy",
                            "78\\% Efficacy",
                            "91\\% Efficacy",
                            "95\\% Efficacy",
                            "Distributor: Civil Society $\\times$ Trust",
                            "Distributor: Army $\\times$ Trust",
                            "Endorser: Religious Leader $\\times$ Trust", 
                            "Endorser: Mayor $\\times$ Trust", 
                            "Endorser: President $\\times$ Trust",
                            "Endorser: R News $\\times$ Trust",
                            "Endorser: L News $\\times$ Trust",
                            "Producer: Sinovac $\\times$ Trust China",
                            "Producer: Atrazeneca $\\times$ Trust UK",
                            "Producer: Pfizer $\\times$ Trust US - Trump",
                            "Producer: Pfizer $\\times$ Trust UK - Biden",
                            "Producer: Gamaleya $\\times$ Trust Russia") 


conjoint_outcomes <- c("willing_vaccine", "quickly_post_1_text_reversed")
conjoint_outcomes_label <- c("Willing", "Months (Rev)")

RHS_list <- list(RHS_trust_conjoint) 

rounds_list <- c("full") 

predictor_names_list <- list(predictornames_trust) 

conjoints_het_names <- c("A6_trust") 

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint(RHS = RHS_list[[i]],
                      outcome_vars = conjoint_outcomes,
                      round = rounds_list[i],
                      outcome_labels = conjoint_outcomes_label,
                      treatment_labels = predictor_names_list[[i]],
                      table_name = paste0("table_", i, "_", conjoints_het_names[[i]]))
}


## Tables A3, A5, A9, A7 
RHS_conjoint_basic <- "distrib_ngo + distrib_army + endorse_relig + endorse_mayor + endorse_pres + endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_pfizer + vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"
RHS_conjoint_education_enc <- "education_enc*distrib_ngo + education_enc*distrib_army + education_enc*endorse_relig + education_enc*endorse_mayor + education_enc*endorse_pres + education_enc*endorse_rightnews + education_enc*endorse_leftnews + education_enc*vaccine_sinovac + education_enc*vaccine_astrazeneca + education_enc*vaccine_pfizer  + education_enc*vaccine_gamaleya + education_enc*uptake_1 + education_enc*uptake_25 + education_enc*uptake_50 + education_enc*uptake_75 + education_enc*efficacy_50 + education_enc*efficacy_70 + education_enc*efficacy_78 + education_enc*efficacy_91 + education_enc*efficacy_95"
RHS_conjoint_copartisan <- "distrib_ngo + distrib_army + endorse_relig + vote_mayor*endorse_mayor + vote_president*endorse_pres + endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_pfizer + vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"

{
  
  predictornames_basic <- c("Distributor: Civil Society",
                            "Distributor: Armed Forces",
                            "Endorser: Religious Leader",
                            "Endorser: Mayor",
                            "Endorser: President",
                            "Endorser: Right Newspaper",
                            "Endorser: Left Newspaper", 
                            "Producer: Sinovac", 
                            "Producer: Astrazeneca", 
                            "Producer: Pfizer",  
                            "Producer: Gamaleya",
                            "1\\% Uptake",
                            "25\\% Uptake",
                            "50\\% Uptake",
                            "75\\% Uptake",
                            "50\\% Efficacy",
                            "70\\% Efficacy",
                            "78\\% Efficacy",
                            "91\\% Efficacy",
                            "95\\% Efficacy" 
  )


  predictornames_education <- c("Education", 
                              "Distributor: Civil Society",
                              "Distributor: Armed Forces",
                              "Endorser: Religious Leader",
                              "Endorser: Mayor",
                              "Endorser: President",
                              "Endorser: Right Newspaper",
                              "Endorser: Left Newspaper", 
                              "Producer: Sinovac", 
                              "Producer: Astrazeneca", 
                              "Producer: Pfizer",  
                              "Producer: Gamaleya",
                              "1\\% Uptake",
                              "25\\% Uptake",
                              "50\\% Uptake",
                              "75\\% Uptake",
                              "50\\% Efficacy",
                              "70\\% Efficacy",
                              "78\\% Efficacy",
                              "91\\% Efficacy",
                              "95\\% Efficacy", 
                              "Education x Dist.: Civil Society",
                              "Education x Dist.: Armed Forces",
                              "Education x End.: Religious Leader",
                              "Education x End.: Mayor",
                              "Education x End.: President",
                              "Education x End.: Right Newspaper",
                              "Education x End.: Left Newspaper", 
                              "Education x Prod.: Sinovac", 
                              "Education x Prod.: Astrazeneca", 
                              "Education x Prod.: Pfizer",  
                              "Education x Prod.: Gamaleya",
                              "Education x 1\\% Uptake",
                              "Education x 25\\% Uptake",
                              "Education x 50\\% Uptake",
                              "Education x 75\\% Uptake",
                              "Education x 50\\% Efficacy",
                              "Education x 70\\% Efficacy",
                              "Education x 78\\% Efficacy",
                              "Education x 91\\% Efficacy",
                              "Education x 95\\% Efficacy") 
  
  predictornames_vote        <- c("Distributor: Civil Society",
                                  "Distributor: Armed Forces",
                                  "Endorser: Religious Leader",
                                  "Vote Mayor",
                                  "Endorser: Mayor",
                                  "Vote President",
                                  "Endorser: President",
                                  "Endorser: Right Newspaper",
                                  "Endorser: Left Newspaper", 
                                  "Producer: Sinovac", 
                                  "Producer: Astrazeneca", 
                                  "Producer: Pfizer", 
                                  "Producer: Gamaleya",
                                  "1\\% Uptake",
                                  "25\\% Uptake",
                                  "50\\% Uptake",
                                  "75\\% Uptake",
                                  "50\\% Efficacy",
                                  "70\\% Efficacy",
                                  "78\\% Efficacy",
                                  "91\\% Efficacy",
                                  "95\\% Efficacy",
                                  "Vote Mayor $\\times$ Mayor Endorse",
                                  "Vote President $\\times$ President Endorse")

}


conjoint_outcomes <- c("willing_vaccine", "quickly_post_1_text_reversed", "conjoint_mechanism_prop", "conjoint_mechanism_sick", "conjoint_mechanism_harm", "conjoint_mechanism_help")
conjoint_outcomes_label <- c("Willing", "Months (Rev)", "Stop Propagation", "Not Get COVID", "Wouldn't Harm", "Gov Help")

RHS_list <- list(RHS_conjoint_basic, 
                 RHS_conjoint_basic, 
                 RHS_conjoint_education_enc,
                 RHS_conjoint_copartisan
                 
) 

rounds_list <- c("full", 
                 "1", 
                 "full", 
                 "full", ) 

predictor_names_list <- list(predictornames_basic,
                             predictornames_basic,
                             predictornames_education,
                             predictornames_vote
) 

conjoints_het_names <- c("A3_basic", "A5_firstround", "A7_copartisan", "A8_education") 

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint(RHS = RHS_list[[i]],
                      outcome_vars = conjoint_outcomes,
                      round = rounds_list[i],
                      outcome_labels = conjoint_outcomes_label,
                      treatment_labels = predictor_names_list[[i]],
                      table_name = paste0("table_", i, "_", conjoints_het_names[[i]]))
}

## Table A4 
RHS_list <- list(RHS_conjoint_basic)
rounds_list <- c("full") 
predictor_names_list <- list(predictornames_basic)
conjoints_het_names <- c("_A4_confidence_intervals") 

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint_ci(RHS = RHS_list[[i]],
                               outcome_vars = conjoint_outcomes,
                               round = rounds_list[i],
                               outcome_labels = conjoint_outcomes_label,
                               treatment_labels = predictor_names_list[[i]],
                               table_name = paste0("table_", i, "_", conjoints_het_names[[i]]))
}

## Table A8
catholic <- subset(hesitancy_long, religion == 1)
evangelical <- subset(hesitancy_long, religion == 3)
RHS_list <- list(RHS_conjoint_endorser_trust) 

rounds_list <- c("full") 

predictor_names_list <- list(predictornames_endorsers) 

conjoints_table_names <- c("catholic_subset") 

for (i in 1:length(conjoints_table_names)) {
  make_table_conjoint_cath(RHS = RHS_list[[i]],
                      outcome_vars = conjoint_outcomes,
                      round = rounds_list[i],
                      outcome_labels = conjoint_outcomes_label,
                      treatment_labels = predictor_names_list[[i]],
                      table_name = paste0("table_A7_", i, "_", conjoints_table_names[[i]]))
}

conjoints_table_names <- c("evangelical_subset") 

for (i in 1:length(conjoints_table_names)) {
  make_table_conjoint_evangelical(RHS = RHS_list[[i]],
                               outcome_vars = conjoint_outcomes,
                               round = rounds_list[i],
                               outcome_labels = conjoint_outcomes_label,
                               treatment_labels = predictor_names_list[[i]],
                               table_name = paste0("table_A7_", i, "_", conjoints_table_names[[i]]))
}

hesitancy_long$catholic <- ifelse(hesitancy_long$religion == 1, 1, 0)
hesitancy_long$evangelical <- ifelse(hesitancy_long$religion == 3, 1, 0)

RHS_co_relig_catholic <- "distrib_ngo + distrib_army  + catholic*endorse_relig + endorse_pres + endorse_mayor + endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"
RHS_co_relig_evangelical <- "distrib_ngo + distrib_army  + evangelical*endorse_relig + endorse_pres + endorse_mayor + endorse_rightnews + endorse_leftnews + vaccine_sinovac + vaccine_astrazeneca + vaccine_pfizer + vaccine_gamaleya + uptake_1 + uptake_25 + uptake_50 + uptake_75 + efficacy_50 + efficacy_70 + efficacy_78 + efficacy_91 + efficacy_95"

{
  names_co_relig <- c(     "Distributor: Civil Society",
                           "Distributor: Armed Forces",  
                           "Co-Religious", 
                           "Endorser: Religious Leader",   
                           "Endorser: President",
                           "Endorser: Mayor", 
                           "Endorser: Right Newspaper",
                           "Endorser: Left Newspaper",
                           "Producer: Sinovac",
                           "Producer: Astrazeneca",
                           "Producer: Pfizer",
                           "Producer: Gamaleya",
                           "1\\% Uptake",
                           "25\\% Uptake",
                           "50\\% Uptake",
                           "75\\% Uptake",
                           "50\\% Efficacy",
                           "70\\% Efficacy",
                           "78\\% Efficacy",
                           "91\\% Efficacy",
                           "95\\% Efficacy",
                           "Co-Religious X Religious Endorsement"
  ) 
}

RHS_list <- list(RHS_co_relig_catholic, RHS_co_relig_evangelical)
rounds_list <- c("full", "full") 
predictor_names_list <- list(names_co_relig, names_co_relig)
conjoints_table_names <- c("co_relig_catholic", "co_relig_evangelical") 

for (i in 1:length(conjoints_table_names)) {
  make_table_conjoint(RHS = RHS_list[[i]],
                                  outcome_vars = conjoint_outcomes,
                                  round = rounds_list[i],
                                  outcome_labels = conjoint_outcomes_label,
                                  treatment_labels = predictor_names_list[[i]],
                                  table_name = paste0("table_A7_", i, "_", conjoints_table_names[[i]]))
}

## Table A10 
hesitancy_long$most_hesitant <- ifelse(hesitancy_long$std_months_pre > 1.6, 1, 0)
most_hes <- subset(hesitancy_long, most_hesitant == 1)
not_most_hes <- subset(hesitancy_long, most_hesitant == 0)

conjoint_outcomes <- c("willing_vaccine", "quickly_post_1_text_reversed")
RHS_list <- list(RHS_conjoint_basic)
rounds_list <- c("full") 
predictor_names_list <- list(predictornames_basic)
conjoint_outcomes_label <- c("Willing", "Months (Rev)") 
conjoints_het_names <- c("most_hes") 

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint_most_hes(RHS = RHS_list[[i]],
                                   outcome_vars = conjoint_outcomes,
                                   round = rounds_list[i],
                                   outcome_labels = conjoint_outcomes_label,
                                   treatment_labels = predictor_names_list[[i]],
                                   table_name = paste0("table_A9_", i, "_", conjoints_het_names[[i]]))
}

conjoints_het_names <- c("not_most_hes") 

for (i in 1:length(conjoints_het_names)) {
  make_table_conjoint_not_most_hes(RHS = RHS_list[[i]],
                               outcome_vars = conjoint_outcomes,
                               round = rounds_list[i],
                               outcome_labels = conjoint_outcomes_label,
                               treatment_labels = predictor_names_list[[i]],
                               table_name = paste0("table_A9_", i, "_", conjoints_het_names[[i]]))
}


#### By-Country Conjoint Results ####
argentina <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                                    RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
                  data = subset(hesitancy_long, country == "Argentina"),
                  weights = subset(hesitancy_long, country == "Argentina")$w_conjoint,
                  cmethod = 'reghdfe')

brazil <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                                 RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
               data = subset(hesitancy_long, country == "Brasil"),
               weights = subset(hesitancy_long, country == "Brasil")$w_conjoint,
               cmethod = 'reghdfe')

chile <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                                RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
              data = subset(hesitancy_long, country == "Chile"),
              weights = subset(hesitancy_long, country == "Chile")$w_conjoint,
              cmethod = 'reghdfe')

colombia <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                                   RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
                 data = subset(hesitancy_long, country == "Colombia"),
                 weights = subset(hesitancy_long, country == "Colombia")$w_conjoint,
                 cmethod = 'reghdfe')

mexico <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                                 RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
               data = subset(hesitancy_long, country == "México"),
               weights = subset(hesitancy_long, country == "México")$w_conjoint,
               cmethod = 'reghdfe')


peru <- felm(as.formula(paste0(conjoint_outcomes[1], " ~ ",
                               RHS_conjoint_full, " | as.factor(responseid) + as.factor(round) |
                         0 | responseid")),
             data = subset(hesitancy_long, country == "Perú"),
             weights = subset(hesitancy_long, country == "Perú")$w_conjoint,
             cmethod = 'reghdfe')

argentina_plot <- coefplot(argentina, 
                         sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                         intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

argentina_plot

brazil_plot <- coefplot(brazil, 
                           sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                           intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

brazil_plot

chile_plot <- coefplot(chile, 
                           sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                           intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

chile_plot

colombia_plot <- coefplot(colombia, 
                           sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                           intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

colombia_plot

mexico_plot <- coefplot(mexico, 
                           sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                           intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

mexico_plot

peru_plot <- coefplot(peru, 
                           sds = summary(colombia, robust = TRUE)$coefficients[,'Cluster s.e.'],
                           intercept = TRUE) + 
  scale_y_discrete(labels = predictornames_figure) +
  ggtitle(conjoint_outcomes_label_long[1])

peru_plot

#### Hesitancy Plots - LAPOP #### 
argentina_2019 <- read_dta("lapop_argentina_2019.dta")
brazil_2019 <- read_dta("lapop_brazil_2019.dta")
chile_2019 <- read_dta("lapop_chile_2019.dta")
colombia_2019 <- read_dta("lapop_colombia_2019.dta")
mexico_2019 <- read_dta("lapop_mexico_2019.dta")
peru_2019 <- read_dta("lapop_peru_2019.dta")
  
### Argentina 
small_argentina <- data.frame(argentina_2019$pais, argentina_2019$b12, argentina_2019$b21a, argentina_2019$b32, argentina_2019$mil10a, argentina_2019$mil10e) 
colnames(small_argentina) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_argentina$china_trust_clean <- 5 - small_argentina$china_trust
small_argentina$us_trust_clean <- 5 - small_argentina$us_trust 
small_argentina$army_trust <- (4*(small_argentina$army_trust))/7 
small_argentina$president_trust <- (4*(small_argentina$president_trust))/7 
small_argentina$muni_trust <- (4*(small_argentina$muni_trust))/7 

small_argentina$army_trust <- ifelse(small_argentina$army_trust < 1, 1, small_argentina$army_trust)
small_argentina$president_trust <- ifelse(small_argentina$president_trust < 1, 1, small_argentina$president_trust)
small_argentina$muni_trust <- ifelse(small_argentina$muni_trust < 1, 1, small_argentina$muni_trust)

small_argentina$us_trust <- NULL
small_argentina$china_trust <- NULL 
small_argentina$pais <- "Argentina"
small_argentina$country <- NULL  

small_argentina <- subset(small_argentina, !is.na(army_trust))
small_argentina <- subset(small_argentina, !is.na(president_trust))
small_argentina <- subset(small_argentina, !is.na(muni_trust))
small_argentina_us <- subset(small_argentina, !is.na(us_trust_clean))
small_argentina_china <- subset(small_argentina, !is.na(china_trust_clean))


### Make small DF with means and SDs 
argentina_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(argentina_summary) <- c("institution")

argentina_summary$mean <- ifelse(argentina_summary$institution == "Army", mean(small_argentina$army_trust), 
                                 ifelse(argentina_summary$institution == "President", mean(small_argentina$president_trust),
                                        ifelse(argentina_summary$institution == "Mayor", mean(small_argentina$muni_trust),
                                               ifelse(argentina_summary$institution == "China", mean(small_argentina_china$china_trust),
                                                      ifelse(argentina_summary$institution == "United States", mean(small_argentina_us$us_trust), 0))))) 

argentina_summary$se <- ifelse(argentina_summary$institution == "Army", sd(small_argentina$army_trust)/sqrt(length(small_argentina$army_trust)), 
                               ifelse(argentina_summary$institution == "President", sd(small_argentina$president_trust)/sqrt(length(small_argentina$president_trust)),
                                      ifelse(argentina_summary$institution == "Mayor", sd(small_argentina$muni_trust)/sqrt(length(small_argentina$muni_trust)),
                                             ifelse(argentina_summary$institution == "China", sd(small_argentina_china$china_trust)/sqrt(length(small_argentina_china$china_trust)),
                                                    ifelse(argentina_summary$institution == "United States", sd(small_argentina_us$us_trust)/sqrt(length(small_argentina_us$us_trust)), 0))))) 

ggplot(argentina_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="lightskyblue1", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Argentina",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))

### Brazil
small_brazil <- data.frame(brazil_2019$pais, brazil_2019$b12, brazil_2019$b21a, brazil_2019$b32, brazil_2019$mil10a, brazil_2019$mil10e) 
colnames(small_brazil) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_brazil$china_trust_clean <- 5 - small_brazil$china_trust
small_brazil$us_trust_clean <- 5 - small_brazil$us_trust 
small_brazil$army_trust <- (4*(small_brazil$army_trust))/7 
small_brazil$president_trust <- (4*(small_brazil$president_trust))/7 
small_brazil$muni_trust <- (4*(small_brazil$muni_trust))/7 

small_brazil$army_trust <- ifelse(small_brazil$army_trust < 1, 1, small_brazil$army_trust)
small_brazil$president_trust <- ifelse(small_brazil$president_trust < 1, 1, small_brazil$president_trust)
small_brazil$muni_trust <- ifelse(small_brazil$muni_trust < 1, 1, small_brazil$muni_trust)

small_brazil$us_trust <- NULL
small_brazil$china_trust <- NULL
small_brazil$pais <- "Brazil"
small_brazil$country <- NULL  



small_brazil <- subset(small_brazil, !is.na(army_trust))
small_brazil <- subset(small_brazil, !is.na(president_trust))
small_brazil <- subset(small_brazil, !is.na(muni_trust))
small_brazil_us <- subset(small_brazil, !is.na(us_trust_clean))
small_brazil_china <- subset(small_brazil, !is.na(china_trust_clean))


### Make small DF with means and SDs 
brazil_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(brazil_summary) <- c("institution")

brazil_summary$mean <- ifelse(brazil_summary$institution == "Army", mean(small_brazil$army_trust), 
                              ifelse(brazil_summary$institution == "President", mean(small_brazil$president_trust),
                                     ifelse(brazil_summary$institution == "Mayor", mean(small_brazil$muni_trust),
                                            ifelse(brazil_summary$institution == "China", mean(small_brazil_china$china_trust),
                                                   ifelse(brazil_summary$institution == "United States", mean(small_brazil_us$us_trust), 0))))) 

brazil_summary$se <- ifelse(brazil_summary$institution == "Army", sd(small_brazil$army_trust)/sqrt(length(small_brazil$army_trust)), 
                            ifelse(brazil_summary$institution == "President", sd(small_brazil$president_trust)/sqrt(length(small_brazil$president_trust)),
                                   ifelse(brazil_summary$institution == "Mayor", sd(small_brazil$muni_trust)/sqrt(length(small_brazil$muni_trust)),
                                          ifelse(brazil_summary$institution == "China", sd(small_brazil_china$china_trust)/sqrt(length(small_brazil_china$china_trust)),
                                                 ifelse(brazil_summary$institution == "United States", sd(small_brazil_us$us_trust)/sqrt(length(small_brazil_us$us_trust)), 0))))) 

ggplot(brazil_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="lightskyblue3", alpha=0.8) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Brazil",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))



### Chile
small_chile <- data.frame(chile_2019$pais, chile_2019$b12, chile_2019$b21a, chile_2019$b32, chile_2019$mil10a, chile_2019$mil10e) 
colnames(small_chile) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_chile$china_trust_clean <- 5 - small_chile$china_trust
small_chile$us_trust_clean <- 5 - small_chile$us_trust 
small_chile$army_trust <- (4*(small_chile$army_trust))/7 
small_chile$president_trust <- (4*(small_chile$president_trust))/7 
small_chile$muni_trust <- (4*(small_chile$muni_trust))/7 

small_chile$army_trust <- ifelse(small_chile$army_trust < 1, 1, small_chile$army_trust)
small_chile$president_trust <- ifelse(small_chile$president_trust < 1, 1, small_chile$president_trust)
small_chile$muni_trust <- ifelse(small_chile$muni_trust < 1, 1, small_chile$muni_trust)

small_chile$us_trust <- NULL
small_chile$china_trust <- NULL
small_chile$pais <- "Chile"
small_chile$country <- NULL  

small_chile <- subset(small_chile, !is.na(army_trust))
small_chile <- subset(small_chile, !is.na(president_trust))
small_chile <- subset(small_chile, !is.na(muni_trust))
small_chile_us <- subset(small_chile, !is.na(us_trust_clean))
small_chile_china <- subset(small_chile, !is.na(china_trust_clean))


### Make small DF with means and SDs 
chile_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(chile_summary) <- c("institution")

chile_summary$mean <- ifelse(chile_summary$institution == "Army", mean(small_chile$army_trust), 
                             ifelse(chile_summary$institution == "President", mean(small_chile$president_trust),
                                    ifelse(chile_summary$institution == "Mayor", mean(small_chile$muni_trust),
                                           ifelse(chile_summary$institution == "China", mean(small_chile_china$china_trust),
                                                  ifelse(chile_summary$institution == "United States", mean(small_chile_us$us_trust), 0))))) 

chile_summary$se <- ifelse(chile_summary$institution == "Army", sd(small_chile$army_trust)/sqrt(length(small_chile$army_trust)), 
                           ifelse(chile_summary$institution == "President", sd(small_chile$president_trust)/sqrt(length(small_chile$president_trust)),
                                  ifelse(chile_summary$institution == "Mayor", sd(small_chile$muni_trust)/sqrt(length(small_chile$muni_trust)),
                                         ifelse(chile_summary$institution == "China", sd(small_chile_china$china_trust)/sqrt(length(small_chile_china$china_trust)),
                                                ifelse(chile_summary$institution == "United States", sd(small_chile_us$us_trust)/sqrt(length(small_chile_us$us_trust)), 0))))) 

ggplot(chile_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="steelblue1", alpha=0.8) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Chile",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Colombia
small_colombia <- data.frame(colombia_2019$pais, colombia_2019$b12, colombia_2019$b21a, colombia_2019$b32, colombia_2019$mil10a, colombia_2019$mil10e) 
colnames(small_colombia) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_colombia$china_trust_clean <- 5 - small_colombia$china_trust
small_colombia$us_trust_clean <- 5 - small_colombia$us_trust 
small_colombia$army_trust <- (4*(small_colombia$army_trust))/7 
small_colombia$president_trust <- (4*(small_colombia$president_trust))/7 
small_colombia$muni_trust <- (4*(small_colombia$muni_trust))/7 

small_colombia$army_trust <- ifelse(small_colombia$army_trust < 1, 1, small_colombia$army_trust)
small_colombia$president_trust <- ifelse(small_colombia$president_trust < 1, 1, small_colombia$president_trust)
small_colombia$muni_trust <- ifelse(small_colombia$muni_trust < 1, 1, small_colombia$muni_trust)

small_colombia$us_trust <- NULL
small_colombia$china_trust <- NULL
small_colombia$pais <- "Colombia"
small_colombia$country <- NULL  

small_colombia <- subset(small_colombia, !is.na(army_trust))
small_colombia <- subset(small_colombia, !is.na(president_trust))
small_colombia <- subset(small_colombia, !is.na(muni_trust))
small_colombia_us <- subset(small_colombia, !is.na(us_trust_clean))
small_colombia_china <- subset(small_colombia, !is.na(china_trust_clean))


### Make small DF with means and SDs 
colombia_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(colombia_summary) <- c("institution")

colombia_summary$mean <- ifelse(colombia_summary$institution == "Army", mean(small_colombia$army_trust), 
                                ifelse(colombia_summary$institution == "President", mean(small_colombia$president_trust),
                                       ifelse(colombia_summary$institution == "Mayor", mean(small_colombia$muni_trust),
                                              ifelse(colombia_summary$institution == "China", mean(small_colombia_china$china_trust),
                                                     ifelse(colombia_summary$institution == "United States", mean(small_colombia_us$us_trust), 0))))) 

colombia_summary$se <- ifelse(colombia_summary$institution == "Army", sd(small_colombia$army_trust)/sqrt(length(small_colombia$army_trust)), 
                              ifelse(colombia_summary$institution == "President", sd(small_colombia$president_trust)/sqrt(length(small_colombia$president_trust)),
                                     ifelse(colombia_summary$institution == "Mayor", sd(small_colombia$muni_trust)/sqrt(length(small_colombia$muni_trust)),
                                            ifelse(colombia_summary$institution == "China", sd(small_colombia_china$china_trust)/sqrt(length(small_colombia_china$china_trust)),
                                                   ifelse(colombia_summary$institution == "United States", sd(small_colombia_us$us_trust)/sqrt(length(small_colombia_us$us_trust)), 0))))) 

ggplot(colombia_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="steelblue3", alpha=0.8) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Colombia",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Mexico
small_mexico <- data.frame(mexico_2019$pais, mexico_2019$b12, mexico_2019$b21a, mexico_2019$b32, mexico_2019$mil10a, mexico_2019$mil10e) 
colnames(small_mexico) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_mexico$china_trust_clean <- 5 - small_mexico$china_trust
small_mexico$us_trust_clean <- 5 - small_mexico$us_trust 
small_mexico$army_trust <- (4*(small_mexico$army_trust))/7 
small_mexico$president_trust <- (4*(small_mexico$president_trust))/7 
small_mexico$muni_trust <- (4*(small_mexico$muni_trust))/7 


small_mexico$army_trust <- ifelse(small_mexico$army_trust < 1, 1, small_mexico$army_trust)
small_mexico$president_trust <- ifelse(small_mexico$president_trust < 1, 1, small_mexico$president_trust)
small_mexico$muni_trust <- ifelse(small_mexico$muni_trust < 1, 1, small_mexico$muni_trust) 

small_mexico$us_trust <- NULL
small_mexico$china_trust <- NULL
small_mexico$pais <- "Mexico"
small_mexico$country <- NULL  


small_mexico <- subset(small_mexico, !is.na(army_trust))
small_mexico <- subset(small_mexico, !is.na(president_trust))
small_mexico <- subset(small_mexico, !is.na(muni_trust))
small_mexico_us <- subset(small_mexico, !is.na(us_trust_clean))
small_mexico_china <- subset(small_mexico, !is.na(china_trust_clean))


### Make small DF with means and SDs 
mexico_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(mexico_summary) <- c("institution")

mexico_summary$mean <- ifelse(mexico_summary$institution == "Army", mean(small_mexico$army_trust), 
                              ifelse(mexico_summary$institution == "President", mean(small_mexico$president_trust),
                                     ifelse(mexico_summary$institution == "Mayor", mean(small_mexico$muni_trust),
                                            ifelse(mexico_summary$institution == "China", mean(small_mexico_china$china_trust),
                                                   ifelse(mexico_summary$institution == "United States", mean(small_mexico_us$us_trust), 0))))) 

mexico_summary$se <- ifelse(mexico_summary$institution == "Army", sd(small_mexico$army_trust)/sqrt(length(small_mexico$army_trust)), 
                            ifelse(mexico_summary$institution == "President", sd(small_mexico$president_trust)/sqrt(length(small_mexico$president_trust)),
                                   ifelse(mexico_summary$institution == "Mayor", sd(small_mexico$muni_trust)/sqrt(length(small_mexico$muni_trust)),
                                          ifelse(mexico_summary$institution == "China", sd(small_mexico_china$china_trust)/sqrt(length(small_mexico_china$china_trust)),
                                                 ifelse(mexico_summary$institution == "United States", sd(small_mexico_us$us_trust)/sqrt(length(small_mexico_us$us_trust)), 0))))) 

ggplot(mexico_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="mediumblue", alpha=0.8) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Mexico",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))



### Peru 
small_peru <- data.frame(peru_2019$pais, peru_2019$b12, peru_2019$b21a, peru_2019$b32, peru_2019$mil10a, peru_2019$mil10e) 


colnames(small_peru) <- c("country", "army_trust", "president_trust", "muni_trust", "china_trust", "us_trust")
small_peru$china_trust_clean <- 5 - small_peru$china_trust
small_peru$us_trust_clean <- 5 - small_peru$us_trust 
small_peru$army_trust <- (4*(small_peru$army_trust))/7 
small_peru$president_trust <- (4*(small_peru$president_trust))/7 
small_peru$muni_trust <- (4*(small_peru$muni_trust))/7 

small_peru$army_trust <- ifelse(small_peru$army_trust < 1, 1, small_peru$army_trust)
small_peru$president_trust <- ifelse(small_peru$president_trust < 1, 1, small_peru$president_trust)
small_peru$muni_trust <- ifelse(small_peru$muni_trust < 1, 1, small_peru$muni_trust)

small_peru$us_trust <- NULL
small_peru$china_trust <- NULL
small_peru$pais <- "Peru"
small_peru$country <- NULL  

# Clean to make pllot 
small_peru <- subset(small_peru, !is.na(army_trust))
small_peru <- subset(small_peru, !is.na(president_trust))
small_peru <- subset(small_peru, !is.na(muni_trust))
small_peru_us <- subset(small_peru, !is.na(us_trust_clean))
small_peru_china <- subset(small_peru, !is.na(china_trust_clean))


### Make small DF with means and SDs 
peru_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(peru_summary) <- c("institution")

peru_summary$mean <- ifelse(peru_summary$institution == "Army", mean(small_peru$army_trust), 
                            ifelse(peru_summary$institution == "President", mean(small_peru$president_trust),
                                   ifelse(peru_summary$institution == "Mayor", mean(small_peru$muni_trust),
                                          ifelse(peru_summary$institution == "China", mean(small_peru_china$china_trust),
                                                 ifelse(peru_summary$institution == "United States", mean(small_peru_us$us_trust), 0))))) 

peru_summary$se <- ifelse(peru_summary$institution == "Army", sd(small_peru$army_trust)/sqrt(length(small_peru$army_trust)), 
                          ifelse(peru_summary$institution == "President", sd(small_peru$president_trust)/sqrt(length(small_peru$president_trust)),
                                 ifelse(peru_summary$institution == "Mayor", sd(small_peru$muni_trust)/sqrt(length(small_peru$muni_trust)),
                                        ifelse(peru_summary$institution == "China", sd(small_peru_china$china_trust)/sqrt(length(small_peru_china$china_trust)),
                                               ifelse(peru_summary$institution == "United States", sd(small_peru_us$us_trust)/sqrt(length(small_peru_us$us_trust)), 0))))) 

ggplot(peru_summary) +  
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="navy", alpha=0.8) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="LAPOP (General Population) Trust - Peru",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4)) 

rm(argentina_2019, brazil_2019, colombia_2019, chile_2019, mexico_2019, peru_2019, argentina_summary, brazil_summary, chile_summary,
   colombia_summary, mexico_summary, peru_summary, small_argentina, small_brazil, small_chile, small_colombia, small_mexico, small_peru)  

#### Hesitancy Plots - Our Survey ####
hesitancy_wide <- read_dta("vaccine_wide.dta")
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_country_govs_1) & hesitancy_wide$trust_country_govs_1 > 0) # China 
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_country_govs_2) & hesitancy_wide$trust_country_govs_2 > 0) # US Trump 
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_country_govs_3) & hesitancy_wide$trust_country_govs_3 > 0) # US China 
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_orgs_3) & hesitancy_wide$trust_orgs_3 > 0) # Army 
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_persons_insts_1) & hesitancy_wide$trust_persons_insts_1 > 0) # President 
hesitancy_wide <- subset(hesitancy_wide, !is.na(hesitancy_wide$trust_persons_insts_2)  & hesitancy_wide$trust_persons_insts_2 > 0) # Mayor     
hesitancy_wide$us_trust <- (hesitancy_wide$trust_country_govs_2 + hesitancy_wide$trust_country_govs_3)/2

## Subset into countries 
argentina <- subset(hesitancy_wide, country == "Argentina")
brazil <- subset(hesitancy_wide, country == "Brasil")
chile <- subset(hesitancy_wide, country == "Chile")
colombia <- subset(hesitancy_wide, country == "Colombia")
mexico <- subset(hesitancy_wide, country == "México")
peru <- subset(hesitancy_wide, country == "Perú")


### Make small DF with means and SDs 
argentina_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(argentina_hes_summary) <- c("institution")

argentina_hes_summary$mean <- ifelse(argentina_hes_summary$institution == "Army", mean(argentina$trust_orgs_3), 
                                     ifelse(argentina_hes_summary$institution == "President", mean(argentina$trust_persons_insts_1),
                                            ifelse(argentina_hes_summary$institution == "Mayor", mean(argentina$trust_persons_insts_2),
                                                   ifelse(argentina_hes_summary$institution == "China", mean(argentina$trust_country_govs_1),
                                                          ifelse(argentina_hes_summary$institution == "United States", mean(argentina$us_trust), 0)))))  

argentina_hes_summary$se <- ifelse(argentina_hes_summary$institution == "Army", sd(argentina$trust_orgs_3)/sqrt(length(argentina$trust_orgs_3)), 
                                   ifelse(argentina_hes_summary$institution == "President", sd(argentina$trust_persons_insts_1)/sqrt(length(argentina$trust_persons_insts_1)),
                                          ifelse(argentina_hes_summary$institution == "Mayor", sd(argentina$trust_persons_insts_2)/sqrt(length(argentina$trust_persons_insts_2)),
                                                 ifelse(argentina_hes_summary$institution == "China", sd(argentina$trust_country_govs_1)/sqrt(length(argentina$trust_country_govs_1)),
                                                        ifelse(argentina_hes_summary$institution == "United States", sd(argentina$us_trust)/sqrt(length(argentina$us_trust)), 0))))) 

ggplot(argentina_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="lightskyblue1", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Argentina",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Make small DF with means and SDs 
brazil_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(brazil_hes_summary) <- c("institution")

brazil_hes_summary$mean <- ifelse(brazil_hes_summary$institution == "Army", mean(brazil$trust_orgs_3), 
                                  ifelse(brazil_hes_summary$institution == "President", mean(brazil$trust_persons_insts_1),
                                         ifelse(brazil_hes_summary$institution == "Mayor", mean(brazil$trust_persons_insts_2),
                                                ifelse(brazil_hes_summary$institution == "China", mean(brazil$trust_country_govs_1),
                                                       ifelse(brazil_hes_summary$institution == "United States", mean(brazil$us_trust), 0)))))  

brazil_hes_summary$se <- ifelse(brazil_hes_summary$institution == "Army", sd(brazil$trust_orgs_3)/sqrt(length(brazil$trust_orgs_3)), 
                                ifelse(brazil_hes_summary$institution == "President", sd(brazil$trust_persons_insts_1)/sqrt(length(brazil$trust_persons_insts_1)),
                                       ifelse(brazil_hes_summary$institution == "Mayor", sd(brazil$trust_persons_insts_2)/sqrt(length(brazil$trust_persons_insts_2)),
                                              ifelse(brazil_hes_summary$institution == "China", sd(brazil$trust_country_govs_1)/sqrt(length(brazil$trust_country_govs_1)),
                                                     ifelse(brazil_hes_summary$institution == "United States", sd(brazil$us_trust)/sqrt(length(brazil$us_trust)), 0))))) 

ggplot(brazil_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="lightskyblue3", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Brazil",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Make small DF with means and SDs 
chile_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(chile_hes_summary) <- c("institution")

chile_hes_summary$mean <- ifelse(chile_hes_summary$institution == "Army", mean(chile$trust_orgs_3), 
                                 ifelse(chile_hes_summary$institution == "President", mean(chile$trust_persons_insts_1),
                                        ifelse(chile_hes_summary$institution == "Mayor", mean(chile$trust_persons_insts_2),
                                               ifelse(chile_hes_summary$institution == "China", mean(chile$trust_country_govs_1),
                                                      ifelse(chile_hes_summary$institution == "United States", mean(chile$us_trust), 0)))))  

chile_hes_summary$se <- ifelse(chile_hes_summary$institution == "Army", sd(chile$trust_orgs_3)/sqrt(length(chile$trust_orgs_3)), 
                               ifelse(chile_hes_summary$institution == "President", sd(chile$trust_persons_insts_1)/sqrt(length(chile$trust_persons_insts_1)),
                                      ifelse(chile_hes_summary$institution == "Mayor", sd(chile$trust_persons_insts_2)/sqrt(length(chile$trust_persons_insts_2)),
                                             ifelse(chile_hes_summary$institution == "China", sd(chile$trust_country_govs_1)/sqrt(length(chile$trust_country_govs_1)),
                                                    ifelse(chile_hes_summary$institution == "United States", sd(chile$us_trust)/sqrt(length(chile$us_trust)), 0))))) 

ggplot(chile_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="steelblue1", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Chile",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Make small DF with means and SDs 
colombia_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(colombia_hes_summary) <- c("institution")

colombia_hes_summary$mean <- ifelse(colombia_hes_summary$institution == "Army", mean(colombia$trust_orgs_3), 
                                    ifelse(colombia_hes_summary$institution == "President", mean(colombia$trust_persons_insts_1),
                                           ifelse(colombia_hes_summary$institution == "Mayor", mean(colombia$trust_persons_insts_2),
                                                  ifelse(colombia_hes_summary$institution == "China", mean(colombia$trust_country_govs_1),
                                                         ifelse(colombia_hes_summary$institution == "United States", mean(colombia$us_trust), 0)))))  

colombia_hes_summary$se <- ifelse(colombia_hes_summary$institution == "Army", sd(colombia$trust_orgs_3)/sqrt(length(colombia$trust_orgs_3)), 
                                  ifelse(colombia_hes_summary$institution == "President", sd(colombia$trust_persons_insts_1)/sqrt(length(colombia$trust_persons_insts_1)),
                                         ifelse(colombia_hes_summary$institution == "Mayor", sd(colombia$trust_persons_insts_2)/sqrt(length(colombia$trust_persons_insts_2)),
                                                ifelse(colombia_hes_summary$institution == "China", sd(colombia$trust_country_govs_1)/sqrt(length(colombia$trust_country_govs_1)),
                                                       ifelse(colombia_hes_summary$institution == "United States", sd(colombia$us_trust)/sqrt(length(colombia$us_trust)), 0))))) 

ggplot(colombia_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="steelblue3", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Colombia",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))


### Make small DF with means and SDs 
mexico_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(mexico_hes_summary) <- c("institution")

mexico_hes_summary$mean <- ifelse(mexico_hes_summary$institution == "Army", mean(mexico$trust_orgs_3), 
                                  ifelse(mexico_hes_summary$institution == "President", mean(mexico$trust_persons_insts_1),
                                         ifelse(mexico_hes_summary$institution == "Mayor", mean(mexico$trust_persons_insts_2),
                                                ifelse(mexico_hes_summary$institution == "China", mean(mexico$trust_country_govs_1),
                                                       ifelse(mexico_hes_summary$institution == "United States", mean(mexico$us_trust), 0)))))  

mexico_hes_summary$se <- ifelse(mexico_hes_summary$institution == "Army", sd(mexico$trust_orgs_3)/sqrt(length(mexico$trust_orgs_3)), 
                                ifelse(mexico_hes_summary$institution == "President", sd(mexico$trust_persons_insts_1)/sqrt(length(mexico$trust_persons_insts_1)),
                                       ifelse(mexico_hes_summary$institution == "Mayor", sd(mexico$trust_persons_insts_2)/sqrt(length(mexico$trust_persons_insts_2)),
                                              ifelse(mexico_hes_summary$institution == "China", sd(mexico$trust_country_govs_1)/sqrt(length(mexico$trust_country_govs_1)),
                                                     ifelse(mexico_hes_summary$institution == "United States", sd(mexico$us_trust)/sqrt(length(mexico$us_trust)), 0))))) 

ggplot(mexico_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="mediumblue", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Mexico",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))

### Make small DF with means and SDs 
peru_hes_summary <- as.data.frame(c("Army", "President", "Mayor", "China", "United States"))
colnames(peru_hes_summary) <- c("institution")

peru_hes_summary$mean <- ifelse(peru_hes_summary$institution == "Army", mean(peru$trust_orgs_3), 
                                ifelse(peru_hes_summary$institution == "President", mean(peru$trust_persons_insts_1),
                                       ifelse(peru_hes_summary$institution == "Mayor", mean(peru$trust_persons_insts_2),
                                              ifelse(peru_hes_summary$institution == "China", mean(peru$trust_country_govs_1),
                                                     ifelse(peru_hes_summary$institution == "United States", mean(peru$us_trust), 0)))))  

peru_hes_summary$se <- ifelse(peru_hes_summary$institution == "Army", sd(peru$trust_orgs_3)/sqrt(length(peru$trust_orgs_3)), 
                              ifelse(peru_hes_summary$institution == "President", sd(peru$trust_persons_insts_1)/sqrt(length(peru$trust_persons_insts_1)),
                                     ifelse(peru_hes_summary$institution == "Mayor", sd(peru$trust_persons_insts_2)/sqrt(length(peru$trust_persons_insts_2)),
                                            ifelse(peru_hes_summary$institution == "China", sd(peru$trust_country_govs_1)/sqrt(length(peru$trust_country_govs_1)),
                                                   ifelse(peru_hes_summary$institution == "United States", sd(peru$us_trust)/sqrt(length(peru$us_trust)), 0))))) 

ggplot(peru_hes_summary) +
  geom_bar(aes(x=institution, y=mean), stat="identity", fill="navy", alpha=1.0) +
  geom_errorbar( aes(x=institution, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(title="Our Survey (Hesitant Population) Trust - Peru",
       x ="", y = "Mean Trust") +
  scale_y_continuous(limits = c(0,4))

rm(peru_hes_summary, mexico_hes_summary, argentina_hes_summary, brazil_hes_summary, chile_hes_summary, colombia_hes_summary, hesitancy_wide)