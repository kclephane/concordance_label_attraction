#call libraries and data sets ----
library(tidyverse)

#working directory for KLC's CB3 PC
setwd("C:/Users/kclephane2/OneDrive - University of Nebraska-Lincoln/label_attraction_concord")

HASH_Fall_2019_1 <- read_csv("raw data/HASH_fall2019_1_legend.csv")
HASH_Fall_2019_2 <- read_csv("raw data/HASH_fall2019_2_legend.csv")
HASH_Spring_2020 <-  read_csv("raw data/HASH_spring2020_legend2.csv")


#### creating functions! ####
#create functions that are useful for data cleaning

recode_OBCS <- function(x) {
  recode(x, 
         `1 (Strongly disagree)`= 1,
         `6 (Strongly agree)` = 6,
         `2` = 2,
         `3` = 3, 
         `4` = 4,
         `5` = 5)
}

recode_ESS <- function(x) {
  recode(x, 
         `1 (Disagree strongly)`= 1,
         `6 (Agree strongly)` = 6,
         `2` = 2,
         `3` = 3, 
         `4` = 4,
         `5` = 5)
}

recode_ISOS <- function(x) {
  recode(x, 
         `1 (Never)`= 1,
         `2 (Rarely)` = 2,
         `3 (Occasionally)` = 3, 
         `4 (Frequently)` = 4,
         `5 (Almost always)` = 5)
}

recode_USA <- function(x) {
  recode(x, 
         "Never" = 0,
         "Once" = 1, 
         "More than once" = 2, 
         "Choose not to answer/Not applicable" = NA_real_)
}

recode_HHRD <- function(x) {
  recode(x, 
         "This has never happened to me" = 1, 
         "Once in a while (less than 10% of the time)" = 2, 
         "Sometimes (10 - 25% of the time)" = 3, 
         "A lot (26 - 49% of the time)" = 4,
         "Most of the time (50 - 70% of the time)" = 5, 
         "Almost all of the time (more than 70%)" = 6,
         "Not applicable/ choose not to respond" = NA_real_
  )
}

recode_LGBIS <- function(x) {
  recode(x, 
         "Strongly disagree" = 1, 
         "Disagree" = 2, 
         "Somewhat disagree" = 3, 
         "Somewhat agree" = 4,
         "Agree" = 5,
         "Strongly agree" = 6, 
         "Not applicable/ choose not to respond" = NA_real_)
  
}

recode_LGBIS_reverse <- function(x) {
  recode(x, 
         "Strongly disagree" = 6, 
         "Disagree" = 5, 
         "Somewhat disagree" = 4, 
         "Somewhat agree" = 3,
         "Agree" = 2,
         "Strongly agree" = 1, 
         "Not applicable/ choose not to respond" = NA_real_)
  
  
}

recode_PHQ9 <- function(x) {
  recode(x, 
         "Not at all" = 0,
         "Several days" = 1,
         "More than half the days" = 2,
         "Nearly every day" = 3, 
         "Not applicable / choose not to respond" = NA_real_
  )
}

recode_GAD7 <- function(x) {
  recode(x, 
         "Not at all" = 0,
         "Several days" = 1,
         "More than half the days" = 2,
         "Nearly every day" = 3
  )
}

recode_SSSS <- function(x) {
  recode(x, 
         "Not at all like me" = 1, 
         "A little like me" = 2, 
         "Somewhat like me" = 3, 
         "Very much like me" = 4,
         "Choose not to respond" = NA_real_
  )
}

recode_SESI <- function(x) {
  recode(x, 
         "Strongly disagree" = 1, 
         "Somewhat disagree" = 2, 
         "Somewhat agree" = 3, 
         "Strongly agree" = 4,
         "Choose not to respond" = NA_real_
  )
}

recode_SESI_reverse <- function(x) {
  recode(x, 
         "Strongly disagree" = 4, 
         "Somewhat disagree" = 3, 
         "Somewhat agree" = 2, 
         "Strongly agree" = 1,
         "Choose not to respond" = NA_real_
  )
}


recode_SRQ <- function(x) {
  recode(x, 
         "Strongly disagree" = 1, 
         "Disagree" = 2, 
         "Somewhat disagree" = 3, 
         "Neither agree nor disagree" = 4,
         "Somewhat agree" = 5, 
         "Agree" = 6, 
         "Strongly agree" = 7, 
         "Choose not to respond" = NA_real_
  )
}

recode_BCEO <- function(x) {
  recode(x, 
         "Disagree" = 1, 
         "Slightly disagree" = 2, 
         "Slightly agree" = 3, 
         "Agree" = 4, 
         "Not applicable/choose not to respond" = NA_real_
  )
}


# recode the funky way the objectification data are coded

HASH_Fall_2019_2 <- HASH_Fall_2019_2 %>% 
  mutate(OBCS_1 = recode_OBCS(OBCS_1), 
         OBCS_2 = recode_OBCS(OBCS_2), 
         OBCS_3 = recode_OBCS(OBCS_3), 
         OBCS_4 = recode_OBCS(OBCS_4), 
         OBCS_5 = recode_OBCS(OBCS_5), 
         OBCS_6 = recode_OBCS(OBCS_6), 
         OBCS_7 = recode_OBCS(OBCS_7), 
         OBCS_8 = recode_OBCS(OBCS_8), 
         OBCS_9 = recode_OBCS(OBCS_9), 
         OBCS_10 = recode_OBCS(OBCS_10), 
         OBCS_11 = recode_OBCS(OBCS_11), 
         OBCS_12 = recode_OBCS(OBCS_12), 
         OBCS_13 = recode_OBCS(OBCS_13), 
         OBCS_14 = recode_OBCS(OBCS_14), 
         OBCS_15 = recode_OBCS(OBCS_15),
         OBCS_16 = recode_OBCS(OBCS_16), 
         ESS_1 = recode_ESS(ESS_1),
         ESS_2 = recode_ESS(ESS_2),
         ESS_3 = recode_ESS(ESS_3),
         ESS_4 = recode_ESS(ESS_4),
         ESS_5 = recode_ESS(ESS_5),
         ESS_6 = recode_ESS(ESS_6),
         ESS_7 = recode_ESS(ESS_7),
         ESS_8 = recode_ESS(ESS_8), 
         ISOS_1 = recode_ISOS(ISOS_1), 
         ISOS_2 = recode_ISOS(ISOS_2),
         ISOS_3 = recode_ISOS(ISOS_3),
         ISOS_4 = recode_ISOS(ISOS_4),
         ISOS_5 = recode_ISOS(ISOS_5),
         ISOS_6 = recode_ISOS(ISOS_6),
         ISOS_7 = recode_ISOS(ISOS_7),
         ISOS_8 = recode_ISOS(ISOS_8),
         ISOS_9 = recode_ISOS(ISOS_9),
         ISOS_10 = recode_ISOS(ISOS_10),
         ISOS_11 = recode_ISOS(ISOS_11),
         ISOS_12 = recode_ISOS(ISOS_12),
         ISOS_13 = recode_ISOS(ISOS_13),
         ISOS_14 = recode_ISOS(ISOS_14),
         ISOS_15 = recode_ISOS(ISOS_15)
  )

HASH_Spring_2020 <- HASH_Spring_2020 %>% 
  mutate(OBCS_1 = recode_OBCS(OBCS_1), 
         OBCS_2 = recode_OBCS(OBCS_2), 
         OBCS_3 = recode_OBCS(OBCS_3), 
         OBCS_4 = recode_OBCS(OBCS_4), 
         OBCS_5 = recode_OBCS(OBCS_5), 
         OBCS_6 = recode_OBCS(OBCS_6), 
         OBCS_7 = recode_OBCS(OBCS_7), 
         OBCS_8 = recode_OBCS(OBCS_8), 
         OBCS_9 = recode_OBCS(OBCS_9), 
         OBCS_10 = recode_OBCS(OBCS_10), 
         OBCS_11 = recode_OBCS(OBCS_11), 
         OBCS_12 = recode_OBCS(OBCS_12), 
         OBCS_13 = recode_OBCS(OBCS_13), 
         OBCS_14 = recode_OBCS(OBCS_14), 
         OBCS_15 = recode_OBCS(OBCS_15),
         OBCS_16 = recode_OBCS(OBCS_16), 
         ISOS_1 = recode_ISOS(ISOS_1), 
         ISOS_2 = recode_ISOS(ISOS_2),
         ISOS_3 = recode_ISOS(ISOS_3),
         ISOS_4 = recode_ISOS(ISOS_4),
         ISOS_5 = recode_ISOS(ISOS_5),
         ISOS_6 = recode_ISOS(ISOS_6),
         ISOS_7 = recode_ISOS(ISOS_7),
         ISOS_8 = recode_ISOS(ISOS_8),
         ISOS_9 = recode_ISOS(ISOS_9),
         ISOS_10 = recode_ISOS(ISOS_10),
         ISOS_11 = recode_ISOS(ISOS_11),
         ISOS_12 = recode_ISOS(ISOS_12),
         ISOS_13 = recode_ISOS(ISOS_13),
         ISOS_14 = recode_ISOS(ISOS_14),
         ISOS_15 = recode_ISOS(ISOS_15)
  )

#merge datasets ====

HASH_raw <- bind_rows(HASH_Fall_2019_1, HASH_Fall_2019_2, HASH_Spring_2020)


#break up multiple choice demographics ----

HASH_coded <- HASH_raw %>% #this section recodes the questions with multiple choices, using the actual combos that appear in the data to create multi-choice categories
  mutate(race_factor = factor(race), 
         race_groups = recode_factor(race_factor, 
                                     "White" = "White, non-Hispanic", 
                                     "East Asian (e.g., Chinese, Japanese)" = "East Asian", 
                                     "Black or African American" = "Black", 
                                     "Hispanic, Latino/a, or Spanish origin" = "White Hispanic", 
                                     "South Asian (e.g., Indian)" = "South Asian", 
                                     "White,Hispanic, Latino/a, or Spanish origin" = "White, Hispanic",
                                     "Other (please specify)" = "Other", 
                                     "White,American Indian, Alaska Native, or First Nations" = "Other", 
                                     "White,Black or African American" = "Multiracial", 
                                     "White,East Asian (e.g., Chinese, Japanese)" = "Multiracial", 
                                     "White,East Asian (e.g., Chinese, Japanese),American Indian, Alaska Native, or First Nations" = "Multiracial", 
                                     "White,East Asian (e.g., Chinese, Japanese),South Asian (e.g., Indian)" = "Multiracial", 
                                     "American Indian, Alaska Native, or First Nations" = "Multiracial", 
                                     "East Asian (e.g., Chinese, Japanese),Other (please specify)" = "Multiracial",
                                     "White,East Asian (e.g., Chinese, Japanese),Native Hawaiian or Pacific Islander" = "Multiracial", 
                                     "White,Native Hawaiian or Pacific Islander" = 'Multiracial',
                                     "White,Hispanic, Latino/a, or Spanish origin,East Asian (e.g., Chinese, Japanese)" = "Multiracial")
  ) %>% 
  mutate(orientation_factor = factor(orientation_label), 
         orientation_groups = recode_factor(orientation_factor, 
                                            "Bisexual" = "Bisexual", 
                                            "Heterosexual" = "Exclusively heterosexual", 
                                            "Heterosexual,Mostly heterosexual (e.g., heteroflexible, bicurious)" = "Mostly heterosexual", 
                                            "Mostly heterosexual (e.g., heteroflexible, bicurious)" = "Mostly heterosexual", 
                                            "Mostly heterosexual (e.g., heteroflexible, bicurious),Bisexual,Queer" = "Bisexual", 
                                            "Mostly heterosexual (e.g., heteroflexible, bicurious),Pansexual" = "Mostly heterosexual", 
                                            "Bisexual,Pansexual" = "Bisexual", 
                                            "Bisexual,Pansexual,Queer" = "Bisexual", 
                                            "Bisexual,Mostly lesbian/gay,Queer" = "Other", 
                                            "Heterosexual,Bisexual" = "Bisexual", 
                                            "Heterosexual,Lesbian/gay" = 'Other', 
                                            "Lesbian/gay" = "Exclusively lesbian/gay", 
                                            "Other (please specify)" = "Other", 
                                            "Pansexual" = "Other", 
                                            "Queer" = "Other", 
                                            "Heterosexual,Mostly heterosexual (e.g., heteroflexible, bicurious),Other (please specify)" = "Mostly heterosexual", 
                                            "Mostly heterosexual (e.g., heteroflexible, bicurious),Bisexual" = "Bisexual", 
                                            "Mostly lesbian/gay,Lesbian/gay" = "Other")
         
  ) %>% 
  mutate(relationship_status_factor = factor(relationship_status), 
         relationship_status_groups = recode_factor(relationship_status_factor,
                                                    "Dating" = "Dating", 
                                                    "Dating,Married/living with long-term relationship partner(s)" = "Married/Cohabiting",
                                                    "Dating,Other relationship status (please specify):" = "Dating", 
                                                    "Engaged" = "Engaged", 
                                                    "Engaged,Married/living with long-term relationship partner(s)" = "Married/Cohabiting", 
                                                    "In consensually non-mongamous relationship(s)" = "Other", 
                                                    "Married/living with long-term relationship partner(s)" = "Married/Cohabiting", 
                                                    "Other relationship status (please specify):" = "Other", 
                                                    "Single" = "Single", 
                                                    "Single,Dating" = "Dating", 
                                                    "Single,Other relationship status (please specify):" = "Other")
  )
HASH_coded$orientation_groups = relevel(HASH_coded$orientation_groups, ref = "Exclusively heterosexual")
HASH_coded$race_groups = relevel(HASH_coded$race_groups, ref = "White, non-Hispanic")
HASH_coded$relationship_status_groups = relevel(HASH_coded$relationship_status_groups, ref = "Single")


HASH_coded <- HASH_coded %>% #this section creates variables around the gender & orientation sliders
  mutate(numeric_masculine = if_else(is.na(masculine) & is.na(feminine) & is.na(genderqueer), NA_real_, 
                                     if_else(is.na(masculine) & (!is.na(feminine) | !is.na(genderqueer)), 0, masculine)
  )) %>% 
  mutate(numeric_feminine = if_else(is.na(masculine) & is.na(feminine) & is.na(genderqueer), NA_real_, 
                                    if_else(is.na(feminine) & (!is.na(masculine) | !is.na(genderqueer)), 0, feminine)
  )) %>% 
  mutate(numeric_genderqueer = if_else(is.na(masculine) & is.na(feminine) & is.na(genderqueer), NA_real_, 
                                       if_else(is.na(genderqueer) & (!is.na(masculine) | !is.na(feminine)), 0, genderqueer)
  )) %>% 
  mutate(numeric_man_attraction = if_else(is.na(attraction_men) & is.na(attraction_women) & is.na(attraction_NB), NA_real_, 
                                          if_else(is.na(attraction_men) & (!is.na(attraction_women) | !is.na(attraction_NB)), 0, attraction_men)
  )) %>% 
  mutate(numeric_woman_attraction = if_else(is.na(attraction_men) & is.na(attraction_women) & is.na(attraction_NB), NA_real_, 
                                            if_else(is.na(attraction_women) & (!is.na(attraction_men) | !is.na(attraction_NB)), 0, attraction_women)
  )) %>% 
  mutate(numeric_NB_attraction = if_else(is.na(attraction_men) & is.na(attraction_women) & is.na(attraction_NB), NA_real_, 
                                         if_else(is.na(attraction_NB) & (!is.na(attraction_men) | !is.na(attraction_women)), 0, attraction_NB))) %>% 
  
  
  mutate(numeric_man_romantic = if_else(is.na(romantic_men) & is.na(romantic_women) & is.na(romantic_NB), NA_real_, 
                                        if_else(is.na(romantic_men) & (!is.na(romantic_women) | !is.na(romantic_NB)), 0, romantic_men)
  )) %>% 
  mutate(numeric_woman_romantic = if_else(is.na(romantic_men) & is.na(romantic_women) & is.na(romantic_NB), NA_real_, 
                                          if_else(is.na(romantic_women) & (!is.na(romantic_men) | !is.na(romantic_NB)), 0, romantic_women)
  )) %>% 
  mutate(numeric_NB_romantic = if_else(is.na(romantic_men) & is.na(romantic_women) & is.na(romantic_NB), NA_real_, 
                                       if_else(is.na(romantic_NB) & (!is.na(romantic_men) | !is.na(romantic_women)), 0, romantic_NB))) %>% 
  
  mutate(numeric_monogamy = if_else(is.na(mongamous) & is.na(poly_cnm), NA_real_, 
                                    if_else(is.na(mongamous) & !is.na(poly_cnm), 0, mongamous))) %>% 
  mutate(numeric_poly = if_else(is.na(mongamous) & is.na(poly_cnm), NA_real_, 
                                if_else(is.na(poly_cnm) & !is.na(mongamous), 0, poly_cnm))) %>% 
  
  mutate(numeric_commitment = if_else(is.na(commitment) & is.na(casual), NA_real_, 
                                      if_else(is.na(commitment) & !is.na(casual), 0, commitment))) %>% 
  mutate(numeric_casual = if_else(is.na(commitment) & is.na(casual), NA_real_, 
                                  if_else(is.na(casual) & !is.na(commitment), 0, casual)))

#the above section of code creates variables for each gender and orientation slider.
#many people only moved one slider - they moved the other
#so we know they saw the question, but they didn't move the
#other so that value is missing data. Almost always this is
#someone who said "100%" on one slider, and the 0 on the other
#slider is implied. This is part of why it 
#looks like we have missing data here, because folks are 
# answering the question but not inputting a number here. 
# So we will create a new variable that fills in 
#those zeros IF they moved the other slider. 

#### concordance calculation ####
HASH_coded <- HASH_coded %>% # this creates a group-wise variable for concordance of label & attraction patterns, 
  mutate(concordance_label_attraction = case_when(
    orientation_groups == "Exclusively heterosexual" & gender == "Man/male" & numeric_man_attraction == 0 & numeric_NB_attraction == 0 & numeric_woman_attraction > 0 ~ "Exclusively hetero concordant",
    orientation_groups == "Exclusively heterosexual" & gender == "Woman/female" & numeric_woman_attraction == 0 & numeric_NB_attraction == 0 & numeric_man_attraction > 0 ~ "Exclusively hetero concordant",
    
    orientation_groups == "Exclusively heterosexual" & gender == "Man/male" & numeric_man_attraction > 0 & numeric_NB_attraction > 0 & numeric_woman_attraction >= 0 ~ "E het discordant",
    orientation_groups == "Exclusively heterosexual" & gender == "Woman/female" & numeric_woman_attraction > 0 & numeric_NB_attraction > 0 & numeric_man_attraction >= 0 ~ "E het discordant",
    
    orientation_groups == "Mostly heterosexual" & gender == "Man/male" & (numeric_man_attraction > numeric_woman_attraction) | (numeric_man_attraction > 0 & numeric_NB_attraction == 0 & numeric_woman_attraction == 0) ~ "M het discordant - heavy leaning queer",
    orientation_groups == "Mostly heterosexual" & gender == "Man/male" &  (numeric_man_attraction == 0 & numeric_NB_attraction == 0 & numeric_woman_attraction > 0) ~ "M het discordant - heavy lean exclusively het",
    
    orientation_groups == "Mostly heterosexual" & gender == "Woman/female" & numeric_woman_attraction > numeric_man_attraction | (numeric_woman_attraction == 0 & numeric_NB_attraction == 0 & numeric_man_attraction > 0) ~ "M het discordant - heavy lean exclusively het", 
    orientation_groups == "Mostly heterosexual" & gender == "Woman/female" & numeric_man_attraction == 0 & numeric_NB_attraction == 0 & numeric_woman_attraction > 0 ~ "M het discordant - heavy lean queer",
    
    
    orientation_groups == "Bisexual" & gender == "Man/male" & numeric_woman_attraction > numeric_man_attraction | (numeric_woman_attraction == 0 & numeric_man_attraction > 0 & numeric_NB_attraction == 0) ~ "Bi discordant - leaing more queer", 
    orientation_groups == "Bisexual" & gender == "Man/male" & (numeric_woman_attraction > 0 & numeric_man_attraction == 0 & numeric_NB_attraction == 0) ~ "Bi discordant - heavy leaning het",
    
    orientation_groups == "Bisexual" & gender == "Woman/female" & numeric_man_attraction > numeric_woman_attraction | (numeric_woman_attraction == 0 & numeric_man_attraction > 0 & numeric_NB_attraction == 0) ~ "Bi discordant - leaning more het",
    orientation_groups == "Bisexual" & gender == "Woman/female" & (numeric_woman_attraction > 0 & numeric_man_attraction == 0 & numeric_NB_attraction >= 0) ~ "Bi discordant - heavy leaning queer" 
  ))

HASH_concordance <- HASH_coded %>% 
  select(., race_groups, gender, orientation_groups, concordance_label_attraction, relationship_status_groups, everything())



#### predictor variable calculations ####
HASH_coded <- HASH_coded %>% #this calculates the Lesbian Gay Bisexual Identity Scale (LGBIS) and subscales
  mutate(LGBIS_recode_11 = recode_LGBIS_reverse(LGBIS_11), 
         LGBIS_recode_23 = recode_LGBIS_reverse(LGBIS_23), 
         LGBIS_recode_1 = recode_LGBIS(LGBIS_1), 
         LGBIS_recode_2 = recode_LGBIS(LGBIS_2), 
         LGBIS_recode_3 = recode_LGBIS(LGBIS_3), 
         LGBIS_recode_4 = recode_LGBIS(LGBIS_4), 
         LGBIS_recode_5 = recode_LGBIS(LGBIS_5), 
         LGBIS_recode_6 = recode_LGBIS(LGBIS_6), 
         LGBIS_recode_7 = recode_LGBIS(LGBIS_7), 
         LGBIS_recode_8 = recode_LGBIS(LGBIS_8), 
         LGBIS_recode_9 = recode_LGBIS(LGBIS_9), 
         LGBIS_recode_10 = recode_LGBIS(LGBIS_10), 
         LGBIS_recode_12 = recode_LGBIS(LGBIS_12), 
         LGBIS_recode_13 = recode_LGBIS(LGBIS_13), 
         LGBIS_recode_14 = recode_LGBIS(LGBIS_14), 
         LGBIS_recode_15 = recode_LGBIS(LGBIS_15), 
         LGBIS_recode_16 = recode_LGBIS(LGBIS_16), 
         LGBIS_recode_17 = recode_LGBIS(LGBIS_17), 
         LGBIS_recode_18 = recode_LGBIS(LGBIS_18), 
         LGBIS_recode_19 = recode_LGBIS(LGBIS_19), 
         LGBIS_recode_20 = recode_LGBIS(LGBIS_20), 
         LGBIS_recode_21 = recode_LGBIS(LGBIS_21), 
         LGBIS_recode_22 = recode_LGBIS(LGBIS_22), 
         LGBIS_recode_24 = recode_LGBIS(LGBIS_24), 
         LGBIS_recode_25 = recode_LGBIS(LGBIS_25), 
         LGBIS_recode_26 = recode_LGBIS(LGBIS_26), 
         LGBIS_recode_27 = recode_LGBIS(LGBIS_27), 
         LGBIS_acceptance_concerns = LGBIS_recode_5 + LGBIS_recode_9 + LGBIS_recode_16, 
         LGBIS_concealment = LGBIS_recode_1 + LGBIS_recode_4 +LGBIS_recode_19, 
         LGBIS_identity_uncertainty = LGBIS_recode_3 + LGBIS_recode_8 +LGBIS_recode_14 + LGBIS_recode_22, 
         LGBIS_internalized_homophobia = LGBIS_recode_2 + LGBIS_recode_20 + LGBIS_recode_27, 
         LGBIS_difficult_process = LGBIS_recode_12 + LGBIS_recode_17 + LGBIS_recode_23, 
         LGBIS_identity_superiority = LGBIS_recode_7 + LGBIS_recode_10 + LGBIS_recode_18 , 
         LGBIS_identity_affirmation = LGBIS_recode_6 + LGBIS_recode_13 + LGBIS_recode_26, 
         LGBIS_identity_centrality = LGBIS_recode_11 + LGBIS_recode_15 + LGBIS_recode_21 + LGBIS_recode_24 + LGBIS_recode_25, 
         LGBIS_positive = LGBIS_identity_superiority + LGBIS_identity_affirmation + LGBIS_identity_centrality, 
         LGBIS_negative = LGBIS_concealment + LGBIS_identity_uncertainty + LGBIS_internalized_homophobia + LGBIS_difficult_process)


HASH_coded <- HASH_coded %>% #this calculates the Female Sexual Function Index (FSFI), and subscale scores, as well as the dichotomous clinical cutoff scores. 
  mutate(FSFI_recode_1 = recode(FSFI_1, 
                                "Almost always or always" = 5,
                                "Most times (more than half the time)" = 4, 
                                "Sometimes (about half the time)" = 3, 
                                "A few times (less than half the time)" = 2, 
                                "Almost never or never" = 1, 
                                "Not applicable/choose not to respond" = NA_real_), 
         
         FSFI_recode_2 = recode(FSFI_2, 
                                "Very high" = 5,
                                "High" = 4,
                                "Moderate" = 3,
                                "Low" = 2,
                                "Very low or none at all" = 1, 
                                "Not applicable/choose not to respond" = NA_real_
         ), 
         FSFI_recode_3 = recode(FSFI_3, 
                                "Almost always or always" = 5,
                                "Most times (more than half the time)" = 4, 
                                "Sometimes (about half the time)" = 3, 
                                "A few times (less than half the time)" = 2, 
                                "Almost never or never" = 1, 
                                "Not applicable/choose not to respond" = NA_real_), 
         
         FSFI_recode_4 = recode(FSFI_4, 
                                "Very high" = 5,
                                "High" = 4,
                                "Moderate" = 3,
                                "Low" = 2,
                                "Very low or none at all" = 1, 
                                "Not applicable/choose not to respond" = NA_real_
         ), 
         FSFI_recode_5 = recode(FSFI_5, 
                                "Very high confidence" = 5,
                                "High confidence" = 4,
                                "Moderate confidence" = 3, 
                                "Low confidence" = 2,
                                "Very low or no confidence" = 1, 
                                "Not applicable/choose not to respond" = NA_real_
         ), 
         FSFI_recode_6 = recode(FSFI_6, 
                                "Almost always or always" = 5,
                                "Most times (more than half the time)" = 4, 
                                "Sometimes (about half the time)" = 3, 
                                "A few times (less than half the time)" = 2, 
                                "Almost never or never" = 1, 
                                "Not applicable/choose not to respond" = NA_real_),                     
         FSFI_recode_7 = recode(FSFI_7, 
                                "Almost always or always" = 5,
                                "Most times (more than half the time)" = 4, 
                                "Sometimes (about half the time)" = 3, 
                                "A few times (less than half the time)" = 2, 
                                "Almost never or never" = 1, 
                                "Not applicable/choose not to respond" = NA_real_),                     
         FSFI_recode_8 = recode(FSFI_8, 
                                "Extremely difficult or impossible" = 1, 
                                "Very difficult" = 2,
                                "Difficult" = 3,
                                "Slightly difficult" = 4,
                                "Not difficult" = 5, 
                                "Not applicable/choose not to respond" = NA_real_),  
         FSFI_recode_9 = recode(FSFI_9, 
                                "Almost always or always" = 5,
                                "Most times (more than half the time)" = 4, 
                                "Sometimes (about half the time)" = 3, 
                                "A few times (less than half the time)" = 2, 
                                "Almost never or never" = 1, 
                                "Not applicable/choose not to respond" = NA_real_),  
         FSFI_recode_10 = recode(FSFI_10, 
                                 "Extremely difficult or impossible" = 1, 
                                 "Very difficult" = 2,
                                 "Difficult" = 3,
                                 "Slightly difficult" = 4,
                                 "Not difficult" = 5, 
                                 "Not applicable/choose not to respond" = NA_real_), 
         
         FSFI_recode_11 = recode(FSFI_11,
                                 "Almost always or always" = 5,
                                 "Most times (more than half the time)" = 4, 
                                 "Sometimes (about half the time)" = 3, 
                                 "A few times (less than half the time)" = 2, 
                                 "Almost never or never" = 1, 
                                 "Not applicable/choose not to respond" = NA_real_),  
         FSFI_recode_12 = recode(FSFI_12,
                                 "Extremely difficult or impossible" = 1, 
                                 "Very difficult" = 2,
                                 "Difficult" = 3,
                                 "Slightly difficult" = 4,
                                 "Not difficult" = 5, 
                                 "Not applicable/choose not to respond" = NA_real_),
         FSFI_recode_13 = recode(FSFI_13,
                                 "Very satisfied" = 5,
                                 "Moderately satisfied" = 4,
                                 "About equally satisfied and dissatisfied" = 3,
                                 "Moderately dissatisfied" = 2,
                                 "Very dissatisfied" = 1,
                                 "Not applicable/choose not to respond" = NA_real_),
         FSFI_recode_14 = recode(FSFI_14,
                                 "No sexual partner" = NA_real_,
                                 "No partnered sexual activity in the past month" = NA_real_,
                                 "Very satisfied" = 5,
                                 "Moderately satisfied" = 4,
                                 "About equally satisfied and dissatisfied" = 3,
                                 "Moderately dissatisfied" = 2,
                                 "Very dissatisfied" = 1,
                                 "Not applicable/choose not to respond" = NA_real_), 
         FSFI_recode_15 = recode(FSFI_15,
                                 "No sexual partner" = NA_real_,
                                 "Very satisfied" = 5,
                                 "Moderately satisfied" = 4,
                                 "About equally satisfied and dissatisfied" = 3,
                                 "Moderately dissatisfied" = 2,
                                 "Very dissatisfied" = 1,
                                 "Not applicable/choose not to respond" = NA_real_), 
         FSFI_recode_16 = recode(FSFI_16, 
                                 "Almost always or always" = 5,
                                 "Most times (more than half the time)" = 4, 
                                 "Sometimes (about half the time)" = 3, 
                                 "A few times (less than half the time)" = 2, 
                                 "Almost never or never" = 1, 
                                 "Not applicable/choose not to respond" = NA_real_),
         FSFI_recode_17 = recode(FSFI_17,
                                 "Almost always or always" = 5,
                                 "Most times (more than half the time)" = 4, 
                                 "Sometimes (about half the time)" = 3, 
                                 "A few times (less than half the time)" = 2, 
                                 "Almost never or never" = 1, 
                                 "Not applicable/choose not to respond" = NA_real_),
         FSFI_recode_18 = recode(FSFI_18, 
                                 "Very high" = 5,
                                 "High" = 4,
                                 "Moderate" = 3,
                                 "Low" = 2,
                                 "Very low or none at all" = 1, 
                                 "Not applicable/choose not to respond" = NA_real_
         ), 
         FSFI_recode_19 = recode(FSFI_19,
                                 "Very satisfied" = 5,
                                 "Moderately satisfied" = 4,
                                 "About equally satisfied and dissatisfied" = 3,
                                 "Moderately dissatisfied" = 2,
                                 "Very dissatisfied" = 1,
                                 "Not applicable/choose not to respond" = NA_real_))  %>% 
  #this rescales the FSFI to fit the original measure items, and to code the "no partnered activity" choices as NA
  mutate(FSFI_desire = 0.6 * (FSFI_recode_1 + FSFI_recode_2), 
         FSFI_arousal = 0.3 * (FSFI_recode_3 + FSFI_recode_4 + FSFI_recode_5 + FSFI_recode_6), 
         FSFI_lubrication = 0.3 * (FSFI_recode_7 + FSFI_recode_8 + FSFI_recode_9 + FSFI_recode_10), 
         FSFI_orgasm = 0.4 * (FSFI_recode_11 + FSFI_recode_12 + FSFI_recode_13), 
         FSFI_satisfaction = 0.4 * (FSFI_recode_14 + FSFI_recode_15 + FSFI_recode_19), 
         FSFI_pain = 0.4 * (FSFI_recode_17 + FSFI_recode_18 + FSFI_recode_16), 
         FSFI_total = FSFI_desire + FSFI_arousal + FSFI_lubrication + FSFI_orgasm + FSFI_satisfaction + FSFI_pain, 
         FSFI_cutoff = factor(if_else(FSFI_total < 26.55, "Below cutoff", "Above cutoff")))

HASH_coded <- HASH_coded %>% # this creates subscores for the Sexual Definition Survey-Expanded (SDSE), which measures pleasure during different kinds of sexual activity 
  mutate(SDSE_desire = SDSE_1 + SDSE_2,
         SDSE_solo_activity = SDSE_3 + SDSE_4, 
         SDSE_solo = SDSE_desire + SDSE_solo_activity,
         SDSE_partnered_mutual_masturbation = SDSE_5 + SDSE_6 + SDSE_7,
         SDSE_partnered_touchingkissing = SDSE_8 + SDSE_9 + SDSE_11 + SDSE_12 + SDSE_13,
         SDSE_partnered_oral = SDSE_14 + SDSE_15,
         SDSE_partnered_toy = SDSE_16 + SDSE_17,
         SDSE_partnered_penetration = SDSE_18 + SDSE_19, 
         SDSE_partnered = rowSums(select(., contains("SDSE_partnered"))), 
         SDSE_total = SDSE_solo + SDSE_partnered)
# reorder dataset ----

HASH_coded <- HASH_coded %>% 
  select(age, birth_sex, gender, orientation_groups, orientation_other, concordance_label_attraction, LGBIS_identity_uncertainty, relationship_status_groups, FSFI_total, FSFI_cutoff, race_groups, 
         everything ())

view(HASH_coded)
