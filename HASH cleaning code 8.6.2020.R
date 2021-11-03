#call libraries and data sets ----
library(tidyverse)

setwd("~Box/Admin/WISH Lab admin/HASH study/Survey study raw data and code")

HASH_Fall_2019_1 <- read_csv("raw data/HASH_fall2019_1_legend.csv")
HASH_Fall_2019_2 <- read_csv("raw data/HASH_fall2019_2_legend.csv")
HASH_Spring_2020 <-  read_csv("raw data/HASH_spring2020_legend.csv")



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

#create subscales ----

HASH_coded <- HASH_coded %>% #this section creates variables around the unwanted sexual activity variables
 
  mutate(USA_numeric_force = recode_USA(USA_force), 
         USA_numeric_pressure = recode_USA(USA_pressure), 
         USA_numeric_intoxication = recode_USA(USA_intoxication), 
         USA_numeric_survival = recode_USA(USA_survival), 
         USA_numeric_breakup = recode_USA(USA_breakup), 
         USA_numeric_withdrew_consent = recode_USA(USA_withdrew_consent)) %>% 
  #this code replaces the values of the unwanted sex variable to numeric, and the "choose not to answer" as NA

  mutate(USA_total = rowSums(select(., contains("USA_numeric")), na.rm = TRUE),
         
         #this generates a total score across USA items, which is for severity and also useful for creating the 
         #any history score below
         
         USA_total = if_else((is.na(USA_force) & is.na(USA_pressure) &
                                is.na(USA_survival) & is.na(USA_intoxication) & 
                                is.na(USA_breakup) & is.na(USA_withdrew_consent)), 
                             NA_real_, USA_total),
         #that above bit of code is necessary to ensure that the initial dropping of NAs in the rowSums
         #command doesn't add in a zero for folks who have NAs across *all* of the rows, only that
         #it ignores NAs if they didn't answer one of the items
         
         USA_any = factor(if_else(USA_total > 0, 1, 0))) %>% 
  #this creates a dichotomous score (any vs none)
  
  mutate(USA_coerced_total = rowSums(select(., c("USA_numeric_pressure", "USA_numeric_breakup", "USA_numeric_withdrew_consent")), na.rm = TRUE), 
         
         # this creates a new variable that indicates total score across all coercive USA items
         
         USA_coerced_total = if_else((is.na(USA_pressure) &
                                        is.na(USA_breakup) & 
                                        is.na(USA_withdrew_consent)), 
                                     NA_real_, USA_coerced_total),
         #that above bit of code is necessary to ensure that the initial dropping of NAs in the rowSums
         #command doesn't add in a zero for folks who have NAs across *all* of the rows, only that
         #it ignores NAs if they didn't answer one of the items
         
         USA_coerced_any = factor(if_else(USA_coerced_total > 0, 1, 0))) %>% 
  #this creates an any vs. none category for the coercive items specifically
  
  
  mutate(USA_coerced_repeated = 
           if_else(USA_coerced_total > 1, 2,
                   if_else(USA_coerced_total == 1, 1,
                           if_else(is.na(USA_coerced_total), NA_real_, 0)))) %>% 
  # this creates a simple categorical variable for coercive USA- none (0), 1 time (1) or more than 1 time (2)


mutate(USA_repeated = 
         if_else(USA_total > 1, 2,
                 if_else(USA_total == 1, 1,
                         if_else(is.na(USA_total), NA_real_, 0)))) %>% 
# this creates a simple categorical variable for  USA- none (0), 1 time (1) or more than 1 time (2)

mutate(USA_intox_type = case_when(
  USA_numeric_intoxication >= 1 ~ "Intoxication USA", 
  USA_numeric_intoxication == 0 & USA_total > 0 ~ "Non-intoxication USA", 
  USA_total == 0 ~ "No USA"))

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

HASH_coded <- HASH_coded %>% # this creates a group-wise variable for orientation of attraction patterns, 
  mutate(orientation_sexual_attraction = case_when(
             numeric_man_attraction < 20 & numeric_woman_attraction < 20 & numeric_NB_attraction < 20 ~ "Ace",
             gender == "Man/male" & numeric_man_attraction == 0 & numeric_NB_attraction == 0 & numeric_woman_attraction > 0 ~ "Exclusively hetero attractions",
             gender == "Woman/female" & numeric_woman_attraction == 0 & numeric_NB_attraction == 0 & numeric_man_attraction > 0 ~ "Exclusively hetero attractions",
             gender == "Man/male" & numeric_man_attraction == 100 & numeric_NB_attraction == 0 & numeric_woman_attraction == 0 ~ "Exclusively same sex attractions",
             gender == "Woman/female" & numeric_woman_attraction == 100 & numeric_NB_attraction == 0 & numeric_man_attraction == 0 ~ "Exclusively same sex attractions",
             gender == "Man/male" & numeric_woman_attraction >= 20 & (numeric_NB_attraction > 20 | numeric_man_attraction > 20) ~ "Bi or pansexual attractions", 
             gender == "Woman/female" & numeric_man_attraction >= 20 & (numeric_NB_attraction > 20 | numeric_woman_attraction > 20) ~ "Bi or pansexual attractions", 
             gender == "Man/male" & numeric_NB_attraction >= 20 & (numeric_woman_attraction > 20 | numeric_man_attraction > 20) ~ "Bi or pansexual attractions", 
             gender == "Woman/female" & numeric_NB_attraction >= 20 & (numeric_man_attraction > 20 | numeric_woman_attraction > 20) ~ "Bi or pansexual attractions", 
             gender == "Man/male" & numeric_woman_attraction >= 80 & (numeric_NB_attraction > 0 | numeric_man_attraction > 0) ~ "Mostly hetero attractions", 
             gender == "Woman/female" & numeric_man_attraction >= 80 & (numeric_NB_attraction > 0 | numeric_woman_attraction > 0) ~ "Mostly hetero attractions", 
             gender == "Man/male" & numeric_man_attraction >= 80 & (numeric_NB_attraction > 0 | numeric_woman_attraction > 0) ~ "Mostly same sex attractions", 
             gender == "Woman/female" & numeric_woman_attraction >= 80 & (numeric_NB_attraction > 0 | numeric_man_attraction > 0) ~ "Mostly same sex attractions"
           ))

HASH_coded$orientation_sexual_attraction = relevel(HASH_coded$orientation_sexual_attraction, ref = "Exclusively hetero attractions")



HASH_coded <- HASH_coded %>% #this calculates the Heterosexist Harrassment, Rejection, and Discrimination Scale (HHRD) and subscales
 mutate(HHRD_1_recode = recode_HHRD(HHRD_1), 
        HHRD_2_recode = recode_HHRD(HHRD_2), 
        HHRD_3_recode = recode_HHRD(HHRD_3), 
        HHRD_4_recode = recode_HHRD(HHRD_4), 
        HHRD_5_recode = recode_HHRD(HHRD_5), 
        HHRD_6_recode = recode_HHRD(HHRD_6),
        HHRD_7_recode = recode_HHRD(HHRD_7), 
        HHRD_8_recode = recode_HHRD(HHRD_8), 
        HHRD_9_recode = recode_HHRD(HHRD_9), 
        HHRD_10_recode = recode_HHRD(HHRD_10), 
        HHRD_11_recode = recode_HHRD(HHRD_11), 
        HHRD_12_recode = recode_HHRD(HHRD_12), 
        HHRD_13_recode = recode_HHRD(HHRD_13), 
        HHRD_14_recode = recode_HHRD(HHRD_14), 
        HHRD_harrass = HHRD_1_recode + HHRD_2_recode + HHRD_3_recode + HHRD_4_recode + HHRD_5_recode + HHRD_6_recode + HHRD_7_recode, 
        HHRD_work_school_discrim = HHRD_8_recode + HHRD_9_recode + HHRD_10_recode + HHRD_11_recode, 
        HHRD_other_discrim = HHRD_12_recode + HHRD_13_recode + HHRD_14_recode, 
        HHRD_total = HHRD_harrass + HHRD_work_school_discrim + HHRD_other_discrim)
        
   
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
    
  
HASH_coded <- HASH_coded %>% #this creates a screener for possible drug & alcohol use disorders
  mutate(drug_dx_screen = if_else(
    (is.na(DSM_drug_1) & is.na(DSM_drug_2) & is.na(DSM_drug_3) & is.na(DSM_drug_4) & is.na(DSM_drug_5) & is.na(DSM_drug_6) & is.na(DSM_drug_7) & is.na(DSM_drug_8)), NA_character_, 
    if_else(
   (DSM_drug_1 == "Yes" | DSM_drug_2 == "Yes" | DSM_drug_3 == "Yes" | DSM_drug_4 == "Yes" | 
      DSM_drug_5 == "Yes" | DSM_drug_6 == "Yes" | DSM_drug_7 == "Yes" | DSM_drug_8 == "Yes"), "Possible drug use disorder", "No drug use disorder"))) %>% 
 
   mutate(alcohol_dx_screen = if_else(
    (is.na(DSM_alcohol_1) & is.na(DSM_alcohol_2) & is.na(DSM_alcohol_3) & is.na(DSM_alcohol_4) & is.na(DSM_alcohol_5) & is.na(DSM_alcohol_6) & is.na(DSM_alcohol_7) & is.na(DSM_alcohol_8) & is.na(DSM_alcohol_9) & is.na(DSM_alcohol_10)), NA_character_, 
    if_else(
      (DSM_alcohol_1 == "Yes" | DSM_alcohol_2 == "Yes" | DSM_alcohol_3 == "Yes" | DSM_alcohol_4 == "Yes" | 
         DSM_alcohol_5 == "Yes" | DSM_alcohol_6 == "Yes" | DSM_alcohol_7 == "Yes" | DSM_alcohol_8 == "Yes" 
       | DSM_alcohol_9 == "Yes" | DSM_alcohol_10 == "Yes"), "Possible alcohol use disorder", "No alcohol use disorder")))

HASH_coded <-  HASH_coded %>% #this calculates the Patient Health Questionnaire (PHQ9), a depression symptom scale, and the General Anxiety Disorder scale (GAD7), an anxiety symptom scale
  mutate(PHQ9_recode_1 = recode_PHQ9(PHQ9_1), 
         PHQ9_recode_2 = recode_PHQ9(PHQ9_2), 
         PHQ9_recode_3 = recode_PHQ9(PHQ9_3), 
         PHQ9_recode_4 = recode_PHQ9(PHQ9_4), 
         PHQ9_recode_5 = recode_PHQ9(PHQ9_5), 
         PHQ9_recode_6 = recode_PHQ9(PHQ9_6), 
         PHQ9_recode_7 = recode_PHQ9(PHQ9_7), 
         PHQ9_recode_8 = recode_PHQ9(PHQ9_8), 
         PHQ9_recode_9 = recode_PHQ9(PHQ9_9), 
         PHQ9_total = rowSums(select(., contains("PHQ9_recode")))
           ) %>% 
  mutate(GAD7_recode_1 = recode_GAD7(GAD7_1), 
         GAD7_recode_2 = recode_GAD7(GAD7_2), 
         GAD7_recode_3 = recode_GAD7(GAD7_3), 
         GAD7_recode_4 = recode_GAD7(GAD7_4), 
         GAD7_recode_5 = recode_GAD7(GAD7_5), 
         GAD7_recode_6 = recode_GAD7(GAD7_6), 
         GAD7_recode_7 = recode_GAD7(GAD7_7), 
         GAD7_total = rowSums(select(., contains("GAD7_recode")))
  )

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
  
  #this creates the continous subscale scores, along with the dichotomous factor clinical cutoff. 
 
HASH_coded <-  HASH_coded %>% # this creates the Sexual Desire Inventory (SDI) with solitary, partnered, and attractive other subscales as well as total score
  mutate(SDI_p1 = recode(SDI_2, 
                                     "Not at all" = 0,
                                     "Once a month" = 1, 
                                     "Every two weeks" = 2, 
                                     "Once a week" = 3,
                                     "Twice a week" = 4, 
                                     "3 to 4 times a week" = 5, 
                                     "Once a day" = 6, 
                                     "More than once a day" = 7, 
                                     "Choose not to respond/Not applicable" = NA_real_),


         SDI_s1 = recode(SDI_1, 
                                 "Not at all" = 0,
                                 "Once a month" = 1, 
                                 "Every two weeks" = 2, 
                                 "Once a week" = 3,
                                 "Twice a week" = 4, 
                                 "3 to 4 times a week" = 5, 
                                 "Once a day" = 6, 
                                 "More than once a day" = 7, 
                                 "Choose not to respond/Not applicable" = NA_real_)
  ) %>% 
  mutate(SDI_p2 = recode(SDI_3, 
                         "Not at all" = 0,
                         "Once or twice a month" = 1, 
                         "Once a week" = 2, 
                         "Twice a week" = 3,
                         "3-4 times a week" = 4, 
                         "Once a day" = 5, 
                         "A couple of times a day" = 6, 
                         "Many times a day" = 7, 
                         "Not applicable/choose not to respond" = NA_real_)) %>% 
    mutate(SDI_partnered = SDI_p1 + SDI_p2 + SDI_4 + SDI_6 + SDI_8, 
         SDI_solitary = SDI_s1 + SDI_5 + SDI_7 +  SDI_9, 
         SDI_attractive_other = SDI_10 + SDI_11, 
         SDI_total = SDI_partnered + SDI_solitary + SDI_attractive_other) 
  #this creates the three factors for the SDI, according to the Mark et al factor analysis

HASH_coded <-  HASH_coded %>% # this creates the Sexual Sensation Seeking Scale (SSSS)
  mutate(SSSS_recode_1 = recode_SSSS(SSSS_1), 
         SSSS_recode_2 = recode_SSSS(SSSS_2), 
         SSSS_recode_3 = recode_SSSS(SSSS_3), 
         SSSS_recode_4 = recode_SSSS(SSSS_4), 
         SSSS_recode_5 = recode_SSSS(SSSS_5), 
         SSSS_recode_6 = recode_SSSS(SSSS_6), 
         SSSS_recode_7 = recode_SSSS(SSSS_7), 
         SSSS_recode_8 = recode_SSSS(SSSS_8), 
         SSSS_recode_9 = recode_SSSS(SSSS_9), 
         SSSS_recode_10 = recode_SSSS(SSSS_10), 
         SSSS_recode_11 = recode_SSSS(SSSS_11))  %>% 
 mutate(SSSS_total = rowSums(select(., contains("SSSS_recode"))))
        
HASH_coded <-  HASH_coded %>% #this creates the Sexual Excitation and Sexual Inhibition Scale (SESI), with the subscales and total scale scores
  mutate(SESI_recode_1 = recode_SESI(SESI_1), 
         SESI_recode_2 = recode_SESI(SESI_2), 
         SESI_recode_3 = recode_SESI(SESI_3), 
         SESI_recode_4 = recode_SESI(SESI_4), 
         SESI_recode_5 = recode_SESI(SESI_5), 
         SESI_recode_6 = recode_SESI(SESI_6), 
         SESI_recode_7 = recode_SESI(SESI_7), 
         SESI_recode_8 = recode_SESI(SESI_8), 
         SESI_recode_9 = recode_SESI(SESI_9), 
         SESI_recode_10 = recode_SESI(SESI_10), 
         SESI_recode_11 = recode_SESI(SESI_11), 
         SESI_recode_12 = recode_SESI(SESI_12), 
         SESI_recode_13 = recode_SESI(SESI_13), 
         SESI_recode_14 = recode_SESI(SESI_14), 
         SESI_recode_15 = recode_SESI(SESI_15), 
         SESI_recode_16 = recode_SESI(SESI_16), 
         SESI_recode_17 = recode_SESI(SESI_17), 
         SESI_recode_18 = recode_SESI(SESI_18), 
         SESI_recode_19 = recode_SESI(SESI_19), 
         SESI_recode_20 = recode_SESI(SESI_20), 
         SESI_recode_21 = recode_SESI(SESI_21), 
         SESI_recode_22 = recode_SESI(SESI_22), 
         SESI_recode_23 = recode_SESI(SESI_23), 
         SESI_recode_24 = recode_SESI(SESI_24), 
         SESI_recode_25 = recode_SESI(SESI_25), 
         SESI_recode_26 = recode_SESI_reverse(SESI_26), 
         SESI_recode_27 = recode_SESI(SESI_27), 
         SESI_recode_28 = recode_SESI(SESI_28), 
         SESI_recode_29 = recode_SESI(SESI_29), 
         SESI_recode_30 = recode_SESI(SESI_30), 
         SESI_recode_31 = recode_SESI(SESI_31), 
         SESI_recode_32 = recode_SESI(SESI_32), 
         SESI_recode_33 = recode_SESI_reverse(SESI_33), 
         SESI_recode_34 = recode_SESI(SESI_34), 
         SESI_recode_35 = recode_SESI_reverse(SESI_35), 
         SESI_recode_36 = recode_SESI(SESI_36) 
         ) %>% 
  mutate(SESI_excitation_arousability = SESI_recode_34 + SESI_recode_13 + SESI_recode_28 + SESI_recode_12 + SESI_recode_30 + SESI_recode_18 + SESI_recode_25 + SESI_recode_1 + SESI_recode_15, 
         SESI_excitation_power = SESI_recode_5 + SESI_recode_29 + SESI_recode_35 + SESI_recode_32, 
         SESI_excitation_smell = SESI_recode_2 + SESI_recode_14, 
         SESI_excitation_partner = SESI_recode_4 + SESI_recode_16 + SESI_recode_21 + SESI_recode_19, 
         SESI_inhibition_relationship = SESI_recode_11 + SESI_recode_8 + SESI_recode_3 + SESI_recode_10 + SESI_recode_6 + SESI_recode_24, 
         SESI_inhibition_contingency = SESI_recode_27 + SESI_recode_22 + SESI_recode_23, 
         SESI_inhibition_dysfunction = SESI_recode_7 + SESI_recode_9 + SESI_recode_36 + SESI_recode_17, 
         SESI_excitation_total = SESI_excitation_arousability + SESI_excitation_partner + SESI_excitation_power + SESI_excitation_smell, 
         SESI_inhibition_total = SESI_inhibition_contingency + SESI_inhibition_dysfunction + SESI_inhibition_relationship
         
           )
  
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


      
 HASH_coded <-  HASH_coded %>% #this creates the total score for the Social Reward Questionnaire (SRQ), sexual relationships subscale
   mutate(SRQ_recode_1 = recode_SRQ(SRQ_1), 
          SRQ_recode_2 = recode_SRQ(SRQ_2),
          SRQ_recode_3 = recode_SRQ(SRQ_3),
          SRQ_recode_4 = recode_SRQ(SRQ_4), 
          SRQ_total = SRQ_recode_1 + SRQ_recode_2 + SRQ_recode_3 + SRQ_recode_4)
 
 HASH_coded <- HASH_coded %>% # this calculates the objectification and enjoyment of sexualization scales and subscales
   mutate(OBCS_surveillance = 56 - (OBCS_1 + OBCS_2 + OBCS_3 + OBCS_4 + (7 - OBCS_5) + (7 - OBCS_6) + OBCS_7 + OBCS_8),
          OBCS_bodyshame = OBCS_9 + OBCS_10 + OBCS_11 + OBCS_12 + (7 - OBCS_13) + OBCS_14 + (7 - OBCS_15) + OBCS_16, 
          OBCS_total = OBCS_surveillance + OBCS_bodyshame) %>% 
   mutate(ISOS_bodyeval = ISOS_1 + ISOS_2 + ISOS_3 + ISOS_4 + ISOS_5 + ISOS_6 + ISOS_7 + ISOS_8 + ISOS_9 + ISOS_10 + ISOS_11, 
          ISOS_unwanted_advances = ISOS_12 + ISOS_13 + ISOS_14 + ISOS_15,
          ISOS_total = ISOS_bodyeval + ISOS_unwanted_advances
            ) %>% 
   mutate(ESS_total = ESS_1 + ESS_2 + ESS_3 + ESS_4 + ESS_5 + ESS_6 + ESS_7 + ESS_8)
 

 
 HASH_coded <- HASH_coded %>% #This creates the Brief Consequences & Expectancies of Alcohol (BCEOA) and Drugs (BCEOD) scales and subscales (measuring alcohol and drug use expectancies)
   mutate(BCEOA_recode_1 = recode_BCEO(BCEOA_1),
          BCEOA_recode_2 = recode_BCEO(BCEOA_2),
          BCEOA_recode_3 = recode_BCEO(BCEOA_3),
          BCEOA_recode_4 = recode_BCEO(BCEOA_4),
          BCEOA_recode_5 = recode_BCEO(BCEOA_5),
          BCEOA_recode_6 = recode_BCEO(BCEOA_6),
          BCEOA_recode_7 = recode_BCEO(BCEOA_7),
          BCEOA_recode_8 = recode_BCEO(BCEOA_8),
          BCEOA_recode_9 = recode_BCEO(BCEOA_9),
          BCEOA_recode_10 = recode_BCEO(BCEOA_10),
          BCEOA_recode_11 = recode_BCEO(BCEOA_11),
          BCEOA_recode_12 = recode_BCEO(BCEOA_12),
          BCEOA_recode_13 = recode_BCEO(BCEOA_13),
          BCEOA_recode_14 = recode_BCEO(BCEOA_14),
          BCEOD_recode_1 = recode_BCEO(BCEOD_1),
          BCEOD_recode_2 = recode_BCEO(BCEOD_2),
          BCEOD_recode_3 = recode_BCEO(BCEOD_3),
          BCEOD_recode_4 = recode_BCEO(BCEOD_4),
          BCEOD_recode_5 = recode_BCEO(BCEOD_5),
          BCEOD_recode_6 = recode_BCEO(BCEOD_6),
          BCEOD_recode_7 = recode_BCEO(BCEOD_7),
          BCEOD_recode_8 = recode_BCEO(BCEOD_8),
          BCEOD_recode_9 = recode_BCEO(BCEOD_9),
          BCEOD_recode_10 = recode_BCEO(BCEOD_10),
          BCEOD_recode_11 = recode_BCEO(BCEOD_11),
          BCEOD_recode_12 = recode_BCEO(BCEOD_12),
          BCEOD_recode_13 = recode_BCEO(BCEOD_13),
          BCEOD_recode_14 = recode_BCEO(BCEOD_14)) %>% 
   mutate(BCEOA_risk = (BCEOA_recode_1 + BCEOA_recode_4 + BCEOA_recode_10)/3, 
          BCEOA_courage = (BCEOA_recode_8 + BCEOA_recode_5)/3,
          BCEOA_social = (BCEOA_recode_11 + BCEOA_recode_13)/2,
          BCEOA_selfperception = BCEOA_recode_14,  
          BCEOA_impairment = (BCEOA_recode_2 + BCEOA_recode_3)/2,  
          BCEOA_sexual = (BCEOA_recode_6 + BCEOA_recode_7)/2,  
          BCEOA_relax = (BCEOA_recode_9 + BCEOA_recode_12)/2,
          BCEOD_risk = (BCEOD_recode_5 + BCEOD_recode_9 + BCEOD_recode_13)/3, 
          BCEOD_courage = (BCEOD_recode_3 + BCEOD_recode_7)/2,
          BCEOD_social = (BCEOD_recode_1 + BCEOD_recode_11)/2,
          BCEOD_selfperception = BCEOD_recode_10,  
          BCEOD_impairment = (BCEOD_recode_6 + BCEOD_recode_12) / 2,  
          BCEOD_sexual = (BCEOD_recode_2 + BCEOD_recode_14)/2,  
          BCEOD_relax = (BCEOD_recode_4 + BCEOD_recode_8)/2, 
          BCEOA_factor1_risk = (BCEOA_risk + BCEOA_courage + BCEOA_social)/3, 
          BCEOA_factor2_impair = (BCEOA_selfperception + BCEOA_impairment)/2, 
          BCEOA_factor3_sexual = BCEOA_sexual,
          BCEOA_factor4_relax = BCEOA_relax, 
          BCEOD_factor1_risk = (BCEOD_risk + BCEOD_courage + BCEOD_social)/2, 
          BCEOD_factor2_impair = (BCEOD_selfperception + BCEOD_impairment)/2, 
          BCEOD_factor3_sexual = BCEOD_sexual,
          BCEOD_factor4_relax = BCEOD_relax
   )
   
HASH_coded <- HASH_coded %>% #this creates the items for sex-linked drug use (SDLU)
  mutate(SLDU_excitation = factor(monthly_drug_excitation, exclude = "Not applicable/ choose not to respond", 
                                   levels = c("Not at all", "1 - 3 times per month", "Once a week",
                                              "2 - 3 times a week", "More than 3 times a week"), ordered = TRUE), 
         SLDU_inhibition = factor(monthly_drug_inhibition, exclude = "Not applicable/ choose not to respond", 
                                   levels = c("Not at all", "1 - 3 times per month", "Once a week",
                                              "2 - 3 times a week", "More than 3 times a week"), ordered = TRUE), 
         SLDU_orgasm = factor(monthly_drug_orgasm, exclude = "Not applicable/ choose not to respond", 
                               levels = c("Not at all", "1 - 3 times per month", "Once a week",
                                          "2 - 3 times a week", "More than 3 times a week"), ordered = TRUE), 
         SLDU_total = as.numeric(SLDU_excitation) + as.numeric(SLDU_inhibition) + as.numeric(SLDU_orgasm)
  )

 

# reorder dataset ----

HASH_coded <- HASH_coded %>% 
  select(age, birth_sex, gender, orientation_groups, orientation_sexual_attraction, race_groups, relationship_status_groups, numeric_masculine:numeric_casual, USA_any, USA_total, USA_coerced_any, USA_coerced_total, USA_intox_type, USA_child, SLDU_excitation:SLDU_total, HHRD_harrass:HHRD_total, LGBIS_acceptance_concerns:LGBIS_negative, drug_dx_screen, alcohol_dx_screen, PHQ9_total, GAD7_total, FSFI_desire:FSFI_cutoff, SDI_partnered:SDI_total, SESI_excitation_arousability:SESI_inhibition_total, SDSE_desire:SDSE_total, BCEOA_factor1_risk:BCEOD_factor4_relax, OBCS_surveillance:ESS_total,
         everything())

view(HASH_coded)

