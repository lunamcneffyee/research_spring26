library(tidyverse)
library(readr)
library(arsenal)
library(stringr)

cycles <- read_csv("data/Luna Cycle Ratings - Merged ratings.csv")
somatic_bl <- read_csv("data/Luna Somatic BL - combined bl.csv")
somatic_wd1 <- read_csv("data/Luna Somatic WD 1 - combined wd1.csv")
somatic_wd2 <- read_csv("data/Luna Somatic WD 2 - combined wd2.csv")
marbles <- read_csv("data/Marble Burying - all.csv")

#cycle ratings
cycles_long <- cycles %>%
  pivot_longer(
    cols=c("BL1","BL2","WD1 Friday","WD1 Saturday",
           "WD2 Friday","WD2 Saturday"),
    names_to="day",
    values_to="stage")%>%
  select(-Notes)

#compare my ratings with maya's

luna_ratings <- cycles_long %>%
  filter(Rater=="Luna")
maya_ratings <- cycles_long %>%
  filter(Rater=="Maya")

differences <- summary(comparedf(luna_ratings,maya_ratings))
differences <- differences[[7]]
differences <- differences %>%
  filter(!var.x=="Rater")%>%
  rename(luna_rating=values.x,
         maya_rating=values.y)%>%
  select(luna_rating,maya_rating,row.x)

for_comparison <- luna_ratings %>%
  mutate(row.x=row_number())%>%
  select(subject,day,row.x)

for_comparison <- left_join(differences,for_comparison, by="row.x")

#write_csv(for_comparison,"data/comparison.csv")

#EPM 
epm_bl_luna <- read_csv("data/Luna BL EPM - summary.csv")
epm_bl_maya <- read_csv("data/Maya WD2 EPM - summary.csv")
epm_wd_luna <- read_csv("data/Maya BL EPM - summary.csv")
epm_wd_maya <- read_csv("data/Maya WD2 EPM - summary.csv")

#Epm standardize behavior
epm_bl_luna <- epm_bl_luna %>%
  mutate(behavior = case_when(
    behavior == "E = GROOMING" ~ "grooming",
    behavior == "R = REARING" ~ "rearing",
    behavior == "S = HEAD DIPPING" ~ "dip"
  ))

epm_bl_maya <- epm_bl_maya %>%
  mutate(behavior = case_when(
    behavior == "G = GROOMING" ~ "grooming",
    behavior == "R = REARING" ~ "rearing",
    behavior == "H = HEAD DIP" ~ "dip"
  ))


epm_behavior_bl_luna <- epm_bl_luna %>%
  select(subject,behavior,behavior_frequency,behavior_time,rater)
epm_behavior_bl_maya <- epm_bl_maya %>% 
  select(subject,behavior,behavior_frequency,behavior_time,rater)

epm_behavior_bl <- left_join(epm_behavior_bl_luna,epm_behavior_bl_maya,by=c("subject","behavior"))
