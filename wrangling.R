library(tidyverse)
library(readr)
library(arsenal)
library(stringr)
library(irr)
library(rstatix)
library(ez)
library(lme4)
library(lmerTest)
library(effectsize)
library(emmeans)
library(papaja)

cycles <- read_csv("raw data/Luna Cycle Ratings - Merged ratings.csv")
somatic_bl <- read_csv("raw data/Luna Somatic BL - combined bl.csv")
somatic_wd1 <- read_csv("raw data/Luna Somatic WD 1 - combined wd1.csv")
somatic_wd2 <- read_csv("raw data/Luna Somatic WD 2 - combined wd2.csv")

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
epm_bl_luna <- read_csv("raw data/Luna BL EPM - summary.csv")
epm_bl_maya <- read_csv("raw data/Maya BL EPM - summary.csv")

epm_wd_luna <- read_csv("raw data/Luna WD2 EPM - summary.csv")
epm_wd_maya <- read_csv("raw data/Maya WD2 EPM - summary.csv")

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
    behavior == "H = HEAD DIPPING" ~ "dip"
  ))


epm_behavior_bl_luna <- epm_bl_luna %>%
  select(subject,behavior,behavior_frequency,behavior_time,rater)
epm_behavior_bl_maya <- epm_bl_maya %>% 
  select(subject,behavior,behavior_frequency,behavior_time,rater)

epm_behavior_bl <- left_join(epm_behavior_bl_luna,epm_behavior_bl_maya,by=c("subject","behavior"))

epm_behavior_bl <- epm_behavior_bl %>%
  mutate(
    mean_count = if_else(
      behavior == "grooming",
      behavior_frequency.x,
      (behavior_frequency.x + behavior_frequency.y) / 2
    ),
    mean_time = if_else(
      behavior == "grooming",
      behavior_time.x,
      (behavior_time.x + behavior_time.y) / 2
    )
  )%>%
  mutate(stage="bl")

#same for wd
epm_wd_luna <- epm_wd_luna %>%
  mutate(behavior = case_when(
    behavior == "E = GROOMING" ~ "grooming",
    behavior == "R = REARING" ~ "rearing",
    behavior == "S = HEAD DIPPING" ~ "dip"
  ))

epm_wd_maya <- epm_wd_maya %>%
  mutate(behavior = case_when(
    behavior == "G = GROOMING" ~ "grooming",
    behavior == "R = REARING" ~ "rearing",
    behavior == "H = HEAD DIP" ~ "dip"
  ))


epm_behavior_wd_luna <- epm_wd_luna %>%
  select(subject,behavior,behavior_frequency,behavior_time,rater)
epm_behavior_wd_maya <- epm_wd_maya %>% 
  select(subject,behavior,behavior_frequency,behavior_time,rater)

epm_behavior_wd <- left_join(epm_behavior_wd_luna,epm_behavior_wd_maya,by=c("subject","behavior"))

epm_behavior_wd <- epm_behavior_wd %>%
  mutate(
    mean_count = if_else(
      behavior == "grooming",
      behavior_frequency.x,
      (behavior_frequency.x + behavior_frequency.y) / 2
    ),
    mean_time = if_else(
      behavior == "grooming",
      behavior_time.x,
      (behavior_time.x + behavior_time.y) / 2
    )
  )%>%
  mutate(stage="wd")

#inter-rater reliability
epm_behavior_wd %>%
  group_by(behavior) %>%
  group_modify(~ {
    data.frame(
      icc = icc(
        .x[, c("behavior_frequency.x", "behavior_frequency.y")],
        model = "twoway",
        type = "agreement",
        unit = "average"
      )$value
    )
  })

#write_csv(epm_behavior_bl,"data/epm_bl_behaviors.csv")
#write_csv(epm_behavior_wd,"data/epm_wd_behaviors.csv")



#Clean somatic
somatic_bl <- somatic_bl %>%
  mutate(ID=str_replace(ID,"_BL$",""))%>%
  mutate(Behavior=str_replace(Behavior,"^[:alpha:] = ","")) %>%
  mutate(Behavior=str_to_lower(Behavior))%>%
  drop_na(Behavior)%>%
  mutate(Behavior=
           case_when(
             Behavior=="other agitation" ~ "agitation other",
             TRUE ~ Behavior
           ))

somatic_bl_summary <- somatic_bl %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time))%>%
  pivot_wider(
    names_from=Behavior,values_from=time
  )%>%
  mutate(stage="bl")
  
somatic_wd1 <- somatic_wd1 %>%
  mutate(ID=str_replace(ID,"_WD1$",""))%>%
  mutate(Behavior=str_replace(Behavior,"^[:alpha:] = ","")) %>%
  mutate(Behavior=str_to_lower(Behavior)) %>%
  drop_na(Behavior) %>%
  mutate(Behavior=
           case_when(
             Behavior=="other agitation" ~ "agitation other",
             TRUE ~ Behavior
           ))

somatic_wd1_summary <- somatic_wd1 %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time))%>%
  pivot_wider(
    names_from=Behavior,values_from=time
  )%>%
  mutate(stage="wd1")

somatic_wd2 <- somatic_wd2 %>%
  mutate(ID=str_replace(ID,"_WD2$",""))%>%
  mutate(Behavior=str_replace(Behavior,"^[:alpha:] = ","")) %>%
  mutate(Behavior=str_to_lower(Behavior)) %>%
  drop_na(Behavior) %>%
  mutate(Behavior=
           case_when(
             Behavior=="other agitation" ~ "agitation other",
             TRUE ~ Behavior
           ))

somatic_wd2_summary <- somatic_wd2 %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time)) %>%
  pivot_wider(
    names_from=Behavior,values_from=time
  )%>%
  mutate(stage="wd2")

stage_effect <- bind_rows(list(somatic_bl_summary,somatic_wd1_summary,somatic_wd2_summary))

stage_effect <- stage_effect %>%
  replace(is.na(.),0) %>%
  mutate(estrogen=
           case_when(
             ID=="EM02" ~ "E2",
             ID=="EM08" ~ "E2",
             ID=="EM11" ~ "E2",
             .default="no E2"))

friedman_somatic_results <- data.frame(behavior = character(),
                      statistic = double(),
                      parameter = double(),
                      p_value = double())
cols <- c("chewing","grooming","locomotion","reared sniff",
                 "digging", "jumping", "scratching", "agitation other",
                 "eating", "shaking", "twitching")
i <- 1

for(col in cols) {
  temp <- friedman.test(y=stage_effect[[col]],
                        groups=stage_effect$stage,
                        blocks=stage_effect$ID)
  friedman_somatic_results[i,"behavior"] <- {{col}}
  friedman_somatic_results[i ,"statistic"] <- temp$statistic
  friedman_somatic_results[i, "p_value"] <- temp$p.value
  i <- i+1
}

#IMPORTANT TABLE
friedman_somatic_results <- friedman_somatic_results %>%
  select(-parameter)
  
#post-hoc somatic results:
for_wilcox_wd2 <- stage_effect %>%
  filter(stage=="wd2")

for_wilcox_bl <- for_wilcox_wd %>%
  filter(stage=="bl")

for_wilcox_wd <- for_wilcox_wd %>% 
  filter(stage=="wd1")

post_hoc_somatic <- data.frame(
  behavior=character(),
  bl_wd1=double(),
  bl_wd2=double(),
  wd1_wd2=double()
  )

wilcox<-wilcox.test(for_wilcox_bl$shaking,for_wilcox_wd$shaking,paired=TRUE)
post_hoc_somatic <- data.frame(matrix(nrow=5,ncol=4))
colnames(post_hoc_somatic) <- c("behavior","bl_wd1","bl_wd2","wd1_wd2")
rows <- c("reared sniff","jumping","digging","agitation other","shaking")


i <- 1

for (i in rows) {
  tempblwd1 <- wilcox.test(for_wilcox_bl[[i]],
                         for_wilcox_wd[[i]],paired=TRUE)
  post_hoc_somatic[i, "bl_wd1"] <- tempblwd1$p.value
}

for (i in rows) {
  tempblwd2 <- wilcox.test(for_wilcox_bl[[i]],
                           for_wilcox_wd2[[i]],paired=TRUE)
  post_hoc_somatic[i, "bl_wd2"] <- tempblwd2$p.value
}

for (i in rows) {
  tempblwdwd <- wilcox.test(for_wilcox_wd[[i]],
                           for_wilcox_wd2[[i]],paired=TRUE)
  post_hoc_somatic[i, "wd1_wd2"] <- tempblwdwd$p.value
}

post_hoc_somatic <- post_hoc_somatic %>%
  filter(!is.na(bl_wd1))%>%
  rownames_to_column("behaviors")
  
post_hoc_somatic <- select(post_hoc_somatic,-behavior)

somatic_wd2_summary_long <- somatic_wd2 %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time)) %>%
  mutate(stage="wd2")%>%
  ungroup()

somatic_wd1_summary_long <- somatic_wd1 %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time)) %>%
  mutate(stage="wd1")%>%
  ungroup()

somatic_bl_summary_long <- somatic_bl %>%
  group_by(ID,Behavior)%>%
  summarise(time=sum(Time)) %>%
  mutate(stage="bl")%>%
  ungroup()

somatic_long_summary <- bind_rows(list(somatic_bl_summary_long,
                                    somatic_wd1_summary_long,
                                    somatic_wd2_summary_long))%>%
  mutate(estrogen=
             case_when(
               ID=="EM02" ~ "E2",
               ID=="EM08" ~ "E2",
               ID=="EM11" ~ "E2",
               .default="no E2"))%>%
  mutate(time=as.numeric(time))

somatic_summary <- stage_effect %>%
  pivot_longer(
    cols=-c(ID,estrogen,stage),
    names_to = c("Behavior"),
    values_to = "time"
  )

ezANOVA(
  data = somatic_summary,
  dv = time,
  wid = ID,
  within = .(stage),
  between = .(estrogen),
  detailed = TRUE
)

df2 <- somatic_summary %>%
  filter(ID==c("EM02","EM08","EM11"))

somatic_long_summary %>%
  filter(Behavior!="reared sniff")%>%
  ggplot(mapping=aes(x=stage,y=time,color=Behavior,group=Behavior))+
  geom_line()+
  facet_wrap(vars(ID))

model <- lmer(
  time ~ stage + (stage | ID),
  data = df2
)
ezANOVA(
  data=df2,
  dv=time,
  wid=ID,
  within=.(stage),
  detailed=TRUE
)
anova(model)
eta <- eta_squared(model, partial = TRUE)
eta

emm <- emmeans(model, ~ stage | Behavior)
pairs(emm)

behavior_effects <- pairs(emm) %>%
  as.data.frame()
behavior_effects$p_adj <- p.adjust(behavior_effects$p.value, method = "holm")

plot(model, type = c("p", "smooth"))

#somatic plot of sig behaviors
behaviors_of_interest <- stage_effect %>%
  select(ID,jumping,digging,`reared sniff`,shaking,`agitation other`,stage,estrogen)%>%
  pivot_longer(
    cols=c(jumping,digging,`reared sniff`,shaking,`agitation other`),
    names_to="behavior",
    values_to="time"
  )%>%
  group_by(behavior,stage,estrogen) %>%
  summarize(time=median(time))%>%
  ungroup()
  
  
  
behaviors_of_interest %>%
  group_by(stage,behavior)%>%
  ggplot(mapping=aes(x=stage,y=time,color=behavior,group=behavior))+
  facet_wrap(vars(estrogen))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  labs(title="Selected Somatic Behaviors by Stage and Condition")+
  theme_apa()

#marbles 
marbles <- read_csv("cleaned data/Marble Burying - all.csv")

marbles <- marbles %>%
  rows_update(tibble(ID="EM01",`Not Buried`=4,`Half Buried`=5,`Fully Buried`=6,
                     `Total buried`=11,Stage="BL"),by=c("ID","Stage"))

friedman_marble_results <- data.frame(amount = character(),
                                       statistic = double(),
                                       p_value = double())
colsm <- c("Not Buried", "Half Buried", "Fully Buried",
          "Total buried")
i <- 1

for(col in colsm) {
  temp <- friedman.test(y=marbles[[col]],
                        groups=marbles$Stage,
                        blocks=marbles$ID)
  friedman_marble_results[i,"amount"] <- {{col}}
  friedman_marble_results[i ,"statistic"] <- temp$statistic
  friedman_marble_results[i, "p_value"] <- temp$p.value
  i <- i+1
}


#marbles post-hoc
marbles_wilcox_wd2 <- marbles %>%
  filter(Stage=="WD2")

marbles_wilcox_bl <- marbles %>%
  filter(Stage=="BL")

marbles_wilcox_wd1 <- marbles %>% 
  filter(Stage=="WD1")

wilcox<-wilcox.test(for_wilcox_bl$shaking,for_wilcox_wd$shaking,paired=TRUE)
post_hoc_marbles <- data.frame(matrix(nrow=5,ncol=4))
colnames(post_hoc_marbles) <- c("amount buried","bl_wd1","bl_wd2","wd1_wd2")
rowsm <- c("Not Buried", "Half Buried", "Fully Buried","Total buried")

i <- 1

for (i in rowsm) {
  tempmbl <- wilcox.test(marbles_wilcox_bl[[i]],
                           marbles_wilcox_wd1[[i]],paired=TRUE)
  post_hoc_marbles[i, "bl_wd1"] <- tempmbl$p.value
}

for (i in rowsm) {
  tempmblwd2 <- wilcox.test(marbles_wilcox_bl[[i]],
                           marbles_wilcox_wd2[[i]],paired=TRUE)
  post_hoc_marbles[i, "bl_wd2"] <- tempmblwd2$p.value
}

for (i in rowsm) {
  tempmwdwd <- wilcox.test(marbles_wilcox_wd1[[i]],
                            marbles_wilcox_wd2[[i]],paired=TRUE)
  post_hoc_marbles[i, "wd1_wd2"] <- tempmwdwd$p.value
}

post_hoc_somatic <- post_hoc_somatic %>%
  filter(!is.na(bl_wd1))%>%
  rownames_to_column("behaviors")

#estrogen marbles post-hoc
e2_marbles_wilcox_wd2 <- marbles_wilcox_wd2 %>%
  mutate(estrogen=
           case_when(
             ID=="EM02" ~ "E2",
             ID=="EM08" ~ "E2",
             ID=="EM11" ~ "E2",
             .default="no E2")) %>%
  filter(estrogen=="E2")

e2_marbles_wilcox_bl <- marbles_wilcox_bl %>%
  mutate(estrogen=
           case_when(
             ID=="EM02" ~ "E2",
             ID=="EM08" ~ "E2",
             ID=="EM11" ~ "E2",
             .default="no E2")) %>%
  filter(estrogen=="E2")

e2_marbles_wilcox_wd1 <- marbles_wilcox_wd1 %>% 
  mutate(estrogen=
           case_when(
             ID=="EM02" ~ "E2",
             ID=="EM08" ~ "E2",
             ID=="EM11" ~ "E2",
             .default="no E2")) %>%
  filter(estrogen=="E2")


e2_post_hoc_marbles <- data.frame(matrix(nrow=5,ncol=4))
colnames(e2_post_hoc_marbles) <- c("amount buried","bl_wd1","bl_wd2","wd1_wd2")
rowsm <- c("Not Buried", "Half Buried", "Fully Buried","Total buried")

i <- 1

for (i in rowsm) {
  tempmbl <- wilcox.test(e2_marbles_wilcox_bl[[i]],
                         e2_marbles_wilcox_wd1[[i]],paired=TRUE)
  e2_post_hoc_marbles[i, "bl_wd1"] <- tempmbl$p.value
}

for (i in rowsm) {
  tempmblwd2 <- wilcox.test(e2_marbles_wilcox_bl[[i]],
                            e2_marbles_wilcox_wd2[[i]],paired=TRUE)
  e2_post_hoc_marbles[i, "bl_wd2"] <- tempmblwd2$p.value
}

for (i in rowsm) {
  tempmwdwd <- wilcox.test(e2_marbles_wilcox_wd1[[i]],
                           e2_marbles_wilcox_wd2[[i]],paired=TRUE)
  e2_post_hoc_marbles[i, "wd1_wd2"] <- tempmwdwd$p.value
}


marble_plot <- marbles %>%
  mutate(estrogen=
           case_when(
             ID=="EM02" ~ "E2",
             ID=="EM08" ~ "E2",
             ID=="EM11" ~ "E2",
             .default="no E2")) %>%
  select(ID,`Not Buried`,`Half Buried`,`Fully Buried`,Stage,estrogen)%>%
  pivot_longer(
    cols=c(`Not Buried`,`Half Buried`,`Fully Buried`),
    names_to="amount buried",
    values_to="count"
  )%>%
  group_by(`amount buried`,Stage,estrogen) %>%
  summarize(count=median(count))%>%
  ungroup()



marble_plot %>%
  group_by(Stage,`amount buried`)%>%
  ggplot(mapping=aes(x=Stage,y=count,color=`amount buried`,group=`amount buried`))+
  facet_wrap(vars(estrogen))+
  geom_point()+
  geom_line()+
  labs(title="Marble Burying by Stage and Condition")+
  theme_apa()


#sample/example plots
example <- stage_effect %>%
  filter(ID=="EM08")%>%
  select(ID,grooming,chewing,`reared sniff`,shaking,digging,stage)%>%
  pivot_longer(
    cols=c(grooming,chewing,`reared sniff`,shaking,digging),
    names_to="behavior",
    values_to="time"
  )

example %>%
  group_by(stage,behavior)%>%
  ggplot(mapping=aes(x=stage,y=time,color=behavior,group=behavior))+
    geom_point()+
  geom_line()+
  scale_y_log10()+
  labs(title="EM08")

epmexample <- bind_rows(epm_behavior_bl,epm_behavior_wd)

epmexample <- epmexample %>%
  filter(subject=="EM11")

epmexample %>%
  group_by(stage,behavior)%>%
  ggplot(mapping=aes(x=stage,y=mean_time,color=behavior,group=behavior))+
  geom_point()+
  geom_line()+
  labs(title="EM11 EPM Behavior")

marble_example <- read_csv("cleaned data/Marble Burying - all.csv")

marble_example <- marble_example %>% 
  pivot_longer(
    cols=c(`Not Buried`,`Half Buried`,`Fully Buried`,`Total buried`),
    names_to="buried",
    values_to="count")
marble_example$buried <- factor(marble_example$buried,levels=c("Not Buried","Half Buried",
                                                          "Fully Buried", "Total buried"))

marble_example %>%
  filter(ID=="EM08")%>%
  group_by(Stage)%>%
  ggplot(mapping=aes(x=Stage,y=count,fill=buried))+
  geom_col(position="dodge")+
  scale_fill_viridis_d(option="I")+
  labs(title="EM08 Marble Burying")
