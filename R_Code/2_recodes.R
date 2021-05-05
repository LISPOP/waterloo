source("R_Code/1_data_import.R")
#### Recode Vote####
lookfor(waterloo, "vote")
waterloo$K1
waterloo$K4
levels(waterloo$K1)
#Change federal vote
waterloo %>% 
  mutate(vote_federal=case_when(
    str_detect(K1, "Liberal") ~"Liberal",
    str_detect(K1, "Conservative") ~ "Conservative",
    str_detect(K1, "New") ~ "NDP",
    str_detect(K1, "Green") ~ "Green",
    TRUE ~ NA_character_
  )) %>% 
  mutate(vote_provincial=case_when(
    str_detect(K1, "Liberal") ~"Liberal",
    str_detect(K1, "Conservative") ~ "PC",
    str_detect(K1, "New") ~ "NDP",
    str_detect(K1, "Green") ~ "Green",
    TRUE ~ NA_character_

  ))->waterloo
waterloo$vote_federal<-factor(waterloo$vote_federal, levels=c("Liberal", "Conservative", "NDP", "Green"))
waterloo$vote_provincial<-factor(waterloo$vote_provincial, levels=c("PC", "Liberal", "NDP", "Green"))
#Recode provincial vote


####Apply Weights#### 
#The final data-set will have a variable like this.
#The file has to be weighted to demographic data 
waterloo %>% 
  as_survey_design(., weights=weight) ->waterloo2
