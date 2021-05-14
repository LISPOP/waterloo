
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
#### Make Ideology Variable #### 
## Running this code will install all the Canada Election Studies data from my personal package
## It's not a huge deal, it can be convenient, even, but be warned. 
##This line installs the packages
install.packages("devtools")
install.packages("tidyverse")
library("devtools") 
devtools::install_github("sjkiss/cesdata")
## This line loads the library
library(cesdata)
## This line loads the ces2019 web survey
data('ces19web')
## This line loads the labelled package in case it's not already loaded
library(labelled)
## Start with the data frame
ces19web %>%
# # Recode the 2019 PID variable to get rid of all the missing variables and useless stuff
mutate(pid=car::Recode(pes19_pidtrad, "6:9=NA")) %>%
# # Just select the pid variable and the ideology variable
 select(pid, cps19_lr_scale_bef_1) %>%
# # Form groups by Party ID
group_by(pid) %>%
# # Calculate the average ideology by Party ID
summarize(avg_pid=mean(cps19_lr_scale_bef_1, na.rm=T)) %>%
arrange(avg_pid)

## This code makes a boxplot of ideology by Party ID 
ces19web %>% 
mutate(pid=car::Recode(pes19_pidtrad, "6:9=NA"))%>% 
select(pid, cps19_lr_scale_bef_1) %>% 
## set the party id variables levels to be according to hypothesized left-right ideology
mutate(pid=fct_relevel(as_factor(pid),"NDP", "Green", "Bloc Québécois ", "Liberal", "Conservative")) %>% 
## Graph
  install.packages("ggplot2")
ggplot(., aes(x=pid, y=as.numeric(cps19_lr_scale_bef_1)))+geom_boxplot()
## Conclusion, the Greens could be put between the NDP and the LIberals

#### Convert Votes To Numbers To Form an Ideology Scale #### 
# Each respondent was asked the following questions:
# Who did you vote for federally in 2019
# Who did you vote for provinciallin in 2018
# Who are you going to vote for federally in 2021?
# Who are you going to vote for provincially in 2020?
# We should be able to sum these totals in a way to come up with one number that plausibly measures voter's ideology
# A person who: voted NDP federally in 2019, NDP provincially in 2018, intends to vote NDP provincially in 2022 and NDP
# federally in 2021 is kind of the most left-wing person possible (except for the Greens, which confuse things.)
# Meanwhile a person who responded Conservative straight through on all those would be the most right-wing person
#What we need to do is to assign a number from 1 to four (there are four possible parties) for each of the four vote questions.
# If a person responded NDP, they get a 1, Green, they get 1.5, Liberal, they get a 2 and they get a 3 if they said Conservative 
# Find the list of vote variables
lookfor(waterloo, 'vote')
#K1 is the federal vote 2019, it will be called ideology1
#K2 is the last federal vote intention, it will be called ideology2
#K4 is the provincial vote 2018, it will be called ideology 3
#K5 is the provincial vote intention, it will be called ideology 4

#Note the case_when code down below makes use of the factor levels that are in each variable.
# they must be *exactly* as coded. Nothing wrong with copying and pasting to get it right. 
levels(waterloo$K1)
levels(waterloo$K2)
levels(waterloo$K4)
levels(waterloo$K5)

 waterloo %>% 
mutate(ideology1=case_when(
   K1=="The New Democratic Party" ~ 1,
   K1=="The Green Party" ~ 1.5,
   K1=="The Liberal Party" ~ 2,
   K1=="The Conservative Party" ~ 3),
)
 
## Now do ideology 2
 waterloo %>% 
   mutate(ideology2=case_when(
     K2=="The New Democratic Party" ~ 1,
     K2=="The Green Party" ~ 1.5,
     K2=="The Liberal Party" ~ 2,
     K2=="The Conservative Party" ~ 3),
) 
##Now do ideology 3
 waterloo %>% 
   mutate(ideology3=case_when(
     K4=="The New Democratic Party" ~ 1,
     K4=="The Green Party" ~ 1.5,
     K4=="The Liberal Party" ~ 2,
     K4=="The Progressive Conservative Party" ~ 3),
   )

## Now do ideology 4
# ideology4=case_when(
# K4==" ~ ,
# )
#)
 waterloo %>% 
   mutate(ideology4=case_when(
     K5=="The New Democratic Party" ~ 1,
     K5=="The Green Party" ~ 1.5,
     K5=="The Liberal Party" ~ 2,
     K5=="The Progressive Conservative Party" ~ 3),
   )
####Apply Weights#### 
#The final data-set will have a variable like this.
#The file has to be weighted to demographic data 
waterloo %>% 
  as_survey_design(., weights=weight) ->waterloo2
