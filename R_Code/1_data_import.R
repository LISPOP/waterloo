#load libraries
library(tidyverse)
library(labelled)
library(haven)
library(here)

#Load data with haven
waterloo<-read_sav(here("data/WRMS2021_autotest.sav"))
#Get variable labels
lookfor(waterloo) %>% 
