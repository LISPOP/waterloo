#load libraries
library(tidyverse)
library(labelled)
library(haven)
library(here)

#Load data with haven
waterloo<-read_sav(here("data/WRMS2021_autotest.sav"))
waterloo<-as_factor(waterloo)
str(waterloo)


#Genreate fake weight data
#There will be  a weight variable like this 
waterloo$weight<-rnorm(1.5, sd=0.25, n=nrow(waterloo))
summary(waterloo$weight)
#install.packages(c('survey', 'srvyr'))
library(srvyr)
library(survey)

#### Read In Election Data File ####
library(readr)
library(readxl)
history<-read_excel(path="data/waterloo_region_voting_history.xlsx")
history %>% 
  mutate(Party=recode(Party, "Green Party"="Green",
                      "Progressive Conservative"="PC"))->history

#this takes Scott's history of results file
history %>% 
  #Groups by year and level (e.g. provincial federal)
  group_by(Year, Level, `District Name`) %>% 
  #then creates a percent variable that is each party's vote total divided by the sum of the vote total
  #Note that I believe that this is working on the results at the level of the RoW
  #That is to say it is calculating the Percent of Votes each candidate got in each election
  # of the total votes cast in *all* of RoW ridings
  mutate(party_percent=(`Vote total`/sum(`Vote total`))*100) %>% 
  #Then we regroup by Party and level
  group_by(Party, Level) %>% 
  #and calculate those averages 
  summarize(historic_average=mean(party_percent))->historic_averages
historic_averages
#Start with scott's history file
history %>% 
  #Filter out elections after 2017 (e.g. we want the most recent provincial and federal)
  filter(Year>2017) %>%
  #group by level (federal / provincial) and District
  group_by(Level, `District Name`) %>% 
  #And calculate the percent
  mutate(party_percent=`Vote total`/sum(`Vote total`)*100) %>% 
  group_by(Level, Party) %>% 
summarize(recent_average=mean(party_percent))->recent_results

#### Get the Wikipedia data on polls ####
#Uncomment if you need to install rvest
#install.packages('rvest')
#load the library
library(rvest)