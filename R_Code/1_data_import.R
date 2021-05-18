#install necessary packages
#these are the packages necessary to run the code

to.install<-c('tidyverse', 'labelled', 'haven', 'here', 'srvyr', 'survey', 'lubridate', 'readr', 'readxl', 'rvest', 'janitor', 'remotes')
#This checks to see if these packages are installed.
new.packages <- to.install[!(to.install %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#These packages *may* cause difficulty for people running R 4.0 on Windows. PLease contact Simon Kiss if that's the case

remotes::install_github('sjkiss/wlucolors')
remotes::install_github('sjkiss/cesdata')
#load libraries
library(tidyverse)
library(labelled)
library(haven)
library(here)

#Load data with haven
waterloo<-read_sav(here("data/WRMS2021_autotest.sav"))
waterloo<-as_factor(waterloo)
str(waterloo)
?read_excel

#Generate fake weight data
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

##Read the HTML from Wiki Article
Ontario_Polling <-read_html("https://en.wikipedia.org/wiki/43rd_Ontario_general_election")

tables <- Ontario_Polling %>% html_table(fill = TRUE)

##Input the seventh table, which is the one including the polling data
polls <-tables [[7]]

##install janitor to clean up heading names
#install.packages("janitor")
library(janitor)

##Clean up heading names
polls <-polls %>% clean_names()
names(polls)

##create a new table that removes misc rows (Leadership change, etc.)
polls <- polls[-c(34,59, 61, 62, 63), ]
#### Simon speaking
#### If we do the formatting on the original poll table, then we
#### don't have to do that anymore on the gathered table. 
#### We may use this poll table for something else, e.g. merge
#### With our poll data to test how the Waterloo Region poll compares
#### It would be nice to keep the wide table in a wide format. 
#### Then format
##convert date to yyyy-mm-dd format
library(lubridate)
polls %>%
  mutate(last_date_of_polling= mdy(last_date_of_polling
  ))->polls

#Convert all vote shares to numbers
#Start with dataframe
polls %>% 
  #mutate(across(), function) applies a function to each variable
  #listed in across
  #?across
  mutate(across(c(pc:other, sample_size, lead), as.numeric))->polls
str(polls)
#Now work on the margin_of_error 
polls$margin_of_error
#Yuck
str_remove_all(polls$margin_of_error, "±|%|N/A")
#save 
polls$margin_of_error<-str_remove_all(polls$margin_of_error, "±|%|N/A")
#Turn to a number
polls$margin_of_error<-as.numeric(polls$margin_of_error)
#Now we will be able to use that perhaps to show a band of the confidence intervals. Who knows. 

##create long table by party and vote share
polls_long <- gather (polls, party, vote_share, pc:other, factor_key = TRUE)
polls_long


#### Checking poll data #### 
library(ggplot2)
library(dplyr)
polls_long
ggplot(polls_long, aes(x=last_date_of_polling, y=vote_share, group=party, color=party)) +
  geom_line() + scale_x_date(date_breaks="4 month", date_labels = "%B")


