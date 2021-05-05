source("R_Code/3_set_theme.R")
#### Load wlucolors package #### 
#remotes::install_github('sjkiss/wlucolors', force=T)
library(wlucolors)

#### 2021 Federal Support By Federal History #### 
#This starts with the survey design object
waterloo2 %>% 
  #form groups of the 2019 federal vote
  group_by(vote_federal) %>% 
  #summarize the count of votes in each group with survey_total()
  summarize(survey_total()) %>% 
  #Now join those to the historic averages  by the variables vote_federal in the first and Party in the second
  left_join(., historic_averages, by=c('vote_federal'='Party')) %>% 
  #Filter only federal elections
  filter(Level=="Federal") %>% 
  #Rename some variables to be more user friendly
  rename(`Vote Intention`=coef, `Historic Average`=historic_average, Party=vote_federal) %>% 
  #Now pivot the data frame longer so that vote intention and historic average
  # Are put into one variable called Variable; their values stored in a column called percent
  pivot_longer(., cols=c(2,5), values_to = c('Percent'), names_to=c("Variable")) %>% 
  #Now just relevel the variable Party to put the parties in order of the eleciton results
  # Liberals won, then the Conservatives, then the NDP, then the Greens
  mutate(Party=fct_relevel(Party, "Liberal", "Conservative", "NDP", "Green")) %>% 
  #Now draw the plot.
  #Put Party on the y (so we're making it a sideways barplot) x equals the percent
  # fill the bars with the values stored in Variable 9e.g. which party
  #Set the alpha (transparency to correspond to Variable) so that the historic averages are 
  ggplot(., aes(y=Party, x=Percent,group=Variable, fill=Party))+
  #Dodge the barplot
  geom_col(position="dodge", aes(alpha=Variable))+
  #this command adds the percentage values
geom_text(aes(y=Party, x=Percent,label=round(Percent)),alpha=1,position=position_dodge(width=0.9), hjust=-0.1)+
  #set theme to be minimal
  theme_minimal()+
xlim(c(0,50))+
# theme_1+
  #This uses my custom fill palette in order to set the colors to correspond to the parties
  scale_fill_mine()+
  #This reverses the order of the parties so that they run from Liberal, Conservative NDP to Green
  scale_y_discrete(limits=rev)+
  #This command turns off the legend that is printed 
  guides(text='none', fill='none', alpha='none') 
    #labs(title="Federal Vote Intention\nSpring 2021 Compared to Historic Levels of Support ")
#This saves the plot out with a meaningful file name 
ggsave(here("Plots", "vote_intention_historic_average.png"), width=6,height=3)
recent_results

#### 2021 Federal Support By Previous Election#### 
waterloo2 %>% 
  #form groups of the 2019 federal vote
  group_by(vote_federal) %>% 
  #summarize the count of votes in each group with survey_total()
  summarize(survey_total()) %>% 
  #Now join those to the historic averages  by the variables vote_federal in the first and Party in the second
  left_join(., recent_results, by=c('vote_federal'='Party')) %>% 
  #Filter only federal elections
  filter(Level=="Federal") %>% 
  #Rename some variables to be more user friendly
  rename(`Vote Intention`=coef, `2019 Result`=recent_average, Party=vote_federal) %>% 
  #Now pivot the data frame longer so that vote intention and historic average
  # Are put into one variable called Variable; their values stored in a column called percent
  pivot_longer(., cols=c(2,5), values_to = c('Percent'), names_to=c("Variable")) %>% 
  #Now just relevel the variable Party to put the parties in order of the eleciton results
  # Liberals won, then the Conservatives, then the NDP, then the Greens
  mutate(Party=fct_relevel(Party, "Liberal", "Conservative", "NDP", "Green")) %>% 
  #Now draw the plot.
  #Put Party on the y (so we're making it a sideways barplot) x equals the percent
  # fill the bars with the values stored in Variable 9e.g. which party
  #Set the alpha (transparency to correspond to Variable) so that the historic averages are 
  ggplot(., aes(y=Party, x=Percent, fill=Party, alpha=Variable))+
  #Dodge the barplot
  geom_col(position="dodge")+
  #set theme to be minimal
  #This uses my custom fill palette in order to set the colors to correspond to the parties
  scale_fill_mine(palette = 'federal') +
#This reverses the order of the parties so that they run from Liberal, Conservative NDP to Green
  scale_y_discrete(limits=rev)+
  #This command turns off the legend that is printed 
  labs(title="Federal Vote Intention by Past Election")+
  #This command turns off the legend that is printed 
  guides(text='none', fill='none') 
ggsave(filename=here("Plots", "federal_vote_intention_2019_result.png"), width=6, height=3)
#### 2021 provincial support by Provincial History#### 
waterloo2 %>% 
  group_by(vote_provincial) %>% 
  summarize(survey_total()) %>% 
  left_join(., historic_averages, by=c('vote_provincial'='Party')) %>% 
  filter(Level=="Provincial") %>% 
  rename(`Vote Intention`=coef, `2018 Result`=historic_average, Party=vote_provincial) %>% 
  pivot_longer(., cols=c(2,5), values_to = c('Percent'), names_to=c("Variable")) %>% 
  mutate(Party=fct_relevel(Party, "PC","Liberal", "NDP", "Green")) %>% 
  ggplot(., aes(y=Party, x=Percent, fill=Party, alpha=Variable))+geom_col(position="dodge")+theme_bw()+
  scale_fill_mine()+scale_y_discrete(limits=rev)
ggsave(here("Plots", "vote_intention_provincial_historic_average.png"), width=6,height=3)

#### 2021 Provincial Support by 2018 rEsult #### 
waterloo2 %>% 
  #form groups of the 2019 federal vote
  group_by(vote_provincial) %>% 
  #summarize the count of votes in each group with survey_total()
  summarize(survey_total()) %>% 
  #Now join those to the recent average of Waterloo Region Ridings  
  #by the variables vote_provincial in the first and Party in the second
  left_join(., recent_results, by=c('vote_provincial'='Party')) %>% 
  #Filter only federal elections
  filter(Level=="Provincial") %>% 
  #Rename some variables to be more user friendly
  rename(`Vote Intention`=coef, `2018 Result`=recent_average, Party=vote_provincial) %>% 
  #Now pivot the data frame longer so that vote intention and historic average
  # Are put into one variable called Variable; their values stored in a column called percent
  pivot_longer(., cols=c(2,5), values_to = c('Percent'), names_to=c("Variable")) %>% 
  #Now just relevel the variable Party to put the parties in order of the eleciton results
  # Liberals won, then the Conservatives, then the NDP, then the Greens
  mutate(Party=fct_relevel(Party,  "PC","Liberal", "NDP", "Green")) %>% 
  #Now draw the plot.
  #Put Party on the y (so we're making it a sideways barplot) x equals the percent
  # fill the bars with the values stored in Variable 9e.g. which party
  #Set the alpha (transparency to correspond to Variable) so that the historic averages are 
  ggplot(., aes(y=Party, x=Percent, fill=Party, alpha=Variable))+
  #Dodge the barplot
  geom_col(position="dodge")+
  #set theme to be minimal
  theme_minimal()+
  #This uses my custom fill palette in order to set the colors to correspond to the parties
  scale_fill_mine()+
  #This reverses the order of the parties so that they run from Liberal, Conservative NDP to Green
  scale_y_discrete(limits=rev)+
  #This command turns off the legend that is printed 
  guides(alpha='none')+labs(title="Vote Intention ")
#### Assessment by Vote####
waterloo2 %>% 
  #Select federal vote and assessment of Trudeau in K3
  select(vote_federal, K3) %>% 
  #Form groups of both
  group_by(vote_federal, K3) %>% 
  #count
  summarize(freq=n()) %>% 
  #calculate percentage
  mutate(Percent=(freq/sum(freq))*100) %>% 
  #rename vote_federal into something more aesthetic
  rename(., `2019 Vote`= vote_federal) %>% 
  #Relevel 2019 vote to be more informative
  mutate(`2019 Vote`=fct_relevel(`2019 Vote`, "Liberal", "Conservative", "NDP", "Green")) %>% 
  filter(!is.na(`2019 Vote`)) %>% 
  #graph
  #Set 2019 vote on y-axis, fill by the categories of the response variable
  #set the x to be the percent
ggplot(., aes(y=`2019 Vote`, fill=K3, x=Percent))+
  #Because we have a y-value, we can use geom_col, not geom_bar()
  geom_col()+
  scale_y_discrete(limits=rev)+
  scale_fill_mine(palette='wlu', name="Approval", rev=T)+
  theme_minimal()

ggsave(filename=here("Plots", "covid_assessment_by_2019_vote_federal.png"))

#### Assessment by Provincial Vote####
waterloo$K6
waterloo2 %>% 
  select(vote_provincial, K6) %>% 
  group_by(vote_provincial, K6) %>% 
  summarize(freq=n()) %>% 
  mutate(Percent=(freq/sum(freq))*100) %>% 
  rename(., `2018 Vote`= vote_provincial) %>% 
  mutate(`2018 Vote`=fct_relevel(`2018 Vote`,  "PC", "Liberal","NDP", "Green")) %>% 
  filter(!is.na(`2018 Vote`)) %>% 
  ggplot(., aes(y=`2018 Vote`, fill=K6, x=Percent))+geom_col()+scale_y_discrete(limits=rev)+scale_fill_mine(palette='wlu', name="Approval", rev=T)+theme_bw()

ggsave(filename=here("Plots", "covid_assessment_by_2018_vote_provincial.png"))

#### Demographics#### 
var_label(waterloo)

