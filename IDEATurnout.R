rm(list = ls())
library(tidyverse)
turnout <- read.csv("./Turnout.csv", na.strings = "")
turnout %>% 
  filter(`Election.type` %in% "EU Parliament") %>% 
  rename(country = ï..Country) %>% 
  rename(type = Election.type) %>% 
  rename(year = Year) %>% 
  rename(turnout = Voter.Turnout) %>% 
  rename(registered = Registration) %>% 
  rename(votingagepop = Voting.age.population) %>% 
  rename(population = Population) -> eu
turnout %>% 
  filter(`Election.type` %in% "Parliamentary") %>% 
  rename(country = ï..Country) %>% 
  rename(type = Election.type) %>% 
  rename(year = Year) %>% 
  rename(turnout = Voter.Turnout) %>% 
  rename(registered = Registration) %>% 
  rename(votingagepop = Voting.age.population) %>% 
  rename(population = Population)-> national
