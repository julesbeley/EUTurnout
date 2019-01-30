rm(list = ls())
library(tidyverse)
turnout <- read.csv("./Turnout.csv", na.strings = "", stringsAsFactors = FALSE)
turnout %>% 
  mutate(`Year` = as.numeric(`Year`)) %>% 
  mutate(`Voter.Turnout` = parse_number(`Voter.Turnout`)) %>% 
  mutate(`Registration` = as.numeric(gsub(",", "", `Registration`))) %>% 
  mutate(`Voting.age.population` = as.numeric(gsub(",", "", `Voting.age.population`))) %>% 
  mutate(`Population` = as.numeric(gsub(",", "", `Population`))) %>% 
  rename(country = ï..Country) %>% 
  rename(type = Election.type) %>% 
  rename(year = Year) %>% 
  rename(turnout = Voter.Turnout) %>% 
  rename(registered = Registration) %>% 
  rename(votingagepop = Voting.age.population) %>% 
  rename(population = Population) %>% 
  arrange(`year`) -> turnout

