rm(list = ls())
library(tidyverse)
library(RColorBrewer)

turnout <- read.csv("./Turnout.csv",
                    na.strings = "",
                    stringsAsFactors = FALSE)

turnout %>%
     mutate(`Year` = as.numeric(`Year`)) %>%
     mutate(`Voter.Turnout` = parse_number(`Voter.Turnout`)) %>%
     mutate(`Registration` = as.numeric(gsub(",", "", `Registration`))) %>%
     mutate(`Voting.age.population` = as.numeric(gsub(",", "", `Voting.age.population`))) %>%
     mutate(`Population` = as.numeric(gsub(",", "", `Population`))) %>%
     rename(country =        ï..Country) %>%
     rename(type = Election.type) %>%
     rename(year = Year) %>%
     rename(turnout = Voter.Turnout) %>%
     rename(registered = Registration) %>%
     rename(votingagepop = Voting.age.population) %>%
     rename(population = Population) %>%
     select(`type`, `country`, `year`, `turnout`, `registered`) %>%
     mutate(`voters` = round(`turnout` * `registered`)) -> turnout

turnout

n <- ggplot(turnout) +
     geom_point(data = turnout %>% filter(type == "Parliamentary"), 
                aes(x = year, y = turnout),
                size = 4,
                alpha = 0.5) +
     geom_point(data = turnout %>% filter(type == "EU Parliament"), 
                aes(x = year, y = turnout),
                color = "dodgerblue4",
                size = 4,
                alpha = 0.5) +
     geom_smooth(data = turnout %>% filter(type == "Parliamentary"),
                 aes(x = year, y = turnout),
                 method = "lm", 
                 colour = "black",
                 alpha = 0.3) +
     geom_smooth(data = turnout %>% filter(type == "EU Parliament"),
                 aes(x = year, y = turnout),
                 method = "lm", 
                 colour = "dodgerblue4",
                 alpha = 0.3) +
     theme(
          plot.title = element_text(size = 50, hjust = 0.5, vjust = 30),
          plot.caption = element_text(size = 30, face = 3, vjust = -40),
          axis.text.x = element_text(size = 30, colour = "black"),
          axis.text.y = element_text(size = 30, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 38, colour = "black", vjust = 25),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey", size = 1.3),
          panel.grid.major.y = element_line(color = "grey", size = 1.3),
          plot.margin = unit(c(4.5, 7.5, 4.5, 7.5), "cm")) +
     labs(title = "Turnout in EU and national parliamentary 
elections with trend lines, 1979-2014",
          caption = "Source: International IDEA. Computed by J. Beley (2019)") +
     ylab("Turnout") +
     scale_y_continuous(breaks = c(25, 50, 75, 100), 
                        labels = c("25%", "50%", "75%", "100%"))
     
png("./compare.png", width = 1600, height = 1000)
n
dev.off()

lm(data = turnout %>% filter(type == "EU Parliament"), turnout ~ year) -> modelEU
lm(data = turnout %>% filter(type == "Parliamentary"), turnout ~ year) -> modelNat
summary(modelEU)
summary(modelNat)
