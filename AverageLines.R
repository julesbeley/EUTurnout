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
     filter(`type` %in% "EU Parliament") %>%
     select(`country`, `year`, `turnout`, `registered`) %>%
     mutate(`voters` = round(`turnout` * `registered`)) -> turnout

for (i in (seq(1979, 2014, 5))) {
     turnout <- rbind(turnout, 
                      data.frame(country = "Average",
                                 year = i,
                                 turnout = round(sum(turnout$voters[turnout$year == i]) / sum(turnout$registered[turnout$year == i]), 2),
                                 registered = sum(turnout$registered[turnout$year == i]),
                                 voters = sum(turnout$voters[turnout$year == i])))
}

turnout %>% arrange(`year`) %>% 
     select(`country`, `year`, `turnout`) -> turnout
subset(turnout, `country` == "Average") -> turnout

graph <- ggplot(turnout) + 
     geom_line(aes(x = `year`, y = `turnout`, color = `country`), 
               size = 1.6) +
     geom_point(aes(x = `year`, y = `turnout`, color = `country`),
                shape = 19,
                size = 5) +
     geom_text(size = 8,
               aes(
                    label = `turnout`, 
                    colour = `country`, 
                    x = `year`,
                    y = `turnout` - 7
               )) +
     theme(
          plot.title = element_text(size = 30, hjust = 0.5, vjust = 5),
          plot.caption = element_text(size = 15, face = 3, vjust = 0),
          axis.text.x = element_text(size = 19, colour = "black"),
          axis.text.y = element_text(size = 19, colour = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey"),
          panel.grid.major.y = element_line(color = "brown2", linetype = 2),
          plot.margin = unit(c(0.6, 1, 0.6, 1), "cm")) +
     scale_color_manual(guide = "none", values = "dodgerblue4") +
     scale_x_continuous(limits = c(1970, 2025), breaks = seq(1979, 2014, 5)) +
     scale_y_continuous(limits = c(0,100), labels = c("0%", "25%", "50%", "75%", "100%")) +
     labs(title = "Average participation in EU elections since 1979",
          caption = "Source: International IDEA. Computed by J. Beley (2018)")
graph

png("./Average.png", width = 1200, height = 700)
graph
dev.off()