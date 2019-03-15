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
     mutate(`voters` = round(0.01 * `turnout` * `registered`)) %>% 
     filter(`country` %in% c(
          "Belgium",
          "Luxembourg",
          "Italy",
          "Germany",
          "Ireland",
          "France",
          "Netherlands",
          "Denmark",
          "United Kingdom"
     )) -> turnout

n <- ggplot(turnout) +
     geom_point(aes(x = year, y = turnout, size = voters, color = type),
                alpha = 0.7) +
     scale_color_manual(values = c("Parliamentary" = "brown2",
                                   "EU Parliament" = "dodgerblue4"),
                        labels = c("National",
                                   "EU"),
                        breaks = c("Parliamentary", "EU Parliament")) +
     guides(size = FALSE,
            color = guide_legend(title = "Type", 
                                 override.aes = list(size = 7))) +
     geom_smooth(data = turnout %>% filter(type == "Parliamentary"),
                 aes(x = year, y = turnout, weight = voters),
                 method = "lm", 
                 colour = "brown2",
                 alpha = 0.3) +
     geom_smooth(data = turnout %>% filter(type == "EU Parliament"),
                 aes(x = year, y = turnout, weight = voters),
                 method = "lm", 
                 colour = "dodgerblue4",
                 alpha = 0.3) +
     theme(
          plot.title = element_text(size = 40, hjust = 0.5, vjust = 30),
          plot.caption = element_text(size = 20, face = 3, vjust = -20),
          axis.text.x = element_text(size = 23, colour = "black"),
          axis.text.y = element_text(size = 23, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 38, colour = "black", vjust = 25),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey", size = 1),
          panel.grid.major.y = element_line(color = "grey", size = 1),
          plot.margin = unit(c(4.5, 7.5, 4.5, 7.5), "cm"),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.key = element_rect(fill = "white")) +
     labs(title = "Turnout in EU and national parliamentary 
elections with trend lines, EU9, 1979-2014",
          caption = "Source: International IDEA. Computed by J. Beley (2019)") +
     ylab("Turnout") +
     scale_y_continuous(breaks = c(25, 50, 75, 100), 
                        labels = c("25%", "50%", "75%", "100%"))
     
png("./compare.png", width = 1600, height = 1000)
n
dev.off()

modelEU <- lm(
     data = turnout %>% filter(type == "EU Parliament"),
     formula = turnout ~ year,
     weights = voters
) 
modelNat <- lm(
     data = turnout %>% filter(type == "Parliamentary"),
     formula = turnout ~ year,
     weights = voters
)  
summary(modelEU)
summary(modelNat)