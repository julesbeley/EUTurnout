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
     rename(country =       ï..Country) %>%
     rename(type = Election.type) %>%
     rename(year = Year) %>%
     rename(turnout = Voter.Turnout) %>%
     rename(registered = Registration) %>%
     rename(votingagepop = Voting.age.population) %>%
     rename(population = Population) %>%
     filter(`type` %in% "EU Parliament") %>%
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
     )) %>% 
     arrange(desc(`turnout`)) -> turnout

turnout %>% 
     select(`country`, `year`, `turnout`, `registered`) %>% 
     arrange(desc(`year`)) %>% 
     mutate(`voters` = round(`turnout` * `registered`)) -> turnout

for (i in (seq(1979, 2014, 5))) {
        turnout <- rbind(turnout, 
              data.frame(country = "EU9",
                         year = i,
                         turnout = round(sum(turnout2$voters[turnout2$year == i]) / sum(turnout2$registered[turnout2$year == i]), 2),
                         registered = sum(turnout2$registered[turnout2$year == i]),
                         voters = sum(turnout2$voters[turnout2$year == i])))
}

turnout %>% 
     arrange(`year`) -> turnout

subset(turnout, `year` != 1979)

graph <- ggplot(turnout) + 
     geom_line(data = subset(turnout, `country` != "EU9"), 
               aes(x = `year`, y = `turnout`, color = `country`), size = 1.6) +
     geom_point(data = subset(turnout, `country` != "EU9"),
                aes(x = `year`, y = `turnout`, color = `country`),
                shape = 19,
                size = 6) +
     geom_line(data = subset(turnout, `country` == "EU9"), 
               aes(x = `year`, y = `turnout`, color = "black"), size = 1.6) +
     geom_point(data = subset(turnout, `country` == "EU9"),
                aes(x = `year`, y = `turnout`, color = "black"),
                shape = 19,
                size = 6) +
     geom_text(
          size = 8,
          data = subset(turnout, `year` == "1979", `country` != "EU9") %>% slice(c(1,3,5,8,9)),
          aes(
               label = `country`,
               colour = `country`,
               x = `year` - 5,
               y = `turnout`
          )) +
     geom_text(
          size = 8,
          data = subset(turnout, `year` == "2014", `country` != "EU9") %>% slice(c(2,6,7,8)),
          aes(
               label = `country`, 
               colour = `country`, 
               x = `year` + 5,
               y = `turnout`
          )) +
     theme(
          plot.title = element_text(size = 30),
          plot.caption = element_text(size = 15, face = 3),
          axis.text.x = element_text(size = 19, colour = "black"),
          axis.text.y = element_text(size = 19, colour = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey"),
          panel.grid.major.y = element_line(color = "brown2", linetype = 2)) +
     scale_color_brewer(guide = "none", palette = "Paired") +
     scale_x_continuous(limits = c(1970, 2025), breaks = seq(1979, 2014, 5)) +
     scale_y_continuous(limits = c(0,100), labels = c("0%", "25%", "50%", "75%", "100%")) +
     labs(title = "Participation in EU elections since 1979 among 'the Nine'",
          caption = "Source: International IDEA. Computed by J. Beley (2018)")
graph

png("./graph.png", width = 1200, height = 700)
graph
dev.off()
