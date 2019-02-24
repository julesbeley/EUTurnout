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
turnout

rbind(
     matrix(ncol = 1, nrow = 80 - 78, 9),
     matrix(ncol = 1, nrow = 85 - 80, 10),
     matrix(ncol = 1, nrow = 94 - 85, 12),
     matrix(ncol = 1, nrow = 103 - 94, 15),
     matrix(ncol = 1, nrow = 106 - 103, 25),
     matrix(ncol = 1, nrow = 112 - 106, 27),
     matrix(ncol = 1, nrow = 114 - 112, 28)
) -> b

b <- data.frame(year = c(1979:2014),
                nms = as.vector(b),
                stringsAsFactors = FALSE)

left_join(turnout, b, by = "year") -> m
m
plot <- ggplot(m, aes(x = nms, y = turnout)) +
     geom_point(size = 6) +
     geom_smooth(method = "lm", 
                 fill = "dodgerblue4", 
                 colour = "brown2",
                 alpha = 0.3) + 
     geom_text(data = m %>% slice(seq(1, 8, 2)),
               size = 13, 
               aes(label = year, x = nms, y = turnout + 1)) +
     geom_text(data = m %>% slice(seq(2, 8, 2)),
               size = 13, 
               aes(label = year, x = nms, y = turnout - 1)) +
     theme(
          plot.title = element_text(size = 50, hjust = 0.5, vjust = 30),
          plot.caption = element_text(size = 30, face = 3, vjust = -40),
          axis.text.x = element_text(size = 30, colour = "black"),
          axis.text.y = element_text(size = 30, colour = "black"),
          axis.title.x = element_text(size = 38, colour = "black", vjust = - 25),
          axis.title.y = element_text(size = 38, colour = "black", vjust = 25),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey", size = 1.3),
          panel.grid.major.y = element_line(color = "grey", size = 1.3),
          plot.margin = unit(c(4.5, 7.5, 4.5, 7.5), "cm")) +
     labs(title = "Average weighted turnout as a function of 
the number of Member states, 1979-2014",
          caption = "Source: International IDEA. Computed by J. Beley (2019)") +
     ylab("Turnout") +
     xlab("Number of Member states") + 
     scale_y_continuous(breaks = c(40, 45, 50, 55, 60), 
                        labels = c("40%", "45%", "50%", "55%", "60%"))

lm(data = m, turnout ~ nms) -> model
summary(model)

png("./reg.png", width = 2000, height = 1200)
plot
dev.off()

