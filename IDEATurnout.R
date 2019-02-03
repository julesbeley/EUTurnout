rm(list = ls())
turnout <- read.csv("./Turnout.csv",
                    na.strings = "",
                    stringsAsFactors = FALSE)
turnout %>%
  mutate(`Year` = as.numeric(`Year`)) %>%
  mutate(`Voter.Turnout` = parse_number(`Voter.Turnout`)) %>%
  mutate(`Registration` = as.numeric(gsub(",", "", `Registration`))) %>%
  mutate(`Voting.age.population` = as.numeric(gsub(",", "", `Voting.age.population`))) %>%
  mutate(`Population` = as.numeric(gsub(",", "", `Population`))) %>%
  rename(country =    ï..Country) %>%
  rename(type = Election.type) %>%
  rename(year = Year) %>%
  rename(turnout = Voter.Turnout) %>%
  rename(registered = Registration) %>%
  rename(votingagepop = Voting.age.population) %>%
  rename(population = Population) %>%
  arrange(`year`) -> turnout

eumap = function(date) {
  par(mfrow = c(3,3))
  library(tidyverse)
  library(maps)
  library(ggplot2)
  library(viridis)
  turnout %>%
    filter(`year` %in% date) %>%
    filter(type %in% "EU Parliament") %>%
    select(`country`, `turnout`) %>%
    rename(region = country) -> turnout
  world <- map_data("world")
  world %>%
    mutate(`region` = recode(`region`, "UK" = "United Kingdom")) -> world
  tojoin <- data.frame(region = names(table(world$region)))
  all <- full_join(turnout, tojoin, by = "region")
  all %>%
    arrange(`region`) -> all
  mapbig <- inner_join(world, all, by = "region")
  worldmap <- ggplot() + theme(
    panel.background = element_rect(fill = "lightcyan1",
                                    color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  europe <- worldmap + coord_fixed(xlim = c(-9, 35),
                                   ylim = c(36, 70.1),
                                   ratio = 1.5)
  europe2 <- europe + geom_polygon(data = mapbig,
                                   aes(fill = turnout,
                                       x = long,
                                       y = lat,
                                       group = group),
                                   color = "grey50") +
    scale_fill_viridis_c(limits = c(13,93),
                         option = "inferno",
                         direction = -1,
                        guide = "none",
                        na.value = "grey70") +
    annotate("text", -3, 68, label = as.character(date), size = 30)
  europe2
}
"one" <- eumap(1979)
"two" <- eumap(1984)
"three" <- eumap(1989)
"four" <- eumap(1994)
"five" <- eumap(1999)
"six" <- eumap(2004)
"seven" <- eumap(2009)
"eight" <- eumap(2014)
library(grid)
library(gridExtra)
png("./participation.png", height = 4000, width = 6500)
grid.arrange(one, two, three, four, five, six, seven, eight, nrow = 2, ncol = 4,
             top = textGrob("Voter turnout in EU elections",
                            gp = gpar(fontsize = 150)))
dev.off()
# https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2