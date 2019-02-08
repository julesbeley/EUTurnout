rm(list = ls())

library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)
library(mapproj)

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
     arrange(`year`) -> turnout

eumap = function(date) {
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
          panel.background = element_rect(fill = "white",
                                          color = NA),
          panel.grid = element_line(colour = "royalblue"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
     )
     europe <- worldmap + coord_map(
          "lambert",
          parameters = c(30, 43),
          xlim = c(-10, 35),
          ylim = c(35, 70.1)
     )
     europe <- europe + geom_polygon(
          data = mapbig,
          aes(
               fill = turnout,
               x = long,
               y = lat,
               group = group
          ),
          color = "grey30"
     )
     europe <- europe + scale_fill_viridis_c(
          limits = c(13, 93),
          option = "plasma",
          direction = -1,
          guide = "none",
          na.value = "grey70"
     )
     europe <-
          europe + annotate("text", -10, 68, label = as.character(date), size = 35)
}

"one" <- eumap(1979)
"two" <- eumap(1984)
"three" <- eumap(1989)
"four" <- eumap(1994)
"five" <- eumap(1999)
"six" <- eumap(2004)
"seven" <- eumap(2009)
"eight" <- eumap(2014)

turnout %>%
     filter(`year` %in% 2014) %>%
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
     axis.title.y = element_blank(),
     legend.text = element_text(size = 100),
     legend.spacing.x = unit(10, "mm"),
     legend.title = element_text(size = 155)
)

europe <- worldmap + coord_fixed(xlim = c(-9, 35),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)

europe <- europe + geom_polygon(data = mapbig,
                                aes(
                                     fill = turnout,
                                     x = long,
                                     y = lat,
                                     group = group
                                ),
                                color = "grey50")

europe <- europe + scale_fill_viridis_c(
     limits = c(13, 93),
     option = "plasma",
     direction = -1,
     na.value = "grey70",
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
     guide = guide_colorbar(
          barheight = unit(600, units = "mm"),
          barwidth = unit(45, units = "mm"),
          title = "%"
     )
)

legend <- get_legend(europe)
legend <- as_ggplot(legend)

lay <- rbind(c(NA, 1, 2, 3, 4, 9), c(NA, 5, 6, 7, 8, 9))

png("./participation.png", height = 4200, width = 10000)

grid.arrange(
     one,
     two,
     three,
     four,
     five,
     six,
     seven,
     eight,
     legend,
     layout_matrix = lay,
     top = textGrob(
          "Voter turnout in EU elections, 1979-2014",
          gp = gpar(fontsize = 200,
                    fontface = "bold"),
          y = 0.5
     ),
     bottom = textGrob(
          "Source: International IDEA. Computed by J. Beley (2018).",
          gp = gpar(fontsize = 100,
                    fontface = 3),
          hjust = 0,
          vjust = 0,
          x = 0.575
     )
)
dev.off()

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

subset(turnout, `year` == 1979) %>% slice(2)

col <- heat.colors(9)

ggplot(turnout) + 
     geom_line(aes(x = `year`, y = `turnout`, color = `country`), size = 1.6) +
     geom_point(aes(x = `year`, y = `turnout`, color = `country`),
                shape = 1,
                size = 3) +
     geom_text(
          data = subset(turnout, `year` == "1979") %>% slice(c(seq(2, 9, 2))),
          aes(
               label = `country`,
               colour = `country`,
               x = `year` - 3,
               y = `turnout`
          )) +
     geom_text(
          data = subset(turnout, `year` == "2014") %>% slice(c(seq(1, 9, 2))),
          aes(
               label = `country`,
               colour = `country`,
               x = `year` + 3,
               y = `turnout`
          )) +
     theme(axis.title.x=element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank()) +
     scale_color_viridis_d(9, guide = "none")
