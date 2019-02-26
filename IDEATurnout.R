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

library(sf)
eastgermany <- read_sf("C:/Users/jules/Documents/Hamburg Bars/Limits.geojson")

as.data.frame(eastgermany$geometry[[1]][[1]]) 

ggplot(as.data.frame(eastgermany$geometry[[2]][[1]])) +
     geom_polygon(aes(x = X1, y = X2)) +
     coord_map(
          "lambert",
          parameters = c(30, 43)
     )

map_data("world") -> world



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
     #if (date < 1994) {
          europe <- europe + geom_polygon()
     #}
     europe <- europe + scale_fill_viridis_c(
          limits = c(13, 93),
          option = "magma",
          direction = -1,
          guide = "none",
          na.value = "grey70"
     )
     europe <- europe + annotate("text", -10, 68, label = as.character(date), size = 35)
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
     legend.title = element_text(size = 130),
     legend.position = "bottom"
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
     limits = c(0, 100),
     option = "magma",
     direction = -1,
     na.value = "grey70",
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
     guide = guide_colorbar(
          barheight = unit(40, units = "mm"),
          barwidth = unit(1000, units = "mm"),
          title = "%",
          title.hjust = 0.5,
          title.vjust = 1,
          ticks.linewidth = 6,
          title.position = "right"
     )
)

legend <- get_legend(europe)
legend <- as_ggplot(legend)

lay <- rbind(c(NA, NA, NA, NA, NA, NA), 
             c(NA, 1, 2, 3, 4, NA), 
             c(NA, 5, 6, 7, 8, NA), 
             c(NA, NA, 9, 9, NA, NA))

png("./participation.png", height = 4200, width = 6600)

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
     top = textGrob(
          "Voter turnout in EU elections, 1979-2014",
          gp = gpar(fontsize = 180),
          y = 0.5,
          x = 0.5
     ),
     right = textGrob(
          "Source: International IDEA. Computed by J. Beley (2019).",
          gp = gpar(fontsize = 100,
                    fontface = 3),
          rot = 90,
          vjust = 0
     ),
     heights = c(0.5, 4, 4, 1),
     widths = c(0.07, 1, 1, 1, 1, 0.07),
     layout_matrix = lay
)

dev.off()


curl -f -o eastgermany.zip --url "https://wambachers-osm.website/boundaries/exportBoundaries?cliVersion=1.0&cliKey=6f4b0380-1ef1-4cdf-ae75-0ce88e32e15a&exportFormat=json&exportLayout=levels&exportAreas=water&union=false&selected=62422,62504,1739381,1739376,1739380,62405,1739377,62685,1431517,1739379,62607,62467,62366"