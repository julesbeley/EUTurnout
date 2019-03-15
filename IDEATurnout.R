rm(list = ls())

library(sf)
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

eastgermany <- rbind(read_sf("Berlin_AL4.geojson"), 
                     read_sf("Berlin_AL6.geojson"))



east <- list()
for (i in (1:dim(eastgermany)[1])) east[[i]] <- eastgermany[i,] 

EG1 <- list()
for (i in (1:length(east))) try(EG1[[i]] <- as.data.frame(east[[i]]$geometry[[1]][[1]]), silent = TRUE)
for (i in (1:length(EG1))) if (is.null(EG1[[i]])) try(EG1[[i]] <- as.data.frame(east[[i]]$geometry[[1]][[1]][[1]]), silent = TRUE)
for (i in (1:10)) for (j in (1:12)) try(EG1[[i + 12]] <- as.data.frame(east[[j]]$geometry[[1]][[1]][[i]]), silent = TRUE) 
for (i in (1:length(EG1))) colnames(EG1[[i]]) <- c("x", "y")

EE <- do.call("rbind", EG1)

patch <- read_sf("patch.geojson")
patch <- as.data.frame(patch$geometry[[1]][[1]])
colnames(patch) <- c("x", "y")
EE <- rbind(EE, patch) 

length <- dim(as.data.frame(concaveman::concaveman(as.matrix(EE), concavity = 1)))[1]
u <- matrix(ncol = 1, nrow = length, 1628)

cbind(as.data.frame(concaveman::concaveman(as.matrix(EE), concavity = 1)), 
      as.vector(u), 
      seq(from = 100965, to = 100965 + length - 1),
      matrix(ncol = 1, nrow = length, "East Germany"),
      matrix(ncol = 1, nrow = length, NA)) -> EG

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
     theme(
          panel.background = element_rect(fill = "white",
                                          color = NA),
          panel.grid = element_line(colour = "royalblue"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
     ) -> mt
     if (date < 1994) {
          world <- map_data("world")
          colnames(EG) <- colnames(world)
          end <- rbind(world, EG)
          end %>%
               mutate(`region` = recode(`region`, "UK" = "United Kingdom")) -> world
          
          tojoin <- data.frame(region = names(table(world$region)))
          all <- full_join(turnout, tojoin, by = "region")
          all %>%
               arrange(`region`) -> all
          mapbig <- inner_join(world, all, by = "region")
          
          worldmap <- ggplot() + mt
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
               limits = c(0, 100),
               option = "magma",
               direction = -1,
               guide = "none",
               na.value = "grey70"
          )
          europe <- europe + annotate("text", -10, 68, label = as.character(date), size = 35)
     }
          
     else {
     world <- map_data("world")
     world %>%
          mutate(`region` = recode(`region`, "UK" = "United Kingdom")) -> world
     
     tojoin <- data.frame(region = names(table(world$region)))
     all <- full_join(turnout, tojoin, by = "region")
     all %>%
          arrange(`region`) -> all
     mapbig <- inner_join(world, all, by = "region")
     
     worldmap <- ggplot() + mt
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
          limits = c(0, 100),
          option = "magma",
          direction = -1,
          guide = "none",
          na.value = "grey70"
     )
     europe <- europe + annotate("text", -10, 68, label = as.character(date), size = 35)
     }
}

plots <- list()
for (i in (seq(from = 1979, to = 2014, by = 5))) plots[[(i - 1974) / 5]] <- eumap(i)

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
     breaks = seq(from = 0, to = 100, by = 10),
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
plots[[9]] <- legend

lay <- rbind(c(NA, NA, NA, NA, NA, NA), 
             c(NA, 1, 2, 3, 4, NA), 
             c(NA, 5, 6, 7, 8, NA), 
             c(NA, NA, 9, 9, NA, NA))

png("./participation.png", height = 4200, width = 6600)

grid.arrange(
     grobs = plots,
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

#curl -f -o eastgermany.zip --url "https://wambachers-osm.website/boundaries/exportBoundaries?cliVersion=1.0&cliKey=6f4b0380-1ef1-4cdf-ae75-0ce88e32e15a&exportFormat=json&exportLayout=levels&exportAreas=water&union=false&selected=62422,62504,1739381,1739376,1739380,62405,1739377,62685,1431517,1739379,62607,62467,62366"