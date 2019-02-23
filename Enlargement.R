rm(list = ls())
library(tidyverse)

rbind(
     matrix(ncol = 1, nrow = 80 - 78, 9),
     matrix(ncol = 1, nrow = 85 - 80, 10),
     matrix(ncol = 1, nrow = 94 - 85, 12),
     matrix(ncol = 1, nrow = 103 - 94, 15),
     matrix(ncol = 1, nrow = 106 - 103, 25),
     matrix(ncol = 1, nrow = 112 - 106, 27),
     matrix(ncol = 1, nrow = 113 - 112, 28)
) -> b

b <- data.frame(years = c(1979:2013),
                nms = as.vector(b),
                stringsAsFactors = FALSE)

enlarge <- c()
enlarge[1] <- 1979
for (i in (1:34)) {
     if (b$nms[i] != b$nms[i + 1]) {
          enlarge[i] <- b$years[i + 1]
     }
}

enlarge <- enlarge[!is.na(enlarge)]
enlarge <- cbind(enlarge, table(b$nms), NA)
enlarge[, 3] <- rownames(enlarge)
colnames(enlarge) <- c("year", "duration", "nms")
enlarge <- as.data.frame(enlarge)
rownames(enlarge) <- NULL
enlarge$year <- as.numeric(as.character(enlarge$year))
enlarge$nms <- as.numeric(as.character(enlarge$nms))
enlarge$duration <- as.numeric(as.character(enlarge$duration))

p <- ggplot(enlarge, aes(ymin = 0)) +
     geom_rect(aes(
          xmin = year,
          xmax = year + duration,
          ymax = nms
     ),
     color = NA,
     fill = "dodgerblue4",
     alpha = 0.7) +
     theme(
          plot.title = element_text(size = 60, hjust = 0.5, vjust = 50),
          plot.caption = element_text(size = 30, face = 3, vjust = -20),
          axis.text.x = element_text(size = 36, colour = "black", angle = 70, hjust = 1),
          axis.text.y = element_text(size = 38, colour = "black"),
          axis.title.y = element_text(size = 45, vjust = 25),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey", size = 1.3),
          panel.grid.major.y = element_line(
               color = "brown2",
               linetype = 2,
               size = 1.3
          ),
          plot.margin = unit(c(4.5, 7.5, 4.5, 7.995), "cm")
     ) +
     scale_x_continuous(limits = c(1970, 2025), 
                        breaks = c(1979, 1981, 1986, 1995, 2004, 2007, 2013, 2014)) +
     scale_y_continuous(limits = c(0,28), breaks = c(0, 9, 10, 12, 15, 25, 27, 28)) +
     labs(title = "EU enlargements, 1979-2014",
          caption = "Source: International IDEA. Computed by J. Beley (2019)") +
     ylab("Number of member states")

png("./enlarge.png", width = 2400, height = 1400)
p
dev.off()
