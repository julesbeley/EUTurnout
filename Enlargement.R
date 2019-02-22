library(tidyverse)

rbind(matrix(ncol = 1, nrow = 72 - 56, 6), 
      matrix(ncol = 1, nrow = 80 - 72, 9),
      matrix(ncol = 1, nrow = 85 - 80, 10),
      matrix(ncol = 1, nrow = 94 - 85, 12),
      matrix(ncol = 1, nrow = 103 - 94, 15),
      matrix(ncol = 1, nrow = 106 - 103, 25),
      matrix(ncol = 1, nrow = 112 - 106, 27),
      matrix(ncol = 1, nrow = 114 - 112, 28)) -> b

b <- data.frame(years = c(1957:2014),
                nms = as.vector(b)) 

ggbarplot(b, x = "years", y = "nms") +
     ylab("Number of Member States") +
     scale_x_continuous(
          name = "",
          breaks = seq(1957, 2014, length = 20)
     )

table(b$nms)

