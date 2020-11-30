library(tidyverse)

source("data_cleaning.R")

#plot
master_covid_election %>%
  ggplot(aes(size = cases)) +
  geom_point(aes(x = state_win, y = winner, color = winner), 
             position = position_jitter(width = .3, height = .5),
             alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_size(range = c(0.5, 10)) 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_text(),
        axis.ticks.y = element_blank())