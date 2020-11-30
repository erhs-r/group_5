library(tidyverse)
library(tidycensus)
library(plotly)

source("data_cleaning.R")

fig <- master_covid_election_with_dates %>%
  mutate(state_abb = fct_reorder(state_abb, state_population)) %>%
  group_by(county) %>%
  ggplot(aes(x = date, y = cases, fill = state_win)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("blue", "purple", "red")) +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) +
  facet_wrap(~state)

#ggplotly(fig)
fig
