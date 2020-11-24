library(tidyverse)
library(tidycensus)
library(plotly)

#install census api key for future session use
#census_api_key(key = "9de2c06af35b38352b1a400e0d2b53ecf2488a3f", install = TRUE)
#run this to load census api key and allow queries of census data
readRenviron("~/.Renviron")
#getting state population for 2019
state_population <- get_estimates( geography = "state", year = "2019", variables = "POP")


#load data
#covid_master <- read_csv("./data/master_covid_election.csv")
#covid_master_dates <- read_csv("./data/master_covid_election_with_dates.csv")

#adding state population to covid_master
state_population <- state_population %>%
  rename(state = NAME,
         state_population = value) %>%
  mutate(state = str_to_lower(state)) %>%
  select(-GEOID, -variable)
  
covid_master_statepop <- master_covid_election_with_dates %>%
  left_join(state_population, by = "state") %>%
  mutate(state_abb = factor(state_abb),
         cases_biden = cases * percent_biden,
         cases_trump = cases * (1 - percent_biden))
  
fig <- covid_master_statepop %>%
  mutate(state_abb = fct_reorder(state_abb, state_population)) %>%
  ggplot(aes(x = date, y = cases, fill = winner)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())
 
figggplotly(fig)
