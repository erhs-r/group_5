<<<<<<< HEAD
library(tidyverse)

#load data
covid_master <- read_csv("./data/master_covid_election.csv")
covid_master_dates <- read_csv("./data/master_covid_election_with_dates.csv")

# #states won biden or trump. source = https://www.270towin.com/2020-election-results-live/
# biden_states <- c("WA", "OR", "CA", "NV", "AZ", "NM", "CO", "HI", "MN", "WI", 
#                   "IL", "MI", "GA", "PA", "VA", "NY", "VT", "NH", "MA", "RI", 
#                   "CT", "NJ", "DE", "MD", "DC")
# trump_states <- c("AK", "ID", "UT", "MT", "WY", "ND", "SD", "KS", "OK", "TX", 
#                   "LA", "IA", "MO", "AR", "MS", "AL", "TN", "KY", "IN", "OH", 
#                   "WV", "NC", "SC", "FL")
# split_states <- c("NE", "ME")
# 
# #adding state win column
# covid_master_state <- covid_master %>%
#   mutate(state_win = case_when(state_abb %in% biden_states ~ "biden",
#                                state_abb %in% trump_states ~ "trump",
#                                state_abb %in% split_states ~ "split",
#                                is.na(winner) & state %in% biden_states ~ "biden",
#                                is.na(winner)& state %in% trump_states ~ "trump",
#                                is.na(winner)& state %in% split_states ~ "split"))
# 
# #adding total cases per state column
# total_state_cases <- covid_master_state %>%
#   group_by(state_abb) %>%
#   summarise(total_cases = sum(cases))
# 
# covid_master_state <- covid_master_state %>%
#   inner_join(total_state_cases, by = "state_abb") %>%
#   mutate(state = factor(state),
#          state_abb = factor(state_abb))
# 
# # replacing NAs in winner column with info from state_win
# covid_master_state <- covid_master_state %>%
#   mutate(winner = case_when(is.na(winner) ~ state_win,
#                             !is.na(winner) ~ winner))
# #replacing NAs in other columns with 0's
# covid_master_state[is.na(covid_master_state)] <- 0

#plot
covid_master_state %>%
  ggplot(aes(size = cases)) +
  geom_point(aes(x = state_win, y = winner, color = winner), 
             position = position_jitter(width = .3, height = .5),
             alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_size(range = c(0.5, 10)) 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_text(),
        axis.ticks.y = element_blank())
=======

>>>>>>> 2068e19526c12a3054dc8b6dbeec7f5ccb03247a
