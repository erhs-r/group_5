library(tidyverse)
library(dygraphs)
library(xts)
library(gridExtra)

source("data_cleaning.R")

#Add county,ST label column for top infected county line labels
master_covid_election_with_dates <- master_covid_election_with_dates %>%
  mutate(county_st = paste(str_to_title(county), state_abb, sep = ", "))

#counties with highest infection rate
plot_data_topcounties_infect <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k, county_st) %>%
  group_by(winner, county_st) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) %>%
  slice_max(n = 5, order_by = mean_infect_100k)

#counties with highest death rate
plot_data_topcounties_death <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k, county_st) %>%
  group_by(winner, county_st) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) %>%
  slice_max(n = 5, order_by = mean_death_100k)

plot_data <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k) %>%
  group_by(date, winner) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k),
            county_st = county_st) 

plot_data_trump <- plot_data %>%
  filter(winner == "trump") %>%
  rename(mean_infect_100k_trump = mean_infect_100k,
         mean_death_100k_trump = mean_death_100k) %>%
  select(-winner) 
 
plot_data_biden <- plot_data %>%
  filter(winner == "biden") %>%
  rename(mean_infect_100k_biden = mean_infect_100k,
         mean_death_100k_biden = mean_death_100k) %>%
  select(-winner) 
 
plot_data <- plot_data_biden %>%
  full_join(plot_data_trump, by = "date") 

plot_data[is.na(plot_data)] <- 0

plot_data <- xts(plot_data[,-1], order.by = plot_data$date)
  
dy_infect <- dygraph(plot_data[,c(1,3)],
        main = "County Infection Rate Average by 2020 Presidential Election Winner",
        ylab = "Infection Rate / 100k People") %>%
  dySeries("mean_infect_100k_biden", 
           label = "Biden Counties") %>%
  dySeries("mean_infect_100k_trump", 
           label = "Trump Counties") %>%
  dyOptions(stackedGraph = TRUE, colors = c("blue", "red")) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "always", width = 400) 

dy_death <- dygraph(plot_data[,c(2,4)],
        main = "County Death Rate Average by 2020 Presidential Election Winner",
        ylab = "Death Rate / 100k People") %>%
  dySeries("mean_death_100k_biden", 
           label = "Biden Counties") %>%
  dySeries("mean_death_100k_trump", 
           label = "Trump Counties") %>%
  dyOptions(stackedGraph = TRUE, colors = c("blue", "red")) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "always", width = 400)

