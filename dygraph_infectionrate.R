library(tidyverse)
library(dygraphs)
library(xts)
library(htmltools)

source("data_cleaning.R")

plot_data <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k) %>%
  group_by(date, winner) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) 

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
  
dygraph(plot_data[,c(1,3)],
        main = "County Infection Rate Average by 2020 Presidential Election Winner",
        ylab = "Infection Rate / 100k People") %>%
  dySeries("mean_infect_100k_biden", 
           label = "Biden Counties") %>%
  dySeries("mean_infect_100k_trump", 
           label = "Trump Counties") %>%
  dyOptions(stackedGraph = TRUE, colors = c("blue", "red")) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "always", width = 400) %>%
tagList(dygraph(plot_data[,c(2,4)],
        main = "County Death Rate Average by 2020 Presidential Election Winner",
        ylab = "Death Rate / 100k People") %>%
  dySeries("mean_death_100k_biden", 
           label = "Biden Counties") %>%
  dySeries("mean_death_100k_trump", 
           label = "Trump Counties") %>%
  dyOptions(stackedGraph = TRUE, colors = c("blue", "red")) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "always", width = 400)) %>%
  browsable()

