library(scales)
library(tidyverse)
library(purrr)
library(tigris)
library(viridis)

source("data_cleaning.R")

counties <- counties(state = c("WA", "OR", "CA", "NV", "AZ", "NM", "CO", "MN", "WI", 
                               "IL", "MI", "GA", "PA", "VA", "NY", "VT", "NH", "MA", "RI", 
                               "CT", "NJ", "DE", "MD", "ID", "UT", "MT", "WY", "ME",
                               "ND", "SD", "KS", "OK", "TX", "LA", "IA", "MO", "AR", "MS",
                               "AL", "TN", "KY", "IN", "OH", "WV", "NC", "SC", "FL", "NE"),
                     cb = TRUE, class = "sf") %>% 
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
  select(fips, geometry)

counties_data <- counties %>% 
  inner_join(master_covid_election, by = "fips")
  
ggplot() +
  geom_sf(data = counties_data, aes(fill = winner,
                                    )) +
  scale_fill_discrete(type = c("blue", "red")) +
  theme(legend.position = "bottom") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
