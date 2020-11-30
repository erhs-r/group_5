library(tidyverse)
library(dygraphs)

source("data_cleaning.R")

plot_data <- master_covid_election_with_dates %>%
  

lungDeaths <- cbind(mdeaths, fdeaths)

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)
