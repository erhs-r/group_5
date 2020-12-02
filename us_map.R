library(tidyverse)
library(tigris)
library(leaflet)

source("data_cleaning.R")

counties <- counties(state = c("WA", "OR", "CA", "NV", "AZ", "NM", "CO", "MN", "WI", "AK", 
                               "IL", "MI", "GA", "PA", "VA", "NY", "VT", "NH", "MA", "RI", 
                               "CT", "NJ", "DE", "MD", "ID", "UT", "MT", "WY", "ME", "HI",
                               "ND", "SD", "KS", "OK", "TX", "LA", "IA", "MO", "AR", "MS",
                               "AL", "TN", "KY", "IN", "OH", "WV", "NC", "SC", "FL", "NE"),
                     cb = TRUE, class = "sf") %>% 
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
  select(fips, geometry)

counties_data <- counties %>% 
  inner_join(master_covid_election, by = "fips") %>% 
  mutate(winner = str_to_title(winner),
         popup_text = paste("<b>State:</b>", str_to_title(state), "<br>",
                            "<b>County:</b>", str_to_title(county), "<br>"))

pal <- colorFactor(c("#2E3FD5", "#E7191C"), counties_data$winner)

leaflet() %>% 
  setView(lng = -110, lat = 40, zoom = 3) %>% 
  addTiles() %>% 
  addPolygons(data = counties_data, color = "#000000", weight = 0.5,
              fillColor = ~ pal(winner), popup = ~ popup_text) %>% 
  addLegend(data = counties_data, pal = pal, values = ~ winner,
            title = "Winner")
  

