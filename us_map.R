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
         popup_text = paste(paste0("<b>", str_to_title(county), ",</b>"), "<b>", state_abb, "</b><br>",
                            "Infection Rate: ", paste0((infection_rate), "%"), "<br>",
                            "Mask Usage: ", paste0((mask_percent), "%")))

states <- states(cb = TRUE, class = "sf") %>% 
  select(STUSPS, geometry) %>% 
  rename(state_abb = STUSPS) %>% 
  filter(state_abb %in% counties_data$state_abb)

pal <- colorFactor(c("blue", "red"), counties_data$winner)

leaflet() %>% 
  setView(lng = -100, lat = 40, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = counties_data, color = "black", 
              smoothFactor = 0.5, weight = 0.5,
              fillColor = ~ pal(winner), fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              popup = ~ popup_text) %>% 
  addPolylines(data = states, color = "black", opacity = 1, weight = 1, smoothFactor = 0.5) %>%
  addLegend(data = counties_data, pal = pal, values = ~ winner,
            title = "Winner", opacity = 1)
  
