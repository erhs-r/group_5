library(tidyverse)
library(tigris)
library(leaflet)
library(RColorBrewer)

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
         percent_biden = percent_biden*100,
         percent_trump = 100 - percent_biden,
         popup_text = paste(paste0("<h1>", str_to_title(county),", ", state_abb, "</h1>",
                            "<b>Biden: </b>", paste0(format(percent_biden, digits = 3), "%"), "<br>",
                            "<b>Trump: </b>", paste0(format(percent_trump, digits = 3), "%"), "<br>",
                            "<br>",
                            "Population: ", str_trim(format(population, big.mark = ","), side = "both"), "<br>",
                            "Infection Rate: ", paste0(infection_rate, "%"), "<br>",
                            "Death Rate: ", paste0(death_rate, "%"), "<br>",
                            "Mask Usage: ", paste0(mask_percent, "%"), "<br>")))

states <- states(cb = TRUE, class = "sf") %>% 
  select(STUSPS, geometry) %>% 
  rename(state_abb = STUSPS) %>% 
  filter(state_abb %in% counties_data$state_abb)

pal1 <- colorFactor(c("blue", "red"), counties_data$winner)
pal2 <- colorNumeric("Greys", counties_data$infection_rate)
pal3 <- colorNumeric("Greys", counties_data$death_rate)

us_map <- leaflet() %>% 
  setView(lng = -90, lat = 35, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = counties_data, color = "black", 
              smoothFactor = 0.5, weight = 0.5,
              fillColor = ~ pal1(winner), fillOpacity = 1, group = "Election") %>% 
  addPolygons(data = counties_data, color = "black",
              smoothFactor = 0.5, weight = 0.5,
              fillColor = ~ pal2(infection_rate), fillOpacity = .6,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = ~ paste0(str_to_title(county),", ",state_abb), labelOptions = labelOptions(direction = "auto"),
              popup = ~ popup_text, group = "Infection Rate") %>% 
  addPolygons(data = counties_data, color = "black",
              smoothFactor = 0.5, weight = 0.5,
              fillColor = ~ pal3(death_rate), fillOpacity = .6,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = ~ paste0(str_to_title(county),", ",state_abb), labelOptions = labelOptions(direction = "auto"),
              popup = ~ popup_text, group = "Death Rate") %>% 
  addPolylines(data = states, color = "black", opacity = 1, weight = 1, smoothFactor = 0.5, group = "Infection Rate") %>%
  addPolylines(data = states, color = "black", opacity = 1, weight = 1, smoothFactor = 0.5, group = "Death Rate") %>%
  addLegend("bottomright", data = counties_data, pal = pal2, values = ~ infection_rate,
            title = "Infection Rate", opacity = 1, labFormat = labelFormat(suffix = "%"), group = "Infection Rate") %>% 
  addLegend("bottomleft", data = counties_data, pal = pal3, values = ~ death_rate,
            title = "Death Rate", opacity = 1, labFormat = labelFormat(suffix = "%"), group = "Death Rate") %>% 
  addLegend("bottomright", data = counties_data, pal = pal1, values = ~ winner,
            title = "Winner", opacity = .6, group = "Election") %>% 
  addLayersControl(
    baseGroups = c("Infection Rate", "Death Rate"),
    overlayGroups = "Election",
    options = layersControlOptions(collapsed = FALSE)
  )
  
us_map  
  



