library(tidyverse)

#Initial reading of the main data file
covid_counties <- read_csv("data_raw/us-counties.csv")
election_counties <- read_csv("data_raw/presidential_election_counties.csv")
masks_counties <- read_csv("data_raw/mask-use-by-county.csv")
# This is from :
#https://github.com/balsama/us_counties_data/blob/main/data/counties.csv#L15
population_counties <- read_csv("data_raw/population_counties.csv")

#-------------------------------------------------------------------------------

#Cleaning up county population data
population_counties_clean <- population_counties %>% 
  rename_all(str_to_lower) %>% 
  mutate(county = str_to_lower(county),
         state = str_to_lower(state)) %>% 
  rename(fips = 'fips code') %>% 
  select(-county, -state)

#Selecting only Trump & Biden results (along with other county identifying columns)
#Also creating percent_biden column that can be used for gradient stuff
election_counties_clean <- election_counties %>% 
  select(fips, name, state, results_trumpd, results_absentee_trumpd,
         results_bidenj, results_absentee_bidenj) %>% 
  mutate(name = str_to_lower(name),
         trump_total = results_trumpd + results_absentee_trumpd,
         biden_total = results_bidenj + results_absentee_bidenj,
         percent_biden = biden_total / (biden_total + trump_total),
         winner = if_else(biden_total > trump_total, "biden", "trump")) %>% 
  rename(county = name)

#Filtering covid df to only have the latest data (11/17/2020) and selecting important columns
covid_counties_clean <- covid_counties %>% 
  filter(date == "2020-11-17") %>% 
  select(fips, county, state, cases, deaths) %>% 
  mutate(county = str_to_lower(county),
         state = str_to_lower(state))

# CHECKING TO MAKE SURE CUMULATIVE AND NOT ACTIVE CASES
# covid_counties %>% 
#   filter(fips == "04013") %>% 
#   ggplot(aes(date, cases)) +
#   geom_lin

# each participant was asked: How often do you wear a mask in public when you expect to be within six feet of another person?
# 250,000 survey responses between July 2 and July 14
# mask_percent is just what I thought would give the % of people who "commonly" wear a mask in the county when in public
masks_counties_clean <- masks_counties %>% 
  rename_all(str_to_lower) %>% 
  rename(fips = countyfp) %>% 
  mutate(mask_percent = frequently + always)

#Combining Data Sets
master_covid_election <- covid_counties_clean %>% 
  inner_join(population_counties_clean, by = "fips") %>% 
  inner_join(election_counties_clean, by = c("fips", "county", "state")) %>% 
  inner_join(masks_counties_clean, by = "fips")

# #Just looking at what counties had the highest death rate
# master_covid_election %>% 
#   mutate(death_rate = (deaths / population) * 100) %>% 
#   select(state, county, death_rate) %>% 
#   arrange(desc(death_rate))



