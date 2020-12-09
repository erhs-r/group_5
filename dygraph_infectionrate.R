####data specific to Chart 2 ####

#Add county,ST label column for top infected county line labels
master_covid_election_with_dates <- master_covid_election_with_dates %>%
  mutate(county_st = paste(str_to_title(county), state_abb, sep = ", "))

#counties with highest infection rate
plot_data_topcounties_infect <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k, county_st) %>%
  group_by(winner, county_st) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) %>%
  slice_max(n = 3, order_by = mean_infect_100k)

biden_highest_infect <- plot_data_topcounties_infect %>% filter(winner == "biden") %>% pull(county_st)
trump_highest_infect <- plot_data_topcounties_infect %>% filter(winner == "trump") %>% pull(county_st)

#counties with highest death rate
plot_data_topcounties_death <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k, county_st) %>%
  group_by(winner, county_st) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) %>%
  slice_max(n = 3, order_by = mean_death_100k)

biden_highest_death <- plot_data_topcounties_death %>% filter(winner == "biden") %>% pull(county_st)
trump_highest_death <- plot_data_topcounties_death %>% filter(winner == "trump") %>% pull(county_st)


make_top_counties_df <- function(county_st_vect, df, candidate, i_or_d) {
  #county_st_vect = character vector of "County, ST" strings of counties
  #df = dataframe with column containing strings that match to those in county_st
  #candidate = "biden" or "trump"
  #i_or_d = "infect" or "death"
  #outputs list of dataframes filtered only to a single element in county_st_vect named data_list
  dfs_list <- list()
  names <- c()
  for (i in 1:length(county_st_vect)){
    dfs_list[[i]] <- df[df$county_st == county_st_vect[i], ]
    names <- c(names, str_replace(paste("df", 
                                        county_st_vect[i], 
                                        sep = "_"), pattern = ", ", "_"))
  }
  names <- str_replace_all(names, " ", "_")
  df_names <- c()
  for (j in names){
    df_names <- c(df_names, paste(j,candidate, i_or_d, sep = "_"))
  }
  names(dfs_list) <- df_names
  dfs_list <- map(dfs_list, ~select(.x, date, infection_rate_per_100k, death_rate_per_100k))
  list2env(dfs_list, envir = .GlobalEnv)
}

#creating dataframes for individual counties with highest infection and death rates that went trump or biden
make_top_counties_df(biden_highest_death, master_covid_election_with_dates, "biden", "death")
make_top_counties_df(trump_highest_death, master_covid_election_with_dates, "trump", "death")
make_top_counties_df(biden_highest_infect, master_covid_election_with_dates, "biden", "infect")
make_top_counties_df(trump_highest_infect, master_covid_election_with_dates, "trump", "infect")

#making character vector of dataframe names
dfs_char_vect <- ls() %>%
  str_subset(pattern = "^df_")

#renaming dataframe columns to contain county name and trump or biden information
for (i in 1:length(dfs_char_vect)){
  x <- get(dfs_char_vect[i])
  if (str_detect(dfs_char_vect[i], "biden") & str_detect(dfs_char_vect[i], "infect")){
    new_cols <- c("date", paste("infect_100k_biden", dfs_char_vect[i], sep = "_"), paste("death_100k_biden", dfs_char_vect[i], sep = "_"))
    colnames(x) <- new_cols
    assign(dfs_char_vect[i],x)
  }
  else if (str_detect(dfs_char_vect[i], "biden") & str_detect(dfs_char_vect[i], "death")){
    new_cols <- c("date", paste("infect_100k_biden", dfs_char_vect[i], sep = "_"), paste("death_100k_biden", dfs_char_vect[i], sep = "_"))
    colnames(x) <- new_cols
    assign(dfs_char_vect[i],x)
  }
  else if (str_detect(dfs_char_vect[i], "trump") & str_detect(dfs_char_vect[i], "death")){
    new_cols <- c("date", paste("infect_100k_trump", dfs_char_vect[i], sep = "_"), paste("death_100k_trump", dfs_char_vect[i], sep = "_"))
    colnames(x) <- new_cols
    assign(dfs_char_vect[i],x)
  } 
  else if (str_detect(dfs_char_vect[i], "trump") & str_detect(dfs_char_vect[i], "infect")){
    new_cols <- c("date", paste("infect_100k_trump", dfs_char_vect[i], sep = "_"), paste("death_100k_trump", dfs_char_vect[i], sep = "_"))
    colnames(x) <- new_cols
    assign(dfs_char_vect[i],x)
  }
}

#combining all the dataframes together by date
top_infect_death <- get(dfs_char_vect[1])
for (i in 2:length(dfs_char_vect)){
  x <- get(dfs_char_vect[i])
  top_infect_death <- full_join(top_infect_death, x, by = "date")
}

#remove columns that don't have either "infect" or "death" twice
keep <- c()
for (i in colnames(top_infect_death)){
  if (str_detect(i, "^death.*death$") | str_detect(i, "^infect.*infect$") | i == "date"){
    keep <- c(keep, TRUE)
  }
  else {
    keep <- c(keep, FALSE)
  }
}

top_infect_death <- top_infect_death[,keep]

#averaging infection rate per 100k and death rate per 100k for biden and trump counties
plot_data <- master_covid_election_with_dates %>%
  select(date, winner, infection_rate_per_100k, death_rate_per_100k) %>%
  group_by(date, winner) %>%
  summarize(mean_infect_100k = mean(infection_rate_per_100k),
            mean_death_100k = mean(death_rate_per_100k)) 

#separating only to trump counties
plot_data_trump <- plot_data %>%
  filter(winner == "trump") %>%
  rename(mean_infect_100k_trump = mean_infect_100k,
         mean_death_100k_trump = mean_death_100k) %>%
  select(-winner) 

#separating only to biden counties
plot_data_biden <- plot_data %>%
  filter(winner == "biden") %>%
  rename(mean_infect_100k_biden = mean_infect_100k,
         mean_death_100k_biden = mean_death_100k) %>%
  select(-winner) 

#joining data
plot_data <- plot_data_biden %>%
  full_join(plot_data_trump, by = "date") %>%
  full_join(top_infect_death, by = "date")

#setting NA values to 0
plot_data[is.na(plot_data)] <- 0

#creating data for infection rate plot and death rate plot
plot_data_infect <- plot_data %>%
  select(date, contains("infect")) 

plot_data_death <- plot_data %>%
  select(date, contains("death")) 

#converting to timeseries objects
plot_data_infect <- xts(plot_data_infect[,-1], order.by = plot_data_infect$date)
plot_data_death <- xts(plot_data_death[,-1], order.by = plot_data_death$date)

#generating infection dygraph
dy_infect <- dygraph(data = plot_data_infect,
                     ylab = "Infection Rate / 100k People") %>%
  dySeries("mean_infect_100k_biden", 
           label = "Biden Counties Avg.",
           fillGraph = TRUE,
           color = "blue") %>%
  dySeries("infect_100k_biden_df_Buffalo_SD_biden_infect", 
           label = "Buffalo, SD",
           color = "blue",
           strokePattern = "dotted") %>%
  dySeries("infect_100k_biden_df_East_Carroll_LA_biden_infect",
           label = "East Carroll, LA",
           color = "blue",
           strokePattern = "dotted") %>%
  dySeries("infect_100k_biden_df_Lee_AR_biden_infect", 
           label = "Lee, AR",
           color = "blue",
           strokePattern = "dotted") %>%
  dySeries("mean_infect_100k_trump", 
           label = "Trump Counties Avg.",
           fillGraph = TRUE,
           color = "red") %>%
  dySeries("infect_100k_trump_df_Lake_TN_trump_infect", 
           label = "Lake, TN",
           color = "red",
           strokePattern = "dotted") %>%
  dySeries("infect_100k_trump_df_Lincoln_AR_trump_infect", 
           label = "Lincoln, AR",
           color = "red",
           strokePattern = "dotted") %>%
  dySeries("infect_100k_trump_df_Trousdale_TN_trump_infect", 
           label = "Trousdale, TN",
           color = "red",
           strokePattern = "dotted") %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "auto", width = 175, labelsSeparateLines = TRUE) %>%
  dyCSS("dy_legend_position.css")


#generating death dygraph
dy_death <- dygraph(data = plot_data_death,
                    main = "County Death Rate by 2020 Presidential Election Winner",
                    ylab = "Death Rate / 100k People") %>%
  dySeries("mean_death_100k_biden", 
           label = "Biden Counties Avg.",
           fillGraph = TRUE,
           color = "blue") %>%
  dySeries("death_100k_biden_df_Randolph_GA_biden_death", 
           label = "Randolph, GA",
           color = "blue") %>%
  dySeries("death_100k_biden_df_Hancock_GA_biden_death", 
           label = "Hancock, GA",
           color = "blue") %>%
  dySeries("death_100k_biden_df_Terrell_GA_biden_death", 
           label = "Terrell, GA",
           color = "blue") %>%
  dySeries("mean_death_100k_trump", 
           label = "Trump Counties Avg.",
           fillGraph = TRUE,
           color = "red") %>%
  dySeries("death_100k_trump_df_Early_GA_trump_death", 
           label = "Early, GA",
           color = "red") %>%
  dySeries("death_100k_trump_df_Kenedy_TX_trump_death", 
           label = "Kenedy, TX",
           color = "red") %>%
  dySeries("death_100k_trump_df_Perkins_NE_trump_death", 
           label = "Perkins, NE",
           color = "red") %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(show = "follow", width = 400, labelsSeparateLines = TRUE) 