source("data_cleaning.R")
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(ggbeeswarm)
library (plotly)

#calculate case_rate per state 
masks <- master_covid_election %>% 
  mutate(state_infection_rate = (total_cases/ state_population)*100000, 
         total_mask = (frequently + always)*100)

#find top 3 states
#north dakota, south dakota, iowa
worst_case_state <- masks %>% 
  group_by(state) %>%
  select(state,state_infection_rate) %>% 
  unique() %>% 
  ungroup() %>% 
  top_n(1, wt = state_infection_rate) %>% 
  pull(state)

middle_case_state <-  masks %>%  
  group_by(state) %>%
  select(state,state_infection_rate) %>% 
  arrange(desc(state_infection_rate)) %>% 
  unique() %>% 
  ungroup() %>% 
  slice(25) %>% 
  pull(state)

best_case_state <-  masks %>%  
  group_by(state) %>%
  select(state,state_infection_rate) %>% 
  unique() %>% 
  ungroup() %>% 
  top_n(-1, wt = state_infection_rate) %>% 
  pull(state)

compare_states <- c(worst_case_state, best_case_state, middle_case_state)

#cleaned data for creating plot
state_mask <- masks %>%
  subset(state %in% compare_states) %>% 
  mutate (state = str_to_title(state), 
          winner = str_to_title(winner),
          county = str_to_title(county))

med <- state_mask %>% 
  group_by(state, winner) %>% 
  summarize( median_mask = median(total_mask)) %>% 
  ungroup %>% 
  inner_join(state_mask, by = c("state", "winner"))
   

#plot ggplot   
state_plot <- med %>% 
  ggplot(aes(x = winner, y = total_mask, 
             text = paste(state,":", "<br>",
                         "State Case per 100K:",round(state_infection_rate),"<br>", 
                        "County:", county, "<br>",
                        "County Case per 100k:", round(infection_rate_per_100k),"<br>",
                        "Median:", median_mask))) +
  geom_quasirandom(aes(color = winner, size = infection_rate_per_100k), 
                   alpha=.5, show.legend = FALSE)+
  facet_wrap(.~ fct_reorder(state, state_infection_rate, .fun = max, 
                            .desc = TRUE), ncol = 3) + 
  scale_y_continuous(labels=function(total_mask) paste0(total_mask,"%"))+
  scale_color_manual(values = c("blue", "red"))+
  labs(x ="", y = "") +
  scale_x_discrete(position ="top") +
  theme_bw()




mask_plot<- ggplotly(state_plot, 
                     tooltip = "text") %>% 
  layout(yaxis = list(titlefont = list(size = 15), title =
                        "% of people who frequently or\n always wear their mask"
                      , automargin = T))
         
         
         

#final plotlk
mask_plot

