library(tidytuesdayR)
library(tidyverse)

tt_data <- tt_load("2020-01-14")

tt_data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

tt_data$passwords %>% view()

passwords %>% 
  mutate(cat= as.factor(category)) %>% 
  mutate(cat= fct_infreq(cat)) %>% 
  ggplot(aes(cat)) + 
  geom_bar()

passwords %>% 
  count(time_unit)

passwords %>% 
  filter(seconds= duration()= "days")

lubridate::duration()


passwords %>% 
  mutate( time_unit_sec= case_when(
    time_unit =="months" ~ 30*24*60*60* value,
    time_unit =="days" ~ 24*60*60*value,
    time_unit =="minutes" ~ 60* value, 
    time_unit =="hours" ~ 60*60* value, 
    time_unit =="seconds" ~ value, 
    time_unit =="weeks" ~ 7*24*60*60, 
    time_unit =="weeks" ~ *12*30*24*60*60* value,
    TRUE ~ "NA"
    
  )
)
  
