# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO UPDATE tidytuesdayR from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-01-14') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 3)


passwords <- tuesdata$passwords


library(dplyr)
category_counts<-passwords %>%
  filter(!is.na(category))%>%
  count(category) %>%
  arrange(n) %>%
  mutate(category = factor(category, levels = category))

passwords %>%
  filter(category == "nerdy-pop")
library(ggplot2)

#plotting
ggplot(category_counts, aes(x=category, y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))
  
  
  