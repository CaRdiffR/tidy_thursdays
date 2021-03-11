library(tidyverse)
# script 10 Mar 2021

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')



# what happens to rating over time... 
ggplot(raw_bechdel, aes(x = year, y = rating)) + geom_line()

ggplot(raw_bechdel, aes(x = year, y = rating)) + geom_point()

ggplot(raw_bechdel, aes(x = year, y = rating)) + geom_point(alpha = 0.05)

# use group_by and summarise to look over time again...       
raw_bechdel %>%
           group_by(year) %>%
           summarise(mean = mean(rating), n = n()) %>%
    ggplot(aes(year, mean)) + geom_line()

# plot with points...  
raw_bechdel %>%
    group_by(year) %>%
    summarise(mean = mean(rating), n = n()) %>%
    ggplot(aes(year, mean)) + geom_point()

# add points and use geom_smooth 
raw_bechdel %>%
    group_by(year) %>%
    summarise(mean = mean(rating), n = n()) %>%
    ggplot(aes(year, mean)) + geom_point() + 
    geom_smooth() +
    ylim(0,3) +
    xlim(1880, 2020) -> graph

graph

graph + theme_classic() 

## add titles - based on the values put in...
graph <-  graph +
    labs(x = NULL, y = "Mean Bechdel Score",
         title = "Change in Bechdel Test over time",
         caption = "\nVisualisation: @brennanpcardiff) | Data from FiveThirtyEight)")

graph + theme_classic() 

# like the look of this graph... maybe things are going in the right direction
# SLOWLY....



