# a script for Tidy Tuesday and Tidy Thursday at CaRdiff UseR group 
# 30 April 2020

library(tidyverse)
# pull in the data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

# How many unique shows...
length(unique(grosses$show))
# 1122

# inspration from here: https://github.com/teunbrand/tidytuesdayscripts/blob/master/scripts/2020_04_28_Broadway_Grosses.R
# and image from here: https://twitter.com/TeunvandenBrand/status/1255253561535074306/photo/1

# first make basic graph = average ticket prices over time...

ggplot(grosses, aes(week_ending, avg_ticket_price)) +
    geom_point(alpha = 0.25)
# $500 shows depress the axis a bit...

grosses %>%
    filter(avg_ticket_price > 400) -> expensive
# this show is Springsteen on Broadway at $500!!!
# can exclude by limiting the y-axis

# filter for the Phantom
grosses %>%
    filter(show == "The Phantom of the Opera") %>%
    ggplot(aes(week_ending, avg_ticket_price, colour=pct_capacity)) +
    geom_point() +
    geom_smooth() + 
    labs(x = "",
        y = "Average Ticket Price ($)", 
        title = "Phantom of the Opera",
        subtitle = "Data from Playbill via Tidy Tuesday") 

# why does the show seem very expensive for some weeks rather than others

grosses %>%
    filter(show == "Mamma Mia!") %>%
    ggplot(aes(week_ending, avg_ticket_price, colour=pct_capacity)) +
    geom_point() +
    geom_smooth() + 
    labs(x = "",
        y = "Average Ticket Price ($)", 
        title = "Phantom of the Opera",
        subtitle = "Data from Playbill via Tidy Tuesday") 


# how does Phantom of the Opera compare to the others?
plot <- ggplot(grosses, aes(week_ending, avg_ticket_price)) +
    geom_point(alpha = 0.25, size = 0.5) + ylim(0,300)

grosses %>%
    filter(show == "The Phantom of the Opera") -> p_of_op

plot2 <- plot +
    geom_point(data = p_of_op, aes(week_ending, avg_ticket_price),
        colour = "red") 

plot2 + theme_bw() +
    labs(x = "",
        y = "Average Ticket Price ($)", 
        title = "Phantom of the Opera (red): relatively cleaper in a more diverse market",
        subtitle = "More expensive some weeks!\nData from Playbill via Tidy Tuesday") 
