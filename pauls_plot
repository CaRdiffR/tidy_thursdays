# is there a blog post to put here...

library(tidyverse)
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

passwords <- passwords[1:500,]

# frequency of passwords
# interesting plot about the frequency of passwords...
ggplot(passwords, aes(str_length(password))) + geom_histogram()


# plot rank v strength
p <- ggplot(passwords, aes(rank, strength, colour = factor(category))) +
    geom_point()

p

p + facet_wrap(~category) + theme(legend.position = "none")

# pays be nerdy and numeric...

# numbers are probably the key...
# want to see if they have numbers
# colour these by number
passwords$digit <- str_detect(passwords$password, "[1234567890]")

# using mutate is the 'tidy' way. Uses NSE to evalute the as if equation
passwords %>% 
    mutate(  # make a new column
        digits = str_detect(passwords$password, "[1234567890]"), # comma here allows another column
        tmp = "tmp"  # fill the column with this string...
        ) -> passwords_2


p1 <- ggplot(passwords, aes(rank, strength, colour = digit)) +
    geom_point()

p1 + facet_wrap(~category) 

# look at one category - pays to be a nerd? 
passwords %>%
    filter(category == "nerdy-pop") %>%
    ggplot(aes(rank, strength, colour = digit)) + geom_point()


# what are the better passwords
# filter on nerdy-pop, simple-alphanumeric, sport and password-related
good_words_in <- c("nerdy-pop", "simple-alphanumeric", "sport", "password-related")

passwords %>%
    filter(category %in% good_words_in) %>%  # the %in% is important!!  
    ggplot(aes(rank, strength, colour = digit)) + 
    geom_point() + facet_wrap(~category)


# https://stackoverflow.com/questions/15624656/label-points-in-geom-point
passwords %>%
    filter(category %in% good_words_in) %>%  # the %in% is important!!  
    ggplot(aes(rank, strength, colour = digit)) + 
    geom_point() +
    geom_text(aes(label=ifelse(strength>20,as.character(password),''),
        vjust=1)) +
    facet_wrap(~category)
# works but labels are not the best :-(

# try ggrepel
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
library(ggrepel)

passwords %>%
    filter(category %in% good_words_in) %>%  # the %in% is important!!  
    ggplot(aes(rank, strength, colour = digit, label = password)) + 
    geom_point() + geom_text_repel()
# this works but is a mess!!!!
# need to remove labels for strength < 20. 

# create a data set with strength greater than 20
passwords %>%
    filter(strength >20) -> better_passwords

# overlay these labels with a different dataset...
passwords %>%
    filter(category %in% good_words_in) %>%  # the %in% is important!!  
    ggplot(aes(rank, strength, colour = digit)) + 
    geom_point() -> graphs

graphs
graphs + facet_wrap(~category)

g_label <- graphs + geom_text_repel(data = better_passwords, 
    aes(rank, strength, label=password))
# works nicely 
g_label + facet_wrap(~category) 
# works nicely
link <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md"
# add some styling...
g_label + facet_wrap(~category) + 
    labs(x = "Password Rank",
        y = "Password Strength", 
        title = "Exploring Passwords for Tidy Tuesday",
        subtitle = link) +
    theme_bw()

# this is my final plot, I think!!
