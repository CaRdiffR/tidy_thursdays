library(tidyverse)

# import the data...
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# 9423 obs of 19 variables

# company names = coname 

length(unique(departures$coname))
# 3860 different names

# 9 different kinds of departure 1 to 9... 

# try plotting the numbers in each 
departures %>% 
    group_by(departure_code) %>%
    tally() %>%
    ggplot(aes(departure_code, n)) + geom_bar(stat = "identity")


# try mutate into a factor in departures... 
departures %>% 
    mutate(departure_code, as.factor(departure_code)) %>%
    group_by(departure_code) %>%
    tally() %>%
    ggplot(aes(departure_code, n)) + geom_bar(stat = "identity")
# this doesn't work...

# try mutate after group by.... 
departures %>% 
    group_by(departure_code) %>%
    tally() %>%
    mutate(departure_code, as.factor(departure_code)) %>%
    ggplot(aes(departure_code, n)) + geom_bar(stat = "identity")
# this still doesn't work...

# this is where I feel like a part timer and get a bit dispirited...

# create a separate object... 
departures %>% 
    group_by(departure_code) %>%
    tally() -> cnt_dep

# make factor in the plot...
# don't like changing the original data...
ggplot(cnt_dep, aes(as.factor(departure_code), n)) + geom_bar(stat = "identity")

# 3, 5 and 7 are the main reasons for departure
# 3 = Involuntary – CEO dismissed for job performance
# 5 = Voluntary - CEO retired
# 7 = Other

# generate a word cloud of how # 3 is managed... 

# filter departure_code = 3

departures %>%
    filter(departure_code == 3) %>%  # 1320 rows...
    select(notes) -> text_dep

# https://rforbiochemists.blogspot.com/2015/10/what-do-i-tweet-about-not-just-r.html
library("wordcloud")
library("tm")


# convert into a Corpus - a structure for organising text...
text <- Corpus(VectorSource(text_dep))
text <- tm_map(text, content_transformer(tolower))
# remove stopwords
text <- tm_map(text, removeWords, stopwords("english"))
wordcloud(text, max.words = 35, colors=brewer.pal(8, "Dark2"))

