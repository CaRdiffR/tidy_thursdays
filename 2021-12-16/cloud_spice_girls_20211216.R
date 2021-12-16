# spice_girl Tidy Tuesday play... 

install.packages("tidytuesdayR")
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2021-12-14')
# creates a list of 3...

lyr <- tuesdata$lyrics
# lyrics, line by line, with the artist that sung them.... 
# section artist is a bit untidy... 

art <- tuesdata$related_artists
# not quite sure what this represents. 
tracks <- tuesdata$studio_album_tracks
# three studio albums - 31 songs... 


# nice easy place to start is word clouds... 
library("wordcloud")
library("tm")
library("ggplot2")

# all lyrics
all_lyr <- as.character(lyr$line)
wordcloud(all_lyr, max.words = 35)
# doesn't work...

# convert into a Corpus - a structure for organising text...
all_lyr_c <- Corpus(VectorSource(all_lyr))

all_lyr_c_p <- tm_map(all_lyr_c, content_transformer(tolower))
# gives an error message because of "invalid input"

# check on Google:
# https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
all_lyr_use <- stringr::str_replace_all(all_lyr,"[^[:graph:]]", " ") 

# try again to convert...
all_lyr_c <- Corpus(VectorSource(all_lyr_use))

all_lyr_c_p <- tm_map(all_lyr_c, content_transformer(tolower))


# remove stopwords
all_lyr_c_p <- tm_map(all_lyr_c_p, removeWords, stopwords("english"))
# get a list of Englisth stopwords....
stopwords(kind = "en")
length(stopwords(kind = "en"))

wordcloud(all_lyr_c_p, max.words = 35, colors=brewer.pal(8, "Dark2"))

## this works... 

# create a function 
make_cloud <- function(text, mx_words = 35){
    text <- as.character(text)
    # eliminate any graphical elements
    text <- stringr::str_replace_all(text,"[^[:graph:]]", " ") 
    # try again to convert...
    text <- Corpus(VectorSource(text))
    text <- tm_map(text, content_transformer(tolower))
    # remove stopwords
    text <- tm_map(text, removeWords, stopwords("english"))
    wordcloud(all_lyr_c_p, max.words = mx_words, colors=brewer.pal(8, "Dark2"))
    return(text)
}

make_cloud(lyr$line)
# this works... 

library(tidyverse)

# OK, so just lines that they sing by themsleves...
# remove anything with a comma or ampersand (i.e. more than one singer)
lyr %>%
    filter(str_detect(lyr$section_artist, ",") == FALSE) -> lyr_solo
lyr_solo %>%
    filter(str_detect(lyr_solo$section_artist, "&") == FALSE) -> lyr_solo
lyr_solo %>%
    filter(str_detect(lyr_solo$section_artist, "with") == FALSE) -> lyr_solo
lyr_solo %>%
    filter(str_detect(lyr_solo$section_artist, "All") == FALSE) -> lyr_solo
lyr_solo %>%
    filter(str_detect(lyr_solo$section_artist, "Spice Girls") == FALSE) -> lyr_solo
# 298 obs left... 

# who had the most solo lines?
# count number of solo lines...
lyr_solo %>%
    group_by(section_artist) %>%
    summarise(n = n())
# Baby Spice has most lines. Posh the least even adding "Victoria"
ggplot(lyr_solo, aes(section_artist)) + geom_bar()

# word cloud of Baby
lyr_solo %>%
    filter(section_artist == "Baby")  %>% make_cloud(mx_words = 10)

# word cloud of Scary
lyr_solo %>%
    filter(section_artist == "Scary")  %>% make_cloud(mx_words = 10)

# word cloud of Ginger
lyr_solo %>%
    filter(section_artist == "Ginger")  %>% make_cloud(mx_words = 10)

# word cloud of Sporty
lyr_solo %>%
    filter(section_artist == "Sporty")  %>% make_cloud(mx_words = 10)

# looks like these are the same for each. 
# I guess reflects a chorus or alternating lines... 

