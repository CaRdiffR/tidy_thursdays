library(readr)
library(tidyverse)
library(tm)
library(wordcloud2)
library(RColorBrewer)

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


movies[movies$binary == "FAIL",]

movies_fail <- movies %>% filter(binary == "FAIL") %>% filter(!is.na(plot))
movies_pass <- movies %>% filter(binary == "PASS") %>% filter(!is.na(plot))

#' Get word count from text vector
#'
#' @param text_data vector with textual data
#'
#' @return data.frame with word counts
get_word_count <- function(text_data) {
  docs <- Corpus(VectorSource(text_data))
  
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  df  
}

wc_fail <- get_word_count(movies_fail$plot)
wc_pass <- get_word_count(movies_pass$plot)

wordcloud2(data=wc_fail, size = 0.7, shape = 'star')

wordcloud2(data=wc_pass, size = 0.7, shape = 'star')

