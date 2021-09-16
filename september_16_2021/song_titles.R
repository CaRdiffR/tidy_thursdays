library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

library(wordcloud)
library(tm)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

songs <- billboard %>%
  mutate(week_id = as.Date(week_id, format="%m/%d/%Y")) %>%
  group_by(decade = floor_date(week_id, years(x = 10))) %>% 
  summarise(all_titles = paste(song, collapse = " "))

songs$decade <- year(songs$decade)

prep_text_for_plot <- function(df, index) {
  text <- df[index,]$all_titles
  docs <- Corpus(VectorSource(text))
  options(warn=-1)
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  options(warn=0)
  
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
}

plot_wordcloud <- function(df, title="") {
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  wordcloud(words = df$word, freq = df$freq, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.8,
            colors=brewer.pal(8, "Set1"))
}

decade_idx <- 7
df <- prep_text_for_plot(songs, decade_idx)
plot_wordcloud(df, paste("Year:", songs[decade_idx,]$decade))
