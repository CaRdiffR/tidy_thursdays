library(readr)
# install.packages("syuzhet")
library(syuzhet)
library(stringi)
library(dplyr)
library(ggplot2)
library(plotly)

# Useful tutorial
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html?

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')
related_artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv')

lyrics$line <- stringi::stri_trans_general(lyrics$line, "latin-ascii")

lyrics$sentiment <- get_sentiment(lyrics$line, method = "syuzhet")

songs_sentiment <- lyrics %>%
  group_by(song_id) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  arrange(avg_sentiment)

albums_sentiment <- lyrics %>%
  group_by(album_name) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  arrange(avg_sentiment)

senti_vec <- (lyrics %>% filter(song_name == "Wannabe"))$sentiment

dct_values <- get_dct_transform(
  senti_vec,
  low_pass_size = 5,
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)

plot(
  dct_values,
  type ="l",
  main ="Wannabe sentiment",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  col = "red"
)

nrc_data <- get_nrc_sentiment(lyrics$line[1:500])
angry_items <- which(nrc_data$anger > 0)
lyrics$line[1:500][angry_items]

joy_items <- which(nrc_data$joy > 0)
lyrics$line[1:500][joy_items]

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Spice Girls songs", xlab="Percentage"
)
