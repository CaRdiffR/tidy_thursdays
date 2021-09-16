library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

audio_features <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

billboard$week_id <- as.Date(billboard$week_id, format="%m/%d/%Y")

bsongperf <- billboard %>%
  group_by(song_id) %>% slice_min(peak_position, with_ties = FALSE)

system.time(
  x1 <- bsongperf %>%
    filter(song_id %in% intersect(bsongperf$song_id, audio_features$song_id))
)

system.time(
  x2 <- bsongperf[bsongperf$song_id %in% intersect(bsongperf$song_id, audio_features$song_id),]
)

all(x1==x2, na.rm=T)

bsongperf <- x2

system.time(
  audio_features <-
    audio_features[audio_features$song_id %in% intersect(bsongperf$song_id, audio_features$song_id),]
)

audio_features <-audio_features[!duplicated(audio_features$song_id),]

top_song_features <- bsongperf %>%
  left_join(audio_features, by = "song_id")

features_by_year <- top_song_features %>%
  group_by(year=floor_date(week_id, "year")) %>%
  summarize(
    danceability=mean(danceability, na.rm=TRUE),
    energy=mean(energy, na.rm=TRUE),
    loudness=mean(loudness, na.rm=TRUE),
    liveness=mean(liveness, na.rm=TRUE),
    valence=mean(valence, na.rm=TRUE),
    tempo=mean(tempo, na.rm=TRUE)
  ) %>%
  pivot_longer(danceability:tempo, names_to="feature")

glimpse(features_by_year)

ggplot(data = features_by_year, aes(year, value)) +
  geom_line(aes(color = feature), size = 1) +
  geom_point(aes(color = feature)) + 
  labs(title = "TOP 100 BILLBOARD SONG FEATURES",
       y = "", x = "year") + 
  facet_wrap(. ~ feature, nrow=2, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")
