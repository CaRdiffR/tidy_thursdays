# playing with data... 
library(tidyverse)

# pull down central billboard data... 
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

# pull down audio_features.csv
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

# find out a bit more...
max(billboard$week_id)
# 2017

min(billboard$week_id)
# 1966

# can I bring together duration - how do long songs do...
# we have spotify_track_duration_ms in audio
# with a song_id

# Q: can we link the song_id from audio and billboard?
# this looks like a join with dplyr

common_song_id <- unique(audio$song_id, billboard$song_id)
# this has 29503 elements - so all in the audio also in the billboard


# what are the most successful songs?
# use weeks_on_chart
# select max weeks_on_chart for each song...

max(billboard$weeks_on_chart)
# 87 weeks...

ggplot(billboard, aes(weeks_on_chart)) + geom_histogram()
# interesting plot

# average
mean(billboard$weeks_on_chart)
# 9

# median
median(billboard$weeks_on_chart)
# 7 

# select max number of weeks for each song...
# https://stackoverflow.com/questions/24558328/select-the-row-with-the-maximum-value-in-each-group
# group %>% group_by(Subject) %>% top_n(1, pt)

billboard %>% 
    group_by(song_id) %>% 
    top_n(1, weeks_on_chart) -> mx_wks_per_song
# 29389 songs
# lost some songs?


ggplot(mx_wks_per_song, aes(weeks_on_chart)) + geom_histogram()
# looks more reasonable than the previous graph...
# average
mean(mx_wks_per_song$weeks_on_chart)
# 11.2

# median
median(mx_wks_per_song$weeks_on_chart)
# 10 

# now join use left_join
# song_id seems to be the key
wks_song_info <- left_join(mx_wks_per_song, 
                           audio, 
                           by = "song_id") #in commas

# now have 29506 obs of 31 variables. 

# plot song length by weeks
ggplot(wks_song_info, aes(weeks_on_chart, 
                          spotify_track_duration_ms)) +
    geom_point(alpha = 0.2) -> p1
p1
# warning for missing values... 
# trend towards longer songs - more weeks on chart?
# a few outliers... 


# add geom_smooth
p1 + geom_smooth()
# looks pretty flat... 

# plot compressed by outliers?
# change with ylim()
p1 + geom_smooth() + ylim(0,1000000)
# still looks pretty flat. 
# no stats here but song lenth doesn't seem to affect 
# weeks on chart... 


# plot weeks on chart by spotify popularity
ggplot(wks_song_info, aes(weeks_on_chart, 
                          spotify_track_popularity)) +
    geom_point(alpha = 0.2) +geom_smooth()

# interesting graph... 
# might be good as a density plot. 
# would be interesting to label a few songs?

# reduce alpha even further... 

# plot weeks on chart by spotify popularity
ggplot(wks_song_info, aes(weeks_on_chart, 
                          spotify_track_popularity)) +
    geom_point(alpha = 0.05) +geom_smooth()

# suggests groups of songs that do well on 
# spotify but don't make the charts 
# and some that do well on both... 

# try adding geom_density
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
ggplot(wks_song_info, aes(weeks_on_chart, 
                          spotify_track_popularity)) +
    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
    scale_fill_gradientn(colours=colfunc(400)) + # gives the colour plot
    geom_density2d(colour="black", bins=5) # draws the lines inside

# this is pretty with three groups seem obvious. 

# 'best' songs probably in top right hand corner. 
# with weeks > 25 or 35 
# and spotify > 60... 

# more analysis possible
