library(readr)
library(dplyr)

library(ggmap)
library(ggplot2)

library(diagram)
library(plotrix)

library(geosphere)

palette(rainbow(20))

register_google(key = Sys.getenv("GOOGLE_MAP_KEY")) # you need to get it from GCP

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

clean_text <- function(s) {
  s = gsub('[[:punct:]]', '', s)
  s = gsub('[[:cntrl:]]', '', s)
  s = gsub('\\d+', '', s)
  s = gsub("@\\w+ *", "", s)
  s = trimws(s, which = "both")
  s = tolower(s)
  s
}

erasmus$sending_city <- clean_text(erasmus$sending_city)
erasmus$receiving_city <- clean_text(erasmus$receiving_city)

erasmus_move <- erasmus[erasmus$sending_city != erasmus$receiving_city,]

erasmus_move  <- erasmus_move %>%
  filter(nchar(sending_city) > 0 )%>%
  filter(nchar(receiving_city) > 0 )%>%
  mutate(trip = paste(sending_city, receiving_city, sep="_"))

summary_trips <- erasmus_move %>% group_by(trip) %>%
  summarise(count = n())

ggplot(summary_trips, aes(x=count)) + geom_histogram() + scale_x_log10(breaks=c(1,2,3,4,5,10,100, 500))

summary_trips <- summary_trips %>%
  filter(count >= 20) %>%
  mutate(cities = strsplit(trip , "_"))

summary_trips$from <- sapply(summary_trips$cities, function(x) x[[1]])
summary_trips$to <- sapply(summary_trips$cities, function(x) x[[2]])
summary_trips$cities <- NULL

list_cities <- unique(c(summary_trips$from, summary_trips$to))

list_cities_coords <- sapply(list_cities, geocode) # takes long, requires google api

list_cities_coords = as.data.frame(t(list_cities_coords))
list_cities_coords$lon <- unlist(list_cities_coords$lon)
list_cities_coords$lat <- unlist(list_cities_coords$lat)
list_cities_coords <- na.omit(list_cities_coords)

plot_europe <- function() {
  ggplot() +
    borders("world", xlim = c(-10, 50),
            ylim = c(20, 70), size= 0.05,
            fill = "black") + #add coutries borders
    coord_map("gilbert",
              xlim = c(-10, 50), ylim = c(33, 71))
}


paths <- NULL
for(i in 1:nrow(summary_trips)){
  trip <- summary_trips[i,]
  if (all(is.na(list_cities_coords[trip$from,])) ||
      all(is.na(list_cities_coords[trip$to,])) )   # if it didn't find coordinates from or to cities
    next
  if (list_cities_coords[trip$from,]$lon < -10 || list_cities_coords[trip$to,]$lon < -10)
    next
  inter <- gcIntermediate(
    c(list_cities_coords[trip$from,]$lon, list_cities_coords[trip$from,]$lat),
    c(list_cities_coords[trip$to,]$lon, list_cities_coords[trip$to,]$lat),
    n=200
  )
  if (nrow(inter) != 200) next
  inter <- data.frame(inter)
  inter$popularity <- trip$count
  paths <- rbind(paths, inter)
}


paths$popularity <- round(log2(paths$popularity))

p <- plot_europe()

p + geom_path(data = paths, aes(lon,lat, col=popularity)) +
  scale_color_continuous(type="viridis")

paths_f <- paths %>% filter(popularity > 6)

p + geom_path(data = paths_f, aes(lon,lat, col=popularity)) +
  scale_color_continuous(type="viridis")
