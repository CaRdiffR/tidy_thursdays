library(tidyverse)
library(lubridate)
library(ggplot2)
library(gifski)
library(gganimate)
library(maps)
library(patchwork)

# inspired by @alexcookson viz:
# https://github.com/tacookson/tidy-tuesday/blob/master/volcano-eruptions.Rmd

#volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

preproc_erupt <- eruptions %>%
  filter(start_year >= 1800,
         eruption_category != "Discredited Eruption",
         !is.na(vei)) %>%
  filter(latitude > 35, latitude < 48,
         longitude > 6, longitude < 19) %>%
  select(latitude, longitude, volcano_name, start_year, vei)

#   filter(latitude > 52.44, latitude < 54.06,
#longitude > -5.19, longitude < -4.10)

p <- preproc_erupt %>%
  ggplot(aes(longitude, latitude, size = vei, group = start_year)) +
  borders(database = "world", regions = c("Italy"), colour = "#396067", fill = "#383949") +
  geom_point(colour = "yellow",
             shape = 16,
             alpha = 0.3) +
  scale_radius(range = c(1, 8)) +
  transition_time(start_year, range = c(1800, 2020)) +
  ggthemes::theme_map() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#2b272e"),
        plot.title = element_text(color = "white", size = 20, face = "bold")) +
  ggtitle("Year: {floor(frame_time)}") +
  enter_grow()

anim_save("italy-volcano-eruptions.gif", p)

