library(dplyr)
library(magrittr)
library(ggplot2)
library(gganimate)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

scoobydoo$caught_fred <- as.logical(scoobydoo$caught_fred)
scoobydoo$caught_daphnie <- as.logical(scoobydoo$caught_daphnie)
scoobydoo$caught_velma <- as.logical(scoobydoo$caught_velma)
scoobydoo$caught_shaggy <- as.logical(scoobydoo$caught_shaggy)
scoobydoo$caught_scooby <- as.logical(scoobydoo$caught_scooby)

scoobydoo_f <- scoobydoo %>% filter(season %in% c("1", "2", "3", "4"))

scoobydoo %>%
  group_by(series_name) %>%
  summarise(total_fred = sum(caught_fred, na.rm=T),
            total_daphne = sum(caught_daphnie, na.rm=T),
            total_velma = sum(caught_velma, na.rm=T),
            total_shaggy = sum(caught_shaggy, na.rm=T),
            total_scooby = sum(caught_scooby, na.rm=T))

