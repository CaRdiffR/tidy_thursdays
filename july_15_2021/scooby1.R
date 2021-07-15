library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(glue)
library(ggplot2)
library(ggimage)
library(gganimate)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

scoobydoo$caught_fred <- as.logical(scoobydoo$caught_fred)
scoobydoo$caught_daphnie <- as.logical(scoobydoo$caught_daphnie)
scoobydoo$caught_velma <- as.logical(scoobydoo$caught_velma)
scoobydoo$caught_shaggy <- as.logical(scoobydoo$caught_shaggy)
scoobydoo$caught_scooby <- as.logical(scoobydoo$caught_scooby)

caught_df <- scoobydoo %>%
    group_by(series_name) %>%
    summarise(total_Fred = sum(caught_fred, na.rm=T),
              total_Daphnie = sum(caught_daphnie, na.rm=T),
              total_Velma = sum(caught_velma, na.rm=T),
              total_Shaggy = sum(caught_shaggy, na.rm=T),
              total_Scooby = sum(caught_scooby, na.rm=T),
              date = min(date_aired))

# only series with representative numbers
caught_df <- caught_df[which(rowSums(caught_df[,2:6])>10), ]

# check if there's no overlapping years
sort(unique(caught_df$date))

caught_longer <- caught_df %>%
      pivot_longer(cols = starts_with("total_"),
                   names_to = "who",
                   values_to = "count") %>%
      mutate(who = str_replace(who, "total_", "")) %>%
      mutate(year = as.numeric(str_extract(date,"[0-9]{4}"))) %>%
      mutate(series_year = paste(year, "-", series_name)) %>%
      mutate(im_path = glue("images/{tolower(who)}.png")) %>%
      select(!date)

scooby_colours <- unlist(list(
  Scooby = "#a78036",
  Daphnie = "#ff77f9",
  Velma = "#ff9a00",
  Shaggy = "#00e76d",
  Fred = "#009ce7"
))

# first version
p <- ggplot(caught_longer, aes(x=who, y=count, fill = who)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image = im_path), size = 0.1) +
  scale_fill_manual(values = scooby_colours) +
  theme(legend.position = 'none') +
  theme(text=element_text(size=15,  family="Comic Sans MS")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, colour = "blue"),
        axis.title = element_text(size=18)) +
  coord_flip() +
  labs(title = '{closest_state}', x = '', y = 'How many monsters caught?') +
  transition_states(series_year) +
  ease_aes('linear')

# B&W version
p <- ggplot(caught_longer, aes(x=who, y=count, fill = who)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image = im_path), size = 0.1) +
  scale_fill_manual(values = scooby_colours) +
  theme_dark() +
  theme(legend.position = 'none') +
  theme(text=element_text(size=15,  family="Comic Sans MS")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, colour = "white"),
        axis.title = element_text(size=18, colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.background = element_rect(fill = "#2b272e")) +
  coord_flip() +
  labs(title = '{closest_state}', x = '', y = 'How many monsters caught?') +
  transition_states(series_year) +
  ease_aes('linear')

animate(p, 260, 20)
anim_save("scooby_gif.gif")

