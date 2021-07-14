library(dplyr)
library(magrittr)
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
    summarise(total_fred = sum(caught_fred, na.rm=T),
              total_daphne = sum(caught_daphnie, na.rm=T),
              total_velma = sum(caught_velma, na.rm=T),
              total_shaggy = sum(caught_shaggy, na.rm=T),
              total_scooby = sum(caught_scooby, na.rm=T))


ggplot(custom_dt, aes( x = REACTION, y = COUNT, label = NAME)) +
  geom_image(aes(image = IMAGE), size = 0.04) +
  geom_text_repel(point.padding = 0.9, segment.alpha = 0) +
  xlab("as reaction") +
  ylab("within message") +
  theme_minimal()


data <- data.frame(
  name=c("A","B","C","D","E") ,  
  value=c(3,12,5,18,45)
)

ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

caught_df <- subset(caught_df, select = grep("total_*", names(caught_df)))

vec <- as.data.frame(t(caught_df[2,]))
names(vec) <- "count" 
catch_single <- tibble::rownames_to_column(vec, "who")
catch_single <- catch_single %>%
  mutate(who = str_replace(who, "total_", "")) %>%
  mutate(im_path = glue("images/{who}.png"))


ggplot(catch_single, aes(x=who, y=count, fill=who)) + 
  geom_bar(stat = "identity") +
  geom_image(aes(image = im_path), size = 0.1) +
  coord_flip() +
  theme(legend.position="none")
