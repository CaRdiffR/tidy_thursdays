library(tidyverse)
library(lubridate)
library(ggplot2)
library(maps)
library(patchwork)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
#eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
#events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

processed_volcano <- volcano %>%
  mutate(logpop10 = log10(population_within_10_km)) %>%
  select(primary_volcano_type, pop=logpop10, longitude, latitude)

processed_volcano$primary_volcano_type <- 
  recode(processed_volcano$primary_volcano_type, Stratovolcano="Stratovolcano(es)")

processed_volcano$primary_volcano_type <- 
  recode(processed_volcano$primary_volcano_type, `Shield(s)`="Shield")

volcano_types <- processed_volcano %>% group_by(primary_volcano_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% head(8)

processed_volcano$primary_volcano_type <- processed_volcano$primary_volcano_type %>%
  replace_na("Other")

sc_max <- max(processed_volcano$pop)
sc_min <- min(processed_volcano$pop)

col_palette <- c("#d11141", "#00b159", "#00aedb", "#f37735", "#ffc425",
                 "#3aefaf", "#0b57dd", "#f666b7", "#cf1833", "#a35c0a",
                 "#bc0bfe", "#61ff41", "#eabf72", "#09dbc3", "#ffffff")

do_map <- function(vulc_type, colid = NULL) {
  dotcol <- ifelse(is.null(colid), "#fdff00", col_palette[colid])
  processed_volcano %>%
    filter(primary_volcano_type == vulc_type) %>%
    ggplot(aes(longitude, latitude, size = pop)) +
    borders(colour = "#396067", fill = "#383949") +
    geom_point(shape = 16, colour = dotcol, fill = "#ff5800", alpha = 0.6) +
    scale_radius(range = c(1, 8), limits = c(sc_min, sc_max)) +
    ggthemes::theme_map() +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = "#2b272e"),
          plot.title = element_text(color = "white", size = 12, face = "bold")) +
    ggtitle(paste("Volcano type: ", vulc_type))
}

plot.list <- list()
ii <- 1
for (type in volcano_types$primary_volcano_type) {
  plot.list[[type]] <- do_map(type, ii)
  ii <- ii + 1
}

plt <- wrap_plots(plot.list, nrow = 2)
plt
