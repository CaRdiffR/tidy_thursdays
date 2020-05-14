# Inspiration from https://twitter.com/ijeamaka_a/status/1260660823229214724/photo/1
# https://github.com/Ijeamakaanyene/data_visualizations/blob/master/scripts/2020_10_volcanos.Rmd

library(dplyr)
library(tidyr)
library(ggplot2)

volcano = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# check eruptions for busiest countries for eruptions...

erupt_by_country = left_join(eruptions, select(volcano, volcano_number, country, subregion),
    by = c("volcano_number"="volcano_number")) %>%
    group_by(country) %>%
    count(sort = TRUE)  

head(erupt_by_country)

my_country <- "Indonesia"
my_start_year <- 1990
my_end_year <- 2000

eruptions_merged = left_join(eruptions, select(volcano, volcano_number, country, subregion),
    by = c("volcano_number"="volcano_number")) %>%
    filter(country == my_country) %>%
    filter(is.na(vei) == FALSE) %>%
    filter(start_year > my_start_year) %>%
    filter(start_year < my_end_year) %>%
    mutate(combo_year = as.numeric(paste(start_year, start_month, sep = ".")))

# Using for loop to create data needed to plot a geom_polygon
volcano_polygon_list = list()
years = unlist(eruptions_merged$combo_year)
volcano_ids = unlist(eruptions_merged$volcano_number)
veis = unlist(eruptions_merged$vei)
# There is most likely a better way of doing this.. but I cannot think of it!
for(i in 1:length(years)){
    volcano_polygon_df = data.frame(
        x = c(years[i], years[i] + .25, years[i] + .5),
        y = c(0, veis[i], 0),
        t = rep(volcano_ids[i], 3)
    )
    
    volcano_polygon_list[[i]] = volcano_polygon_df
}

# Converting into df and adding subregion information
volcano_polygon_df = volcano_polygon_list %>%
    bind_rows() %>%
    left_join(., select(eruptions_merged, volcano_number, subregion),
        by = c("t" = "volcano_number"))

volcano_timeline = ggplot() +
    geom_polygon(data = volcano_polygon_df, aes(x = x, y = y, group = t, fill = subregion),
        alpha = 0.75, colour = "black") +
    geom_segment(aes(y = 0, yend = 0, x = my_start_year, xend = my_end_year), 
        size = 1,
        colour = "black",
        arrow = arrow()) +
    scale_x_continuous(limits = c(my_start_year, my_end_year),
        expand = c(0.005, 0.005)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = rcartocolor::carto_pal(n = 7, name = "BrwnYl")) 

volcano_timeline <-  volcano_timeline +
    labs(y = NULL, x = NULL, fill = NULL,
    title = paste("Volcanic Activity Timeline", my_start_year, "to", my_end_year, "within", my_country),
subtitle = "Each triangle's height represents the erruptions volcanic explosion index.",
caption = paste0("Source: The Smithsonian Institution\n",
    "Visualization: adapted from Ijeamaka Anyene | @ijeamaka_a"))

    
volcano_timeline + theme_bw() 
