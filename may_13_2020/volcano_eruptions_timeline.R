## Inspiration from https://twitter.com/ijeamaka_a/status/1260660823229214724/photo/1
## https://github.com/Ijeamakaanyene/data_visualizations/blob/master/scripts/2020_10_volcanos.Rmd

library(dplyr)
library(tidyr)
library(ggplot2)

volcano = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# exploring the data a bit
## check countries with most eruptions...
erupt_by_country = left_join(eruptions, select(volcano, volcano_number, country, subregion),
    by = c("volcano_number"="volcano_number")) %>%
    group_by(country) %>%
    count(sort = TRUE)  
head(erupt_by_country)

## what size eruptions are most common...
count_erupt_size = left_join(eruptions, select(volcano, volcano_number, country, subregion),
    by = c("volcano_number"="volcano_number")) %>%
    group_by(vei) %>%
    count(sort = TRUE)  
## vei not available for lots of eruptions - historical I guess...
count_erupt_size

# create the bound data_set
all_eruptions_merged = left_join(eruptions, select(volcano, volcano_number, 
    country, subregion),
    by = c("volcano_number"="volcano_number")) %>%
    mutate(combo_year = as.numeric(paste(start_year, start_month, sep = ".")))

# allow some customization
## add your country, start year and end year
## and size of eruption from 1 to 7 or zero to see all.
my_country <- "Indonesia"
my_start_year <- 1250
my_end_year <- 2020
size <- 2

# filter merged data set...
eruptions_merged = all_eruptions_merged %>%
    filter(country == my_country) %>%
    filter(is.na(vei) == FALSE) %>%
    filter(start_year > my_start_year) %>%
    filter(start_year < my_end_year) %>%
    filter(vei > size)

# Using for loop to create data needed to plot a geom_polygon
volcano_polygon_list = list()
years = unlist(eruptions_merged$combo_year)
volcano_ids = unlist(eruptions_merged$volcano_number)
veis = unlist(eruptions_merged$vei)

## There is most likely a better way of doing this.. but I cannot think of it!
## so for this to work on a wider than 20 year window we need to 
## change the value for making the pyramid
## 0.25 is approx 1% of the number of decades
pyr_size <- (my_end_year - my_start_year)/100
for(i in 1:length(years)){
    volcano_polygon_df = data.frame(
        x = c(years[i], years[i] + pyr_size, years[i] + pyr_size*2),
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

# create the plot
volcano_timeline = ggplot() +
    geom_polygon(data = volcano_polygon_df, aes(x = x, y = y, group = t, fill = subregion),
        alpha = 0.75, colour = "black") +
    geom_segment(aes(y = 0, yend = 0, x = my_start_year, xend = my_end_year), 
        size = 1,
        colour = "black",
        arrow = arrow()) +
    scale_x_continuous(limits = c(my_start_year, my_end_year),
        expand = c(0.005, 0.005)) +
    scale_y_continuous(expand = c(0, 0))
    

## add titles - based on the values put in...
volcano_timeline <-  volcano_timeline +
    labs(y = "Explosion Index", x = NULL, fill = NULL,
    title = paste0("Volcanic Activity Timeline (vei>", size, ") ",
        my_start_year, " to ", my_end_year, " within ", my_country),
        subtitle = "Each triangle's height represents the erruptions volcanic explosion index.",
        caption = paste0("Source: The Smithsonian Institution\n",
            "Visualization: Paul Brennan | @brennanpcardiff  adapted from Ijeamaka Anyene | @ijeamaka_a"))

## brown colour theme...
volcano_timeline + theme_bw() + 
    ## separate out the colour scale to allow a change to this 
    scale_fill_manual(values = rcartocolor::carto_pal(n = 7, name = "BrwnYl")) 

## Teal Green colour theme...
volcano_timeline + theme_bw() +
    ## separate out the colour scale to allow a change to this 
    scale_fill_manual(values = rcartocolor::carto_pal(n = 7, name = "TealGrn")) 

