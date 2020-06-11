library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)

science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
clean_cit <- function(x) gsub("[citation needed]", "", x, fixed = TRUE)

categories <- lapply(
  lapply(
    strsplit(
      tolower(science$occupation_s), ";"),
    trim),
  clean_cit
)

uniq_cats <- data.frame(unlist(categories))
colnames(uniq_cats) <- c("job")
uniq_cats <- uniq_cats %>% group_by(job) %>% tally(sort = T) %>% filter(n >= 3)
filtjobs <- uniq_cats$job
mainjob <- filtjobs[[1]]
filtjobs <- filtjobs[-1]

science$pos_x <- 0
science$pos_y <- 0
science$job   <- "null"

set.seed(0)
allcolours <- 1:(length(filtjobs)+1) %>%
  map(function(x) colours()[sample(1:600)[[1]]])

allcolours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#c1fff5", "#ffa779")

cnt <- length(grep(mainjob, categories))
science[grepl(mainjob, categories), ]$pos_x <- 45*runif(cnt, -1, 1)
science[grepl(mainjob, categories), ]$pos_y <- 45*runif(cnt, -1, 1)
science[grepl(mainjob, categories), ]$job <- mainjob


ii <- 0
step <- 2*pi / length(filtjobs)
r <- 100
for (cat in filtjobs) {
  cnt <- length(grep(cat, categories))
  science[grepl(cat, categories), ]$pos_x <- r*cos(ii*step) + 30*runif(cnt, -1, 1)
  science[grepl(cat, categories), ]$pos_y <- r*sin(ii*step) + 30*runif(cnt, -1, 1)
  science[grepl(cat, categories), ]$job <- cat
  ii <- ii + 1
}

science <- science %>% filter(job != "null")

science$job <- as.factor(science$job)

legend <- data.frame(col = unlist(allcolours),
                     job  = c(mainjob, filtjobs))

p <- ggplot(science, aes(x = pos_x, y = pos_y, color = col, label = name)) +
  geom_text() +
  theme_void() +
  scale_colour_manual(values=unlist(allcolours)) +
  coord_cartesian(clip = "off") +
  theme(plot.background = element_rect(fill = 'grey11', colour = 'grey11'),
        legend.position="none",
        plot.margin = margin(40, 100, 30, 30),) +
  annotate("text", x = -50,
         y = 140,
         label = "African American Scientists",
         size = 5,
         hjust = 0,
         color = "white",
         family = "Andale Mono") +
  annotate("text", x = -50,
           y = -140,
           label = "Data source: https://en.wikipedia.org/wiki/List_of_African-American_inventors_and_scientists",
           size = 3,
           hjust = 0,
           color = "white",
           family = "Andale Mono")

leg_list <- legend %>% transpose() %>% map(function(x) {
  annotate("text", x = -50,
           y = -160,
           label = x$job,
           size = 10,
           hjust = 0,
           color = x$col)
})

