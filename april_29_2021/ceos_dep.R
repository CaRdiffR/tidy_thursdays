library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# companies with high number of departures

top_depts <- departures %>%
  count(coname, sort = TRUE) %>%
  top_n(10)

top_depts$coname <- factor(top_depts$coname, levels = top_depts$coname)

ggplot(top_depts, aes(coname, n, fill=n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

# types of departure a year

dep_type_by_year <-departures %>%
  group_by(departure_code, fyear_gone) %>%
  summarise(n = n()) %>%
  drop_na() %>%
  filter(fyear_gone < 2021)

dep_type <- c("Involuntary - CEO death",
"Involuntary - CEO illness",
"Involuntary – CEO dismissed for job performance",
"Involuntary - CEO dismissed for legal violations or concerns",
"Voluntary - CEO retired",
"Voluntary - new opportunity (new career driven succession)",
"Other",
"Missing",
"Execucomp error")

dep_type_by_year_mod <- dep_type_by_year
dep_type_by_year_mod$departure_code <- cut(dep_type_by_year$departure_code, breaks = 0:9, labels = dep_type)

ggplot(dep_type_by_year_mod, aes(fyear_gone, n, group=departure_code)) +
  geom_line(aes(color=departure_code))+
  geom_point(aes(color=departure_code)) +
  scale_y_continuous(trans = 'log10') +
  xlab("Year of CEO dismissal") +
  ylab("Log Count")

# The same but simplified

dep_type_by_year_sim <- dep_type_by_year
dep_type_by_year_sim$departure_code <- cut(dep_type_by_year_sim$departure_code, c(0,5,7), c("Involuntary", "Voluntary"))
dep_type_by_year_sim <- dep_type_by_year_sim %>% drop_na()

dep_type_by_year_sim <-dep_type_by_year_sim %>%
  group_by(fyear_gone, departure_code) %>%
  summarise(s = sum(n))

ggplot(dep_type_by_year_sim, aes(fyear_gone, s, group=departure_code)) +
  geom_line(aes(color=departure_code))+
  geom_point(aes(color=departure_code)) +
  xlab("Year of CEO dismissal") +
  ylab("Count")

# Most common sources

## https://regex101.com/
## https://github.com/VerbalExpressions/RVerbalExpressions

sources <- lapply(strsplit(departures$sources, ";"), function(x) as.data.frame(URL_parts(x))$host)
sources <- unlist(sources)

sources_count <- as.data.frame(table(sources)) %>% arrange(desc(Freq))

head(sources_count,100)



