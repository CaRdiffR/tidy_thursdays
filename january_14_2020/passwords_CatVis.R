library(dplyr)
category_counts<-passwords %>%
  filter(!is.na(category))%>%
  count(category) %>%
  arrange(n) %>%
  mutate(category = factor(category, levels = category))

passwords %>%
  filter(category == "nerdy-pop")
library(ggplot2)

#plotting
ggplot(category_counts, aes(x=category, y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))
  
  
  