library(tidytuesdayR)
library(tidyverse)

tt_data <- tt_load("2020-01-14")

tt_data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

tt_data$passwords %>% view()

passwords %>% 
  mutate(cat= as.factor(category)) %>% 
  mutate(cat= fct_infreq(cat)) %>% 
  ggplot(aes(cat)) + 
  geom_bar()

passwords %>% 
  count(time_unit)

lubridate::duration()


password_tidy <- passwords %>% 
  drop_na() %>% 
  mutate(length= nchar(password)) %>% 
  mutate( 
    time_unit_sec= case_when(
      time_unit == "days" ~ 24*60*60*value,
      time_unit =="hours" ~ 60*60* value, 
      time_unit =="minutes" ~ 60* value, 
      time_unit =="months" ~ 30*24*60*60* value,
      time_unit =="seconds" ~ value, 
      time_unit =="weeks" ~ 7*24*60*60*value, 
      time_unit =="years" ~ 12*30*24*60*60* value
        )
  )

passwords %>% colnames()
password_tidy %>%
  ggplot(aes(x=time_unit_sec, y= offline_crack_sec))+
  geom_point()+
  scale_x_continuous(trans="log")+
  scale_y_continuous(trans="log")+
  NULL
  
cor(password_tidy$time_unit_sec, password_tidy$offline_crack_sec)

cor(password_tidy$strength, password_tidy$offline_crack_sec)
cor(password_tidy$strength, password_tidy$time_unit_sec)

cor(password_tidy$strength, password_tidy$length)
cor(password_tidy$offline_crack_sec, password_tidy$length)


password_tidy %>%
  ggplot(aes(x=strength, y= time_unit_sec))+
  geom_point()+
  # scale_x_continuous(trans="log")+
  scale_y_continuous(trans="log")+
  geom_smooth()+
  NULL

password_tidy[c(406,500,336),]

password_tidy %>% 
  arrange(desc(offline_crack_sec)) %>% 
  head()


password_tidy %>% 
  arrange(desc(length)) %>% 
  head()

password_tidy %>% 
  count(strength) %>% 
  ggplot(aes(strength,n))+
  geom_col()


lm1 <- password_tidy %>% 
  lm(log(offline_crack_sec)~strength+length+category, data=.)

summary(lm1)
plot(lm1)


