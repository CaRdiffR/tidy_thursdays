library(tidyverse)

passwords %>%
    mutate(
        human_duration = case_when(
            time_unit == "seconds" ~ lubridate::duration(seconds = value),
            time_unit == "minutes" ~ lubridate::duration(minutes = value),
            time_unit == "hours" ~ lubridate::duration(hours = value),
            time_unit == "days" ~ lubridate::duration(days = value),
            time_unit == "weeks" ~ lubridate::duration(weeks = value),
            time_unit == "months" ~ lubridate::duration(weeks = value*4.345),
            time_unit == "years" ~ lubridate::duration(weeks = value*52)
            ),
        machine_duration = lubridate::duration(offline_crack_sec)
        ) -> dave_stuff

# https://github.com/tidyverse/ggplot2/issues/2414

dave_stuff %>% 
    select(password, human_duration, machine_duration) %>% 
    gather("type", "time", -password) %>% 
    ggplot(aes(x = password, y = time@.Data, group = type, fill = type)) +
    geom_point()

dave_stuff 
