library(readr)
library(tidyverse)
library(tidytext)
library(ranger)
library(caret)
library(DALEX)
library(breakDown)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')

# Symmarizing all the shows by their weekly gross
grosssumm <- grosses %>%
  group_by(show) %>% summarise(totalGross = sum(weekly_gross))

m <- synopses %>% left_join(grosssumm, by = "show")
m <- m %>% na.omit() %>% mutate(logGross = log10(totalGross))

## Transforming it into classificationproblem
hist(m$logGross)

median_lgross <- quantile(m$logGross, .5)

m <- m %>%
  filter(logGross > quantile(logGross, .6) | logGross < quantile(logGross, .4)) %>%
  mutate(success = logGross > median_lgross) %>%
  select(show, synopsis, success)

nrow(m)

# Preprcessing data

## Creating 1-grams and removing numbers and stop words
data_counts <- map_df(1,
       ~ unnest_tokens(m, word, synopsis, 
                       token = "ngrams", n = .x)) %>%
  filter(!str_detect(word, "[0-9]")) %>%
  anti_join(stop_words, by = "word") %>%
  count(show, word, sort = TRUE)

## Leaving only important words (such that appear at least 5 times in corpus)
words_imp <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 7) %>%
  select(word)

## Creating TF/IDF matrix
data_dtm <- data_counts %>%
  right_join(words_imp, by = "word") %>%
  bind_tf_idf(word, show, n) %>%
  cast_dtm(show, word, tf_idf)

## Making sure if all entries have some words
m <- tibble(show = dimnames(data_dtm)[[1]]) %>%
  left_join(m[!duplicated(m$show), ], by = "show")

# Split into train / test
smp_size <- floor(0.8 * nrow(m))
set.seed(123)
train_ind <- sample(seq_len(nrow(m)), size = smp_size)

x_df_train <- data_dtm[train_ind, ] %>% as.matrix() %>% as.data.frame()
x_df_test <- data_dtm[-train_ind, ] %>% as.matrix() %>% as.data.frame()

y_train <- as.factor(as.numeric(m$success[train_ind]))
y_test  <- as.factor( as.numeric(m$success[-train_ind]))

fit_ctrl <- trainControl(number = 3)

rf_mod <- train(x = x_df_train, 
                y = y_train, 
                method = "ranger",
                trControl = fit_ctrl,
                num.trees = 25,
                importance = "impurity")

pred <- predict(rf_mod,
                newdata = x_df_test)

confusionMatrix(y_test, pred)

## Explaining the model
importance <- varImp(rf_mod, scale = TRUE)
plot(importance, top = 20)

rf_explainer <- DALEX::explain(rf_mod, 
  data = x_df_test, y = as.numeric(y_test),
  label = "RF")

ind_to_check  <- 1
rownames(x_df_test)[[ind_to_check]]

br <- broken(rf_mod, x_df_test[ind_to_check, ])

br <- broken(rf_mod, x_df_test[ind_to_check, ], data = x_df_test)

bd_rf <- variable_attribution(rf_explainer,
                              new_observation = x_df_test[ind_to_check, ],
                              type = "break_down")
plot(bd_rf)

