library(readr)
library(tidyverse)
library(tidytext)
library(ranger)
library(caret)
library(DALEX)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')

# Symmarizing all the shows by their weekly gross
grosssumm <- grosses %>%
  group_by(show) %>% summarise(totalGross = sum(weekly_gross))

m <- synopses %>% left_join(grosssumm, by = "show")
m <- m %>% na.omit()

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
  filter(n >= 5) %>%
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

y_train <- m$totalGross[train_ind]
y_test  <- m$totalGross[-train_ind]

hist(y_train)

## Variance of gross is high, so better to predict log
y_train_log <- log10(y_train)
y_test_log  <- log10(y_test)

hist(y_train_log)

fit_ctrl <- trainControl(number = 3)

rf_mod <- train(x = x_df_train, 
                y = y_train, 
                method = "ranger",
                trControl = fit_ctrl,
                num.trees = 100,
                importance = "impurity")

pred <- predict(rf_mod,
                newdata = x_df_test)

plot(y_test, pred)
plot(log10(y_test), log10(pred))

## Explaining the model
importance <- varImp(rf_mod, scale = TRUE)
plot(importance, top = 20)

rf_explainer <- DALEX::explain(rf_mod$finalModel, 
  data = x_df_test, y = y_test_log,
  label = "RF")

plot(model_performance(rf_explainer))

plot(model_diagnostics(rf_explainer))


ind_to_check  <- 187
rownames(x_df_test)[[ind_to_check]]
bd_rf1 <- variable_attribution(rf_explainer,
                              new_observation = x_df_test[ind_to_check, ],
                              type = "break_down")
plot(bd_rf1) + ggtitle("American Psycho")

ind_to_check  <- 130
rownames(x_df_test)[[ind_to_check]]
bd_rf2 <- variable_attribution(rf_explainer,
                              new_observation = x_df_test[ind_to_check, ],
                              type = "break_down")
plot(bd_rf2)
