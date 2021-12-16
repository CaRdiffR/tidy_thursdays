library(readr)
# install.packages("syuzhet")
library(syuzhet)
library(stringi)
library(dplyr)
library(ggplot2)
library(plotly)

# Useful tutorial
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html?

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')
related_artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv')

# Cleaning non-ASCII characters (cause problems with text processing packages)

lyrics$song_name <- stringi::stri_trans_general(lyrics$song_name, "latin-ascii")

lyrics$line <- stringi::stri_trans_general(lyrics$line, "latin-ascii")

# Sentiment analysis - positive vs negative

lyrics$sentiment <- get_sentiment(lyrics$line, method = "syuzhet")

songs_sentiment <- lyrics %>%
  group_by(song_id) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  arrange(avg_sentiment)

albums_sentiment <- lyrics %>%
  group_by(album_name) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  arrange(avg_sentiment)

senti_vec <- (lyrics %>% filter(song_name == "Wannabe"))$sentiment
lyrics_vec <- (lyrics %>% filter(song_name == "Wannabe"))$line

dct_values <- get_dct_transform(
  senti_vec,
  low_pass_size = 7,
  x_reverse_len = length(lyrics_vec),
  scale_vals = F,
  scale_range = T
)

plot(
  dct_values,
  type ="l",
  main ="Wannabe sentiment",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  col = "red"
)

fig <- plot_ly(x = 1:length(dct_values), y = dct_values,
               text = lyrics_vec, color = I("blue"),
               type = 'scatter', mode = 'lines+markers')
fig

###### Interactive plot

library(shiny)
library(giphyr)

ui <- fluidPage(
  h1("Spice Girls songs sentiment"),
  selectInput("song", "Select a song",
              choices = unique(lyrics$song_name)),
  checkboxInput("smoother", "Smooth the line?"),
  textOutput("title"),
  plotlyOutput("sentiment_plot")
)

server <- function(input, output, session) {
  output$title <- renderText({
    input$song
  })

  output$sentiment_plot <- renderPlotly({
    song_nm = input$song
    senti_vec <- (lyrics %>% filter(song_name == song_nm))$sentiment
    lyrics_vec <- (lyrics %>% filter(song_name == song_nm))$line

    dct_values <- get_dct_transform(
      senti_vec,
      low_pass_size = 7,
      x_reverse_len = length(lyrics_vec),
      scale_vals = F,
      scale_range = T
    )
    if (input$smoother) {
      yval <- dct_values
      col = "orange"
    } else {
      yval <- senti_vec
      col = "blue"
    }
    plot_ly(x = 1:length(dct_values), y = yval,
            text = lyrics_vec, color = I(col),
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = 'Line number'),
             yaxis = list(title = 'Sentiment'))
  })

}

shinyApp(ui, server)

#####

# Emotions

nrc_data <- get_nrc_sentiment(lyrics$line[1:500])
angry_items <- which(nrc_data$anger > 0)
lyrics$line[1:500][angry_items]

joy_items <- which(nrc_data$joy > 0)
lyrics$line[1:500][joy_items]

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Spice Girls songs", xlab="Percentage"
)
