library(shiny)
library(tidyverse)
library(RColorBrewer)
library(shinycssloaders)
library(wordcloud)
library(tm)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
passwords <- drop_na(passwords)
passwords <- passwords %>% mutate(logstrength = log(strength))
pal <- brewer.pal(9,"Dark2")

ui <- fluidPage(
  div(
    h1("Passwords wordcloud"),
    br(),
    sliderInput("strength", "Password strength from low to strong:",
                min = 0, max = 50, value = c(5, 10), step = 1, width="600px"),
    align = "center"
  ),
  withSpinner(
    plotOutput("wordCloud", height = "600px")
  )
)

server <- function(input, output) {
  data <- reactive({
    pass <- passwords %>% filter(strength >= input$strength[[1]] &
                                   strength <= input$strength[[2]])
    validate(
      need(nrow(pass) > 0, "Empty range. Please change your selection.")
    )
    pass$password
  })
  output$wordCloud <- renderPlot({
    wordcloud::wordcloud(data(), random.color=T, random.order=T, rot.per=0.2,
                         scale=c(6,1), colors=pal)
  })
}

shinyApp(ui, server)
