library(tidyverse)
library(ggplot2)
library("usmap")
library(shiny)
library(shinycssloaders)

#brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
#beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
#brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

ui <- fluidPage(
  div(
    h1("USA beer production per state"),
    br(),
    selectInput("year", "Year:", unique(beer_states$year)),
    align = "center"
  ),
  withSpinner(
    plotOutput("beerMap", height = "600px")
  )
)

server <- function(input, output) {
  
  data <- reactive({
    beer_states %>% filter(year == input$year) %>% mutate(logbar = log10(barrels))
  })

  output$beerMap <- renderPlot({
    plot_usmap(data = data(),
               values = "barrels", color = "black") + 
      scale_fill_continuous(name = "Barrels", label = scales::comma, low = "blue", high = "orange") + 
      theme(legend.position = "right", plot.title = element_text(size = 25, face = "bold", hjust = 0.5)) +
      ggtitle(input$year)
  })
}

shinyApp(ui, server)