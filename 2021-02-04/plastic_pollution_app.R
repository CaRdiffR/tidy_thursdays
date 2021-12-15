library(shiny)
library(shinycssloaders)
library(plotly)

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

plastics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

countries <- unique(plastics$country)

select_data <- function(country_name, top_n = 10) {
  subset_plast <- plastics %>% 
    filter(country == country_name) %>%
    group_by(parent_company) %>%
    summarise(tot = sum(grand_total)) %>% arrange(desc(tot)) %>%
    filter(!(parent_company %in% c('Grand Total', 'Unbranded', 'null'))) %>%
    head(top_n) %>% 
    ungroup() %>% 
    mutate(parent_company = fct_reorder(parent_company, desc(tot)))
  subset_plast
}

draw_barplot <- function(data){
  #color.function <- colorRampPalette( c( "#CCCCCC" , "#104E8B" ) )
  #color.ramp <- color.function(n = nrow(data))
  ggplot(data, aes(parent_company, tot, fill = parent_company)) +
    geom_bar(stat = "identity") +
    guides(colour=FALSE) +
    theme(axis.text.x = element_text(angle = 30))
}

new_plot <- function(){
  data_2020 <- plastics %>% 
    filter(year==2020) %>% 
    group_by(country) %>% 
    summarise(tot = sum(grand_total)) %>% 
    arrange(desc(tot)) %>% 
    ungroup()
  
  world_map <- map_data("world")
  
  #plastic_map_data <- left_join(data_2020, world_map, by =c("country"= "region"))
  
  plastic_plot <- full_join(data_2020, world_map, by =c("country"= "region")) %>% 
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = tot), color = "white")
  plastic_plot
}

ui <- fluidPage(
  div(
    h1("Plastic pollution"),
    br(),
    selectInput("country", "Select country:",
                countries, selected = "Argentina", width = "50%"),
    sliderInput("nr", "Number of companies:",
                min = 10, max = 20, value = 15),
    align = "center"
  ),
  withSpinner(
    plotlyOutput("barplot", height = "50%")
  ),
  withSpinner(
    plotlyOutput("map", width = "500px")
  ),
  div(
    br(),
    p("Created using data from ",
      tags$a(href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-26", target = "_blank", "TidyTuesday"),
      "with fellows during ",
      tags$a(href = "https://www.meetup.com/Cardiff-R-User-Group", target = "_blank", "Cardiff R enthusiasts"),
      "meet-up."
    ),
    align = "center"
  )
)

server <- function(input, output, session) {
  per_country_data <- reactive({
    select_data(input$country, input$nr)
  })

  output$barplot <- renderPlotly({
    ggplotly(
      draw_barplot(per_country_data())
    )
  })
  
  output$map <- renderPlotly({
    ggplotly(
      new_plot()
    )
  })
  
}

shinyApp(ui, server)
