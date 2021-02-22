library(shiny)
library(ggplot2)
library(dplyr)
library(fivethirtyeight)
library(plotly)
data(biopics)
categoricalVars <-  c("country", "type_of_subject", "subject_race", "subject_sex") 

ui <- fluidPage(
  plotlyOutput("movie_plot")
)

server <- function(input, output) {
  output$movie_plot <- renderPlotly({
    my_plot <- ggplot(biopics) +
      aes_string(x = "year_release",
                 y = "box_office",
                 color = "type_of_subject",
                 country = "country",
                 director = "director") +
      geom_point() +
      theme(legend.position = "none")
    ggplotly(my_plot, tooltip = c("director", "country"))
  })
}

shinyApp(ui = ui, server = server)
