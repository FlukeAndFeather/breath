library(breath)
library(shiny)
library(tidyverse)

ui <- fillPage(
  plotOutput("all_plot", height = "20%"),
  plotOutput("main_plot", height = "80%")
)

server <- function(input, output, session) {
  data <- data.frame(x = 1:1000, y = cumsum(runif(1000)))
  output$profile_row <- renderPlot(plot(y ~ x, data, type = "l"))
  output$main_row <- renderPlot(plot(y ~ x, data[1:100, ], type = "l"))
}

shinyApp(ui, server)


ui <- fillPage(
  plotOutput("plot", height = "20%"),
  plotOutput("plot2", height = "80%")
)
server <- function(input, output, session) {
  data <- data.frame(x = 1:1000, y = cumsum(rnorm(1000)))
  output$plot <- renderPlot(ggplot(data, aes(x, y)) + geom_line())
  output$plot2 <- renderPlot(ggplot(slice(data, 1:100), aes(x, y)) + geom_line())
}
shinyApp(ui, server)
