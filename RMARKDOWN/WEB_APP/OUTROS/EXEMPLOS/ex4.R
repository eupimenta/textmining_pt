#setwd("/Users/ewersonpimenta/Desktop/")
#write.csv(cars, file = "cars.csv", row.names = FALSE)

library(shiny)
library(dplyr)

ui <- fluidPage(
  fileInput("csvFile", "Drag cars.csv over here!"),
  tableOutput("rawData"),
  tableOutput("modifiedData")
)

server <- function(input, output, session) {
  rawData <- eventReactive(input$csvFile, {
    read.csv(input$csvFile$datapath)
  })
  
  output$rawData <- renderTable({
    rawData() %>% head
  })
  
  output$modifiedData <- renderTable({
    rawData() %>% mutate(sum = speed + dist) %>% head
  })
}

shinyApp(ui, server)
