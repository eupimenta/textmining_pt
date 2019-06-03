library(shiny)
library(highcharter)

db_miss
hc_base <- highchart() %>% 
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%"
    ),
    column = list(
      colorByPoint = TRUE
    ),
    pie = list(
      colorByPoint = TRUE, center = c('30%', '10%'),
      size = 120, dataLabels = list(enabled = FALSE)
    )) %>%
  
  hc_xAxis(categories = db_miss$variables) %>% 
  hc_add_series(name = "Missing", data = (100*db_miss$percent_missing)) 

ui <- fluidPage(
  h2("Viewer"),
  fluidRow(
    h3(""), highchartOutput("hc_1", width = "100%", height = "800px"),
    h3("Click"), verbatimTextOutput("hc_1_input2")
  )
)
server = function(input, output) {
  output$hc_1 <- renderHighchart({
    hc_base %>% 
      hc_add_theme(hc_theme_ffx())%>%
      hc_tooltip(backgroundColor="skyblue",crosshairs = TRUE, borderWidth = 5, valueDecimals=2)
  })
  
  observeEvent(input$hc_1_click,{
    output$hc_1 <- renderHighchart({
      hc_base %>% 
        hc_add_theme(hc_theme_ffx())%>%
        hc_tooltip(backgroundColor="skyblue",crosshairs = TRUE, borderWidth = 5, valueDecimals=2)%>%
        hc_add_series_scatter(cars$speed, cars$dist)
    })
    
  })
  
  output$hc_1_input2 <- renderPrint({input$hc_1_click })
}
shinyApp(ui = ui, server = server)