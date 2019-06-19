library(shiny)
library(networkD3)
library(DT)
library(sqldf)

df <- read.csv(header = T, text = '
source,name,age,hair
dad,Jon X,18,brown
dad,Jon Y,22,blonde
')

funct <-
  function (n) {
    isp <- sprintf("Select df.age From df Where df.name='%s';", n)
    isd <- sqldf::sqldf(isp)
    return(isd)
  }

ui <- shinyUI(fluidPage(
  fluidRow(
    column(4, simpleNetworkOutput("simple")),
    column(4, DT::dataTableOutput("table"))
  )
))

server <- shinyServer(function(input, output, session) { 
  session$onSessionEnded(stopApp)
  output$simple <- renderSimpleNetwork({
    sn<-simpleNetwork(df)
    sn$x$options$clickAction = 'Shiny.onInputChange("id",d.name)'
    sn
  })
  
  output$table <- DT::renderDataTable(DT::datatable(funct(input$id)))
})

shinyApp(ui = ui, server = server)