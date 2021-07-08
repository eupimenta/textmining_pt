library(shiny)

#sample data
years <- c(1990,1995,2000,2005,2010)
oryear <- years[3]

server <- function(input, output, session) {
  
  #Input uploaded file
  # inFile<-input$ascii_layer
  
  output$contents <- renderTable({
    inFile <- input$ascii_layer
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
  })
  
  #make dynamic selection     
  output$selectUI <- renderUI({ 
    selectInput("test_select", "Test selection", years, selected=oryear) 
  })
  
  #make dynamic slider
  output$slider <- renderUI({
    sliderInput("inSlider", "Slider", min=input$min_val, max=input$max_val, value=2000)
  })
}

ui <- pageWithSidebar(
  
  headerPanel("Test Shiny App"),
  sidebarPanel(
    
    #File Upload
    fileInput('ascii_layer', 'Choose ASCII Layer', multiple=FALSE, accept='asc'),
    
    #HTML Selection Output from server.R    
    uiOutput("selectUI"),
    
    #Numeric Inputs
    numericInput("min_val", "Enter Minimum Value", 1993),
    numericInput("max_val", "Enter Maximum Value", 2013),
    
    #display dynamic UI
    uiOutput("slider")
    
  ),
  mainPanel(
    tableOutput('contents')
  )
)

runApp(list(ui = ui, server = server))