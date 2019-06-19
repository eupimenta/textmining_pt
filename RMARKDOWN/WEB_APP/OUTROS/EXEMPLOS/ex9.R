library(shiny)
library(shinyjs) 


mpgData <- mtcars

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Test"),
  
  
  # Sidebar with checkboxes to select plot
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      helpText("Select type of plot:"),
      
      checkboxGroupInput("checkPlot", 
                         
                         label = ("Plots"), 
                         
                         choices=c("Baseline","Scenario A"),
                         
                         selected = c("Baseline","Scenario A")
                         
      )
      
    ),
    
    mainPanel(
      textOutput("overview"),
      
      plotOutput("plot")
    )
    
  )
  
)

server <- function(input, output, session) {
  
  
  #get Check group input (type of plot)
  
  checkedVal <- reactive({
    as.vector(input$checkPlot)
  }) 
  
  #plot
  
  
  # first plot
  output$plot <- renderPlot({
    
    if(("Baseline" %in% checkedVal()) & ("Scenario A" %in% checkedVal()))
    {   plot(mpgData$mpg, mpgData$cyl, type='l',col="steelblue",ylim=range(c(mpgData$cyl,mpgData$disp)))
      lines(mpgData$mpg, mpgData$disp, type = "l",col="red")
    }
    else if("Baseline" %in% checkedVal())
    {   
      plot(mpgData$mpg, mpgData$cyl, type='l',col = "steelblue")
    }
    else if("Scenario A" %in% checkedVal())
    {
      plot(mpgData$mpg, mpgData$disp, type='l',col = "red")
    }
  })
  
  
  
  
}


shinyApp(ui, server)