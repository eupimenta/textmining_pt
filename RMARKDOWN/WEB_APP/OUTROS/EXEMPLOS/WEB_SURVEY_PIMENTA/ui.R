library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Pesquisa Sobre Humanização e Papel de Gatos no Domicílio (PSHPGD) usando Shiny Survey Tool v.02"),
  
  sidebarPanel(
    # This is intentionally an empty object.
    h6(textOutput("save.results")),
    h5("Criado por:"),
    tags$a("Ewerson Pimenta", 
           href="https://github.com/eupimenta"),
    h5("Para maiores detalhes sobre a pesquisa:"),
    tags$a("IBGE/DRIVE", 
           href=paste0(" https://drive.ibge.gov.br/index.php/f/5695027")),
    h5("Github Repository:"),
    p("Esse questionário utiliza o seguinte template"),
    tags$a("Survey-Tool - by Francis Smart", 
           href=paste0("https://github.com/EconometricsBySimulation/",
                       "Shiny-Demos/tree/master/Survey")),
    # Display the page counter text.
    h5(textOutput("counter")),
    p("Obs: NA: Não se Aplica ou não quis responder"),
    br(),
    img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200)
    
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("MainAction"),
    # This displays the action putton Next.
    actionButton("Click.Counter", "Seguinte")    
  )
))
