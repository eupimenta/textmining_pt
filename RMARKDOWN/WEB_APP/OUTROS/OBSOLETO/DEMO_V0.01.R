library(shiny)
library(readxl)
library(dplyr)
library(DT)


runApp(
  list(
    ui = fluidPage(
      titlePanel("Demo - Modelo de Classificação de Textos (IA c/ Envio de arq.) -  V0.01"),
      
      #sidebarPanel( ),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file","Enviar Arquivo", multiple = FALSE,buttonLabel = "Procurar", accept = c(#".txt",".csv",
                                                                                                   ".xlsx")), # fileinput() function is used to get the file upload contorl option
          p(strong("Obs: Apenas arquivos em formato Excel (.xlsx)")),
          helpText("Tamanho de arquivo máx. 5MB"),
          helpText("Configure as opções de leitura do arquivo"),
          checkboxInput(inputId = 'header', label = 'Título', value = TRUE),
          #checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
          #selectInput(inputId = 'sep', label = 'Separador', choices = c(','=',',';'=';',Tab='\t', Espaço=' ', 'NULL'=''), selected = ';')
          #,uiOutput("selectfile")
          # This is intentionally an empty object.
          h6(textOutput("save.results")),
          
          helpText("Após envio de arquivo clique no botão 'Executar', \n a seguir, para obter resposta do modelo de classificação:"),
          # This displays the action Button Executar
          actionButton("run","Executar"),
          br(),
          
          h5("Projeto de autoria de:"),
          tags$a("Alize Leal e Ewerson Pimenta (2019)", 
                 href=paste0(" https://drive.ibge.gov.br/index.php/f/5695027")),
          tags$b("", 
                 href=paste0("https://github.com/eupimenta/textmining_pt")),
          h5("Desenvolvido por:"),
          h6("Ewerson Pimenta",tags$a("(Github)", 
                                      href=paste0("https://github.com/eupimenta"))),
          
          # Display the page counter text.
          h5(textOutput("counter")), 
          br(),
          
          #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200),
          img(src = "https://www.educabras.com/media/faculdades/image/ence.png", width = 370)
        ),
        
        mainPanel(
          uiOutput("tb")
          #tableOutput("result")
          
        )
        
      )
    ),
    
    options(DT.options = list(pageLength = 50, searchHighlight = TRUE, filter = 'bottom',
            language = list(search = 'Filter:'))),
    
    server = function(input,output) {
      
      ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
      # this reactive output display the content of the input$file dataframe
      output$filedf <- renderTable({
        ## inFile is the name of the inputed file
        inFile <- input$file
        
        if(is.null(inFile)){return ()}
        inFile # the file input data frame object that contains the file attributes
        
        #if(is.null(inFile))
        #  return(NULL)
        #file.rename(inFile$datapath,
        #            paste(inFile$datapath, ".xlsx", sep=""))
        #read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      })
      
      # Extract the file path for file
      output$filedf2 <- renderTable({
        if(is.null(input$file)){return ()}
        input$file$datapath # the file input data frame object that contains the file attributes
      })
      
      ## Below code to display the structure of the input file object
      output$fileob <- renderPrint({
        if(is.null(input$file)){return()}
        inFile <- input$file
        if(is.null(inFile))
          return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        str(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1, col_names = input$header))
        
      })
      
      ## Side bar select input widget coming through renderUI()
      # Following code displays the select input widget with the list of file loaded by the user
      output$selectfile <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(), 
             helpText("Selecione o arquivo desejado"),
             selectInput("Select", "Selecione", choices=input$file$name)
        )
        
      })
      
      ## Dataset code ##
      # This reactive output contains the dataset and display the dataset in table format
      output$table <- renderDataTable({ 
        if(is.null(input$file)){return()}
        inFile <- input$file
        if(is.null(inFile))
          return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1, col_names = input$header)
        
      })
      
      ## Summary Stats code ##
      # this reactive output contains the summary of the dataset and display the summary in table format
      output$summ <- renderPrint({
        if(is.null(input$file)){return()}
        inFile <- input$file
        if(is.null(inFile))
          return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        summary(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1, col_names = input$header))
        
        
        
        #summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
        #                   sep=input$sep, 
        #                   header = input$header, 
        #                   stringsAsFactors = input$stringAsFactors))
      })
      
      ## MainPanel tabset renderUI code ##
      # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
      # Until the file is loaded, app will not show the tabset.
      output$tb <- renderUI({
        if(is.null(input$file)) {return()}
        else
          tabsetPanel(
            tabPanel("Arquivo (objeto r data.frame)", tableOutput("filedf"), tableOutput("filedf2")),
            tabPanel("Estrutura do arquivo", verbatimTextOutput("fileob")),
            tabPanel("Dataset", DT::dataTableOutput("table")),
            tabPanel("Etatísticas descritivas", verbatimTextOutput("summ")))
      })
      
      N <- 10
      
      result_val <- reactiveVal()
      observeEvent(input$run,{
        result_val(NULL)
        withProgress(message = 'Calculando resposta', {
          for(i in 1:N){
            
            # Long Running Task
            Sys.sleep(1)
            
            # Update progress
            incProgress(1/N)
          }
          result_val(quantile(rnorm(1000)))
        })
      })
      output$result <- renderTable({
        result_val()
      })
    }
  )
)