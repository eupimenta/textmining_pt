library(shiny)
library(readxl)
library(dplyr)
library(DT)

library(caret)          # Classification and Regression Training 
library(highcharter)    #
library(knitr)          # A general-purpose tool for dynamic report generation in R
library(kableExtra)     # Add features to a kable to generate beautiful tables in html and pdf
library(lsa)            # Calculates a latent semantic vector space from a given document-term matrix.
library(ptstem)         # Stemming português
library(quanteda)       # Quantitative analysis of textual data
library(randomForest)   # RandomForest implements Breiman's random forest algorithm
library(rpart)          # Recursive Partitioning and Regression Trees
library(rpart.plot)     # Plot an rpart model. A simplified interface to the prp function.
library(rslp)           # Removedor de sufixos da lingua portuguesa (stemming)
library(stringr)        # Simple, Consistent Wrappers for Common String Operations
library(scales)         # Generic plot scaling methods
library(tidyr)          # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(tidytext)       # Text Mining using 'dplyr', 'ggplot2', and other Tidy Tools
library(tidyverse)      # data manipulation tidyverse_packages()
library(tinytex)        # Helper Functions to Install and Maintain 'TeX Live' and Compile 'LaTeX' docs
library(tm)             # Text Mining (Term-Document Matrix)
library(topicmodels)    # An R Package for Fitting Topic Models
library(wordcloud)      # create pretty word clouds, visualize diff and similarity between docs
library(wordcloud2)     # An HTML5 interface to wordcloud for data visualization.


runApp(
  list(
    ui = fluidPage(
      titlePanel("Demo - Modelo de Classificação de Textos (IA c/ Envio de arq.) -  V0.01"),
      
      #sidebarPanel( ),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file","Enviar Arquivo .xlsx", multiple = FALSE,buttonLabel = "Procurar", accept = c(#".txt",".csv",
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
          
          helpText("Após envio de arquivo clique no botão 'Executar', a seguir, para obter resposta do modelo de classificação. "), 
          helpText("Finalmente, após executar o programa clique em Download para download dos dados com scores obtidos do modelo preditivo."),
          # This displays the action Button Executar
          actionButton("run","Executar"),
          downloadButton('downloadData', 'Download'),
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
    
    
    ## Read the Prep-Modeling Data Base .csv file
    #getwd(),
    #setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
    db_modelo <- read.csv("db_modelo_rf_v10.csv"),
    var_modelo <- names(db_modelo)[-1],
    
    
    ## Implement the Selected Best Model - Random Forest Classification Predict
    # Partitioning Data into Training and Test
    intrain <- createDataPartition(y = db_modelo$DIRETORIA, p = 0.65, list = FALSE),
    training <- db_modelo[intrain,],
    testing <- db_modelo[-intrain,],
    
    
    ## Read the Stop Words .csv file
    #getwd(),
    #setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
    stopwords_pt = read.csv("stopwords_PT_FINAL.csv", sep = ';', header = F, encoding = "UTF-8"),
    stopwords_pt = stopwords_pt[,1],
    stopwords_pt = as.character(stopwords_pt),
    
    
    server = function(input,output,session) {
      
      ## Side bar select input widget coming through renderUI()
      # Following code displays the select input widget with the list of file loaded by the user
      output$selectfile <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(), 
             helpText("Selecione o arquivo desejado"),
             selectInput("Select", "Selecione", choices=input$file$name)
        )
        
      })
      
      
      ## rawData inputed data by user
      rawData <- eventReactive(input$file, {
        req(input$file)
        inFile <- input$file
        read_excel(inFile$datapath, 1, col_types = c("guess", "text"))
      })
      
      DB <- eventReactive(input$file, {
        req(input$file)
        inFile <- input$file
        read_excel(inFile$datapath, 1, col_types = c("guess", "text"))
      })
      
      
      ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
      # this reactive output display the content of the input$file dataframe
      output$filedf <- renderTable({
        ## inFile is the name of the inputed file
        inFile <- input$file
        
        if(is.null(inFile)){return ()}
        inFile # the file input data frame object that contains the file attributes
      })
      
      # Extract the file path for file
      output$filedf2 <- renderTable({
        if(is.null(input$file)){return ()}
        input$file$datapath # the file input data frame object that contains the file attributes
      })
      
      ## Below code to display the structure of the input file object
      output$fileob <- renderPrint({
        str(rawData())
      })
      
      
      ## Dataset code ##
      # This reactive output contains the dataset and display the dataset in table format
      output$table <- renderDT( 
        rawData(),
        #class = "display nowrap compact", # style
        filter = "top"
      )
      
      ## Summary Stats code ##
      # this reactive output contains the summary of the dataset and display the summary in table format
      output$summ <- renderPrint({
        summary(rawData())
      })
      
      ## Text Classification Model code ##
      # this reactive output contains the results of the classification predictive modeling 
      output$model <- renderDataTable({
        vars <- names(rawData())
        #colnames(rawData()) <- c(rawData().data[[vars[[1]]]], "DESCRI_PEDIDO")
        rawData() %>% 
          mutate(DESCRI_PEDIDO = tolower(.data[[vars[[2]]]])) %>%
          head()
          #mutate(sum = as.numeric(.data[[vars[[1]]]]) + as.numeric(.data[[vars[[1]]]])) %>% 
          #head
        #colnames(Data1) <- c(vars[[1]])
        
          #    mutate(sum = as.numeric(.data[[vars[[1]]]]) + as.numeric(.data[[vars[[1]]]])) %>% 
          
        #output$modifiedData <- renderTable({
        #  vars <- names(rawData())
        #  rawData() %>% 
        #    mutate(sum = as.numeric(.data[[vars[[1]]]]) + as.numeric(.data[[vars[[1]]]])) %>% 
        #    head
        #})
      })
      
      
      output$DEV <- renderDataTable({
        colnames(DB()) <- c("Protocolo", "PEDIDO")
        DB()$PEDIDO <- as.character(tolower(DB()$PEDIDO))
        lin <- DB() %>% dim()[1]
        col <- dim(DB() %>% dim())[2]
        
        ## REMOVE CARACTERES
        DB$PEDIDO = gsub("-","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:.:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:,:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:':]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:!:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:?:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:-:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:__:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:;:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:/:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:(:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:):]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:%:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:º:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:°:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:ª:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("\\d+","",DB$PEDIDO)
        DB$PEDIDO = gsub("[0-9]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:\n\t:]"," ",DB$PEDIDO)
        DB$PEDIDO = gsub("[:\t:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("[:\n:]","",DB$PEDIDO)
        DB$PEDIDO = gsub("\\s+"," ",DB$PEDIDO)
        DB$PEDIDO = gsub("\"","",DB$PEDIDO)
        
        ## STEMMINGS
        DB$PEDIDO = ptstem(rslp(DB$PEDIDO), complete = TRUE)
        DB$PEDIDO = gsub("\\s+"," ",DB$PEDIDO)
        
        ### Stopwords
        mystopwords <- data_frame(palavra = stopwords_pt)
        for (j in 1:dim(DB)[1]) {
          for(i in 1:dim(mystopwords)[1]){
            stopw = as.character(mystopwords[i,1])
            DB$PEDIDO1[j] = gsub(paste0("\\ ",stopw," "), " ", as.character(DB$PEDIDO[j]))
          }
        }
        
        DB %>% head
        #db_modelo <- read.csv("db_modelo_rf_v10.csv")
        #var_modelo <- names(db_modelo)[-1] # gg
        #fe <- matrix(data = 0, nrow = lin, ncol = length(var_modelo))
        #fe <- data.frame(fe); colnames(fe) <- var_modelo
        #i=j=0
        #for(i in 1:length(DB$Protocolo)){
        #  for(j in 1:length(var_modelo)){
        #    g <- grepl(var_modelo[j], DB$PEDIDO1[i])
        #    if(g == TRUE){
        #      fe[i, j] <- 1        
        #    }
        #  }
        #}
        #colnames(rawData()) <- c(rawData().data[[vars[[1]]]], "DESCRI_PEDIDO")
        #dim(rawData()) %>% 
        #  mutate(DESCRI_PEDIDO = tolower(.data[[vars[[2]]]])) %>%
        #  head()
        
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
            tabPanel("Etatísticas descritivas", verbatimTextOutput("summ")),
            tabPanel("Modelo de Classificação de Texto", DT::dataTableOutput("model")),
            tabPanel("DEV", DT::dataTableOutput("DEV")))
      })
      
      
      
      
      # This renders the data downloader
      output$downloadData <- downloadHandler(
        
        filename = "Pedidos_Classificados.csv",
        content = function(file) {
          write.csv(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1, col_names = input$header), file)
        }
      )
      
      N <- 10
      
      result_val <- reactiveVal()
      observeEvent(input$run,{
        result_val(NULL)
        withProgress(message = 'Calculando resposta', {
          for(i in 1:N){
            
            # Long Running Task
            Sys.sleep(0.15)
            
            # Update progress
            incProgress(1/N)
          }
          result_val(quantile(rnorm(10)))
        })
      })
      output$result <- renderTable({
        result_val()
      })
    }
  )
)
