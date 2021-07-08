#############################################################################################################################
### Esse código foi desenvolv. por Ewerson Pimenta com a versão atual V.4.0 concluída em 21/05/2019 às 00h42
# Tem por finalidade simular uma demonstração de utilidade de um modelo de classificação de texto via Random Forest (RF)
# donde o problema é derivado de uma classificação multinomial com 3 categorias principais: 1. DEA, 2.DEE e 3. OUTROS

#### Até o momento o desenvolvimento está OK e funcionando com:
# Input de dados em formato excel apenas com duas colunas onde coluna1 = 'Protocolo' e coluna2 = 'DESCRI_PEDIDO' (OK)
# Algumas visualizações prévias da tabela inputada (.xlsx) como: str(); summary() e DT().  (OK)
# Conversão da tabela original de textos em data.frame contendo apenas Protocolo e variáveis inseridas no modelo, 0 e 1. (OK)
# Stemming; Stop Words; Bag of Words (OK)
# Modelo Random Rorest (RF) Implementado pronto para uso de classificação (OK)
# Classificação via scores obtidos do modelo de aprend. de máquina RF devidamente implementado (OK)
# Botão de Download

#### Falta desenvovler:

# BOTÃO DE EXECUTAR - que inicia a previsão do modelo e retorna o tempo estimado de conclusão da tarefa

#getwd()
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/")

#############################################################################################################################

#rsconnect::appDependencies()

library(rsconnect)
library(reticulate)
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


## Read the Stop Words .csv file
#getwd(),
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
stopwords_pt = read.csv("stopwords_PT_FINAL.csv", sep = ';', header = F, encoding = "UTF-8")

##-> Read the Prep-Modeling Data Base .csv file
#getwd(),
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
db_modelo <- read.csv("db_modelo_rf_v10.csv") # dim(db_modelo); dim(fe)



######################################################################
######## SERVER CODE - BEGIN ######## 
################################
################################      
shinyServer(function(input,output,session) {
  
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
    glimpse(as.tibble(rawData()))
    #rawData()[[2]]
    #class(rawData()[[2]])
    #class(rawData())
    #rawData()[[2]][1]
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
  
  
  
  ######################################################################
  ######## TEXTMINING - PRE-MODELING STEPS: STEMMING, STOPWORDS AND TERMS MATRIX ######## 
  ################################
  ################################   
  
  ## Text Classification PRE-Model code ##
  # this reactive output contains the results of the classification predictive modeling 
  output$textmining <- renderDataTable({
    
    (col_input = ncol(DB()))
    (lin_input = nrow(DB()))
    
    ##-> TOLOWER
    #Pedidos_eSIC_amostra <- read_excel("~/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/DATA/Pedidos_eSIC_amostra.xlsx")
    #DB1 <- Pedidos_eSIC_amostra
    #DB1$DESCRI_PEDIDO = tolower(DB1$`Descrição do Pedido`)
    DB1 <- DB(); #colnames(DB1) <- c("PROTOCOLO", "DESCRI_PEDIDO")
    DB1$DESCRI_PEDIDO = tolower(DB()[[2]])
    
    
    ##-> STOP WORDS
    mystopwords = stopwords_pt %>% select (palavra = V1)
    mystopwords$palavra = as.character(mystopwords$palavra)
    DB1$DESCRI_PEDIDO <- removeWords(DB1$DESCRI_PEDIDO, mystopwords$palavra)
    
    
    ##-> STEMMING
    vars <- names(DB1) #vars <- names(DB())
    texto <-
      DB1 %>%
      mutate(DESCRI_PEDIDO = 
               removeWords(gsub("\\s+"," ",ptstem(rslp(gsub("\"","",gsub("\\s+"," ",gsub("[:§:]","",gsub("[:\":]","",
                                                                                                         gsub("[:\n:]","",gsub("[:\t:]","",gsub("[:\n\t:]"," ",gsub("[0-9]"," ",gsub("\\d+","",
                                                                                                                                                                                     gsub("[:ª:]","",gsub("[:°:]","",gsub("[:º:]","",gsub("[:%:]","",gsub("[:):]","",
                                                                                                                                                                                                                                                          gsub("[:(:]","",gsub("[:/:]"," ",gsub("[:&:]"," ",gsub("[:;:]","",
                                                                                                                                                                                                                                                                                                                 gsub("[:__:]","",gsub("[:-:]"," ",gsub("[:?:]","",gsub("[:!:]","",
                                                                                                                                                                                                                                                                                                                                                                        gsub("[:':]"," ",gsub("[:,:]","",gsub("[:.:]","",
                                                                                                                                                                                                                                                                                                                                                                                                              gsub("-"," ",DESCRI_PEDIDO)       #tolower(.data[[vars[[2]]]])
                                                                                                                                                                                                                                                                                                                                                                        )))))))))))))))))))))))))), complete = TRUE)), mystopwords$palavra)
      )
    
    #View(head(DB1))
    
    
    ##-> BAG OF WORDS MATRIX (0 or 1 MATRIX)        
    var_modelo <- names(db_modelo)[-1]; var_modelo <- unique(var_modelo) 
    fe <- matrix(data = 0, nrow = lin_input, ncol = length(var_modelo))
    fe <- data.frame(fe); colnames(fe) <- var_modelo
    i=j=0
    for(i in 1:lin_input){
      for(j in 1:length(var_modelo)){
        g <- grepl(var_modelo[j], DB1$DESCRI_PEDIDO[i])
        if(g == TRUE){
          fe[i, j] <- 1        
        }
      }
    } # dim(fe) [1] 251 691
    
    # removing unecessary terms
    exclui_termos <- as.character(c())
    # cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
    z=0
    for (k in 1:dim(fe)[2]) {
      if (colSums(fe)[k] <= 4) {
        exclui_termos[z] <- colnames(fe)[k]
        z = z+1
      }
    } # length(exclui_termos) [1] 433
    
    cat(paste0("Existem ", length(exclui_termos), " termos com freq. menor ou igual a 4. Logo, se removermos estes o número de variáveis (termos) resultante será igual a ", dim(fe)[2] - length(exclui_termos), " termos únicos."))
    
    fe <- fe %>% select(-exclui_termos)
    #remove(g,i,j,k,z,exclui_termos,col_input,lin_input,var_modelo,vars)
    #remove(g,i,j,k,z,exclui_termos,col_input,lin_input,var_modelo,vars)
    
    
    #df <- (as_tibble(exclui_termos))
    #View(df[with(df, order(value)), ])
    
    #df1 <- (as_tibble(colnames(db_modelo)[-1]))
    #View(df1[with(df1, order(value)), ])
    
    #fe1 = fe %>% select(-exclui_termos)
    #(fe_fim = as_tibble(cbind(texto, fe1)))
    #dim(fe); dim(fe1)
    
    as_tibble(cbind(texto, fe))
    #remove(fe,texto)
  })
  
  
  ######################################################################
  ######## RANDOM FOREST - CLASSIFICATION MODEL PREDICTION ######## 
  ################################
  ################################      
  output$model = renderDataTable({
    
    ##-> Select only the terms (variables) where the input-data terms are equal to db_modelo terms
    db_modelo <- db_modelo %>% select(DIRETORIA, colnames(fe))
    
    # dim(db_modelo); dim(fe)
    #cbind(Termos = colnames(db_modelo[-1]), Freq_Termos = colSums(db_modelo[-1]))
    #cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
    
    
    ##-> Partitioning data into training and test
    set.seed(098798)
    intrain <- createDataPartition(y = db_modelo$DIRETORIA, p = 0.65, list = FALSE)
    training <- db_modelo[intrain,]
    testing <- db_modelo[-intrain,]
    
    ##-> Random Forest Classif. Model - rf3: (tree = 420, mtry = 26, importance = TRUE, proximity = TRUE) 
    set.seed(2967) #756446
    rf3 <- randomForest(as.factor(DIRETORIA) ~ ., data=training,
                        ntree = 420,
                        mtry = 26,
                        importance = TRUE,
                        proximity = TRUE)
    #rf3
    # Predict the testing set with the trained model
    #predictions3 <- predict(rf3, testing[,-1], type = "class")
    #predict(rf3, testing[,-1], type = "prob")
    #confusionMatrix(predictions3, as.factor(testing$DIRETORIA)) # Accuracy and other metrics
    
    
    ##-> Predicting the input data with the scores obtained with RF model
    resposta = as.character(predict(rf3, fe, type = "response"))
    resposta = as.tibble(cbind(Protocolo = DB1$Protocolo, resposta,
                               round(predict(rf3, fe, type = "prob"),3)))
    #table(Predito = as.character(head(resposta)),Real = head(inputData$DIRETORIA)) # NEED TO GET TO KNOW THE REAL CLASSIFICATION
    resposta
    
  })
  
  
  
  ######################################################################
  ######## RANDOM FOREST - CLASSIFICATION MODEL INFO ######## 
  ################################
  ################################      
  output$RF_model <- renderPrint({
    
    rf3
    
  })
  
  output$RFOOB_plot <- renderPlot({
    
    plot(rf3, main = "Taxa de erro OOB - Out of Bag") #, xlab="Erro", ylab="Árvores geradas")
    legend('topright', colnames(rf3$err.rate), col=1:5, fill=1:5)
    
  })
  
  output$RF_confMatrix <- renderPrint({
    
    # Predict the testing set with the trained model
    predictions3 <- predict(rf3, testing[,-1], type = "class")
    confusionMatrix(predictions3, as.factor(testing$DIRETORIA))
    #remove(predictions3)
    
  })
  
  
  output$highchart1 <- renderHighchart({
    RF_importance = randomForest::importance(rf3)[order(randomForest::importance(rf3)[,1], decreasing = TRUE), ]
    RF1 = data.frame(variables = rownames(RF_importance), importance = RF_importance[,4])
    RF1 = RF1[order(RF1$importance, decreasing = TRUE),]
    rownames(RF1) <- NULL; RF1 = RF1[1:20,]
    #summary(RF1)
    
    #library("highcharter")
    hchart1 <- highchart() %>%
      hc_add_series(data = RF1$importance, 
                    type = "bar",
                    name = "Importância",
                    showInLegend = FALSE,
                    tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = "")) %>%
      hc_yAxis(title = list(text = "Importância"), 
               allowDecimals = TRUE, max = 12,
               labels = list(format = "{value}")) %>%
      hc_xAxis(title = list(text = "Fatores"),
               categories = RF1$variables,
               tickmarkPlacement = "on",
               opposite = FALSE) %>%
      hc_title(text = "Importância de termos - Random Forest",
               style = list(fontWeight = "bold")) %>% 
      hc_subtitle(text = paste("")) %>%
      hc_tooltip(valueDecimals = 2,
                 pointFormat = "Importância: {point.y}")%>%
      #pointFormat = "Variável: {point.x} <br> Importância: {point.y}") 
      hc_credits(enabled = TRUE, 
                 text = "Fonte: CGU, e-SIC (2019). Elaboração: Leal, Alize; Pimenta, Ewerson (2019)",
                 style = list(fontSize = "10px")) %>%
      hc_exporting(enabled = TRUE, filename = "RF-Termsimportance")
    #hc <- hc %>% 
    #  hc_add_theme(hc_theme_darkunica())
    hchart1; 
    #remove(RF_importance, RF1)
    
  }) 
  
  
  
  ######################################################################
  ######## MAIN PANEL - TABPANEL LIST CONFIGURATION ######## 
  ################################
  ################################     
  
  ## MainPanel tabset renderUI code ##
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return(
      mainPanel(
        br(),
        #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200),
        list(
          p("Esse projeto de pesquisa é parte do trabalho de conclusão de curso de Alize de Fárima Antunes Lea e Ewerson Carneiro Pimenta apresentado à Escola Nacional de Ciências Estatísticas do Instituto Brasileiro de Geografia e Estatística como requisito parcial à obtenção do título de Bacharel em Estatística."),
          br(),
          img(src = "https://uploads.toptal.io/blog/post_image/120783/nlp-tutorial-text-classification-60b609c0b7a2622d2b0d6122f2b27f97.png", width = 800),
          br(),
          p("Para iniciar a demonstração, faça o upload (envio) de arquivo no menu ao lado."),
          br(),
          helpText("Esse aplicativo web é inteiramente desenvolvido utilizando recursos de código livre e Open Source como código R e shiny."),
          #img(src = "https://ih0.redbubble.net/image.512525295.6965/stf,small,600x600-c,0,0,1000,1000.u3.jpg", width = 100),
          
          list(
            
            img(src = "https://www.rstudio.com/wp-content/uploads/2018/09/cropped-rstudio.png", height = 60),
            img(src = "https://leeoesterreich.org/wp-content/uploads/2018/10/shiny.png", height = 60),
            img(src = "https://l3software.com.br/wp-content/uploads/2018/05/highcharts.jpg", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/dplyr.png", height = 60),
            img(src = "https://www.tidyverse.org/images/hex-tidyverse.png", height = 60),
            #img(src = "https://d33wubrfki0l68.cloudfront.net/5f8c22ec53a1ac61684f3e8d59c623d09227d6b9/b15de/images/hex-tidyr.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/stringr.png", height = 60),
            #img(src = "https://d33wubrfki0l68.cloudfront.net/0ab849ed51b0b866ef6895c253d3899f4926d397/dbf0f/images/hex-ggplot2.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/tibble-200x232@2x.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/magrittr-200x232@2x.png", height = 60),
            img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/devtools.png", height = 60),
            img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 71),
            
            img(src = "http://transparencia.confea.org.br/wp-content/uploads/2017/06/icone-sic.png", height = 60)),
          #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = 100)),
          br()
          
          
        )
      )
      
    )}
    else
      tabsetPanel(
        tabPanel("Arquivo (objeto r data.frame)", tableOutput("filedf"), tableOutput("filedf2")),
        tabPanel("Estrutura do arq. e Estat. resumo", verbatimTextOutput("fileob"), verbatimTextOutput("summ")),
        tabPanel("Dataset", DT::dataTableOutput("table")),
        tabPanel("Mineração de Texto", DT::dataTableOutput("textmining")),
        tabPanel("Random Forest", verbatimTextOutput("RF_model"), 
                 plotOutput(outputId = "RFOOB_plot", height = "700px", width = "1200px"),
                 verbatimTextOutput("RF_confMatrix"),
                 highchartOutput("highchart1", height = "800px", width = "1200px")),
        tabPanel("Modelo de Classificação de Texto", DT::dataTableOutput("model")))
    #, plotOutput("ConfMatrix")))
  })
  
  
  
  ######################################################################
  ######## DOWNLOAD BUTTON: RETURN SCORES OF THE PREDICTED DATA ######## 
  ################################
  ################################     
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    
    filename = function(){
      #date_time_sys <- as.character(format.Date(Sys.time(), "RJ%Y%m%d%_%Hh%M"))
      paste("ClassifDados-", as.character(format.Date(Sys.time(), "RJ%Y%m%d%-%Hh%M")), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(resposta, file)
    }
  )
  
  
  
  ######################################################################
  ######## EXECUTE BUTTON: RETURN THE TIME TO OBTAIN THE PREDICTION ######## STILL NEED TO IMPLEMENT
  ################################
  ################################   
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
  
  #################
  ######## SERVER CONFIG -END ######## 
  ################################
  ################################      
  
})