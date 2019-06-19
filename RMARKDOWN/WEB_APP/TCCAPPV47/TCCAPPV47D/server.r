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

# referências: 
# https://medium.com/data-from-the-trenches/text-classification-the-first-step-toward-nlp-mastery-f5f95d525d73
# https://www.expertsystem.com/natural-language-processing-and-text-mining/
# https://rpubs.com/williamsurles/316682
# https://www.maxwell.vrac.puc-rio.br/11675/11675_5.PDF
# https://shiny.rstudio.com/articles/css.html
# https://deanattali.com/blog/advanced-shiny-tips/

# Artigos ref:
# http://www.lbd.dcc.ufmg.br/colecoes/sbsi/2015/089.pdf

#getwd()
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/")

#############################################################################################################################

#rsconnect::appDependencies()

list.of.packages <- c("rsconnect","reticulate","shiny","readxl", "backports","bibtex","caret","class","cluster","curl","devtools",
                      "dplyr","DT","e1071","highcharter","knitr","keras","kableExtra","lsa","ptstem","quanteda",
                      "randomForest","rpart","rpart.plot","rslp","stringr","scales","tibble","tidyr","tidytext","tidyverse",
                      "tinytex","tm","topicmodels","wordcloud","wordcloud","wordcloud2")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#remove(list.of.packages, new.packages)

#devtools::install_github("rstudio/fontawesome")
#devtools::install_github("ropenscilabs/icon")
#bibtex::write.bib(list.of.packages[-length(list.of.packages)])




library(rsconnect)
library(reticulate)
library(shiny)
library(readxl)
library(dplyr)
library(DT)

library(caret)          # Classification and Regression Training 
library(class)
library(highcharter)    #
library(backports)
library(curl)
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
library(tibble)
library(tidytext)       # Text Mining using 'dplyr', 'ggplot2', and other Tidy Tools
library(tidyverse)      # data manipulation tidyverse_packages()
library(tinytex)        # Helper Functions to Install and Maintain 'TeX Live' and Compile 'LaTeX' docs
library(tm)             # Text Mining (Term-Document Matrix)
library(topicmodels)    # An R Package for Fitting Topic Models
#library(wordcloud)      # create pretty word clouds, visualize diff and similarity between docs
library(wordcloud2)     # An HTML5 interface to wordcloud for data visualization.
#library(fontawesome) # icon
#
library(shinythemes)
library (e1071)
#library(icon)

#https://ropensci.org/technotes/2018/05/15/icon/
#`r fa_coffee(colour = "#1FA67A", size = 2)+ `r fa_r_project(colour = "#384CB7", size = 2)` + `r fa_chart_line(colour = "#f7a12a", size = 2)` = `r fa_heart(colour = "red", size = 2)`
#fa_icon(iconname = "circle")

#devtools::install_github("rstudio/fontawesome")



## Read the Stop Words .csv file
#getwd(),
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
stopwords_pt = read.csv("stopwords_PT_FINAL.csv", sep = ';', header = F, encoding = "UTF-8")

##-> Read the Prep-Modeling Data Base .csv file
#getwd(),
#setwd("/Users/ewersonpimenta/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/WEB_APP/"),
db_modelo <- read.csv("db_modelo_rf_v10.csv") # dim(db_modelo); dim(fe)
db_modelo <- as_tibble(db_modelo)

db_modelo0 <- read.csv("db_modelo_rf_v20.csv") # dim(db_modelo); dim(fe)
db_modelo0 <- as_tibble(db_modelo0)

set.seed(098798)
intrain <- createDataPartition(y = db_modelo$DIRETORIA, p = 0.65, list = FALSE)
training <- db_modelo[intrain,]; 
testing <- db_modelo[-intrain,];

set.seed(098798)
intrain0 <- createDataPartition(y = db_modelo0$DIRETORIA, p = 0.65, list = FALSE)
training0 <- db_modelo0[intrain0,]; 
testing0 <- db_modelo0[-intrain0,];

tab_dim <-
  rbind(cbind(t(table(db_modelo$DIRETORIA)/sum(table(db_modelo$DIRETORIA))),TotalPedidos = sum(table(db_modelo$DIRETORIA))),
        cbind(t(table(training$DIRETORIA)/sum(table(training$DIRETORIA))),TotalPedidos = sum(table(training$DIRETORIA))),
        cbind(t(table(testing$DIRETORIA)/sum(table(testing$DIRETORIA))),TotalPedidos = sum(table(testing$DIRETORIA))))
rownames(tab_dim) <- c("Base Cheia", "Treino", "Teste")

tab_dim0 <-
  rbind(cbind(t(table(db_modelo0$DIRETORIAS)/sum(table(db_modelo0$DIRETORIAS))),TotalPedidos = sum(table(db_modelo0$DIRETORIAS))),
        cbind(t(table(training0$DIRETORIAS)/sum(table(training0$DIRETORIAS))),TotalPedidos = sum(table(training0$DIRETORIAS))),
        cbind(t(table(testing0$DIRETORIAS)/sum(table(testing0$DIRETORIAS))),TotalPedidos = sum(table(testing0$DIRETORIAS))))
rownames(tab_dim0) <- c("Base Cheia", "Treino", "Teste")

txt1 <- paste0("A base de dados 'cheia' contém todo o histórico de classificações de pedidos registrados e encaminhados à Empresa de Pesquisa Energética (EPE) e devidamente respondidos pelas diretorias da EPE responsáveis pelo assunto contido no pedido entre 01 de julho de 2015 e 27 de março de 2019.")
txt2 <- paste0("Os seguintes passos foram aplicados na base cheia: 1° uma reclassificação das diretorias de 5 (DEA, DEE, DGC, DPG, OUTROS) para 3 classes (DEA, DEE, OUTROS) e reagrupamento da base por diretoria, 2° foram aplicadas etapas de mineração de texto: 2.1. limpeza e pre-processamento de texto (remoção de stop words, aplicação de stemming), 2.2. escolha dos termos/palavras-chaves mais relevantes, agrupados por diretoria, via estatística tf-idf,  e 2.3. Reestruturação dos dados em uma data.frame via abordagem de bag of words (matriz de zeros e uns).")
txt3 <- paste0("Após o passo anterior, a base cheia continha dimensões de ", dim(db_modelo)[1]," linhas (observações/pedidos) por ", dim(db_modelo)[2], " colunas/variáveis. Vale ressaltar que das C = ",dim(db_modelo)[2], " colunas, a primeira é a variável resposta contendo as categorias da Diretoria reclassificada: DEA, DEE e OUTROS. Já as K = ", dim(db_modelo)[2]-1," variáveis restantes são todas binárias indicando a ocorrência ou não do k-ésimo termo (palavra-chave/token) durante a descrição do i-ésimo pedido registrado, para todo i=1,2,...,", dim(db_modelo)[1],".")
txt4 <- paste0("Além disso, as bases de treino e teste receberam, respectivamente, 65% e 35% de todas as observações. Ou seja, a base de teste contém ", dim(testing)[1], " dos ", dim(db_modelo)[1]," pedidos registrados, selecionados via amostragem aleatória simples com um balanceamento da distribuição de classes durante a partição de dados.")
#txt5 <- paste0("A tabela abaixo mostra a proporção de observações por categoria da variável resposta (DIRETORIA) em cada uma das 3 bases (cheia, treino e teste):")

txt_bases <- rbind(txt1,txt2,txt3,txt4); rownames(txt_bases) <- NULL
#txt_bases <- cbind(rbind(txt1,txt2),rbind(txt3,txt4)); rownames(txt_bases) <- NULL

##-> Random Forest Classif. Model - rf3: (tree = 420, mtry = 26, importance = TRUE, proximity = TRUE) 
set.seed(2967) #756446
rf3 <- randomForest(as.factor(DIRETORIA) ~ ., data=training,
                    ntree = 420,
                    mtry = 26,
                    importance = TRUE,
                    proximity = TRUE)

#rf3
predictions3 <- predict(rf3, testing[,-1], type = "class") #Predict the testing set with the trained model
#predict(rf3, testing[,-1], type = "prob")
#confusionMatrix(predictions3, as.factor(testing$DIRETORIA)) # Accuracy and other metrics

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
           allowDecimals = TRUE, max = max(RF1$importance),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Termo"),
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
hchart1 <- hchart1 %>% 
  hc_add_theme(hc_theme_google())
#hc_add_theme(hc_theme_538())
#hc_add_theme(hc_theme_flatdark())
#hc_add_theme(hc_theme_darkunica())

# Selecioanndo Árvore de Decisão
rf <- rpart(DIRETORIA ~ ., data=training, method="class", xval = 3)


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
    
    
    #withProgress(message = 'Processando', value = 1, {
    # Number of times we'll go through the loop
    #  n <- 2
    
    #  for (z in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    
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
    
    # Increment the progress bar, and update the detail text.
    #incProgress(1/n, detail = paste0("Etapa ", z, "/",n))
    
    # Pause for 0.1 seconds to simulate a long computation.
    #Sys.sleep(0.001)
    #  }
    #})
    as_tibble(cbind(texto, fe))
    #remove(fe,texto)
  })
  
  
  ######################################################################
  ######## DATASET TREINO+TESTE ######## 
  ################################
  ################################      
  
  output$dbmodelo0 <- function() {
    
    # Tabela 1 Alguns textos de insights gerados na partição (split) dos dados
    kable(txt_bases) %>%
      kable_styling(full_width = F) %>%
      column_spec(1, bold = T) %>%
      column_spec(1, width = "65em")
    
    
  }
  
  output$dbmodelo01 <- function() {
    
    # Tabela 2 contendo a prop. de observações por categoria da Diretoria e base
    kable(round(tab_dim0,2)) %>%
      kable_styling(full_width = F) %>%
      column_spec(1, bold = T) 
    
  }  
  
  output$dbmodelo1 <- function() {
    
    # Tabela 2 contendo a prop. de observações por categoria da Diretoria e base
    kable(round(tab_dim,2)) %>%
      kable_styling(full_width = F) %>%
      column_spec(1, bold = T) 
    
  }
  
  
  output$dbmodelo2 <- renderDT(
    
    # Tabela 3 contendo as 25 primeiras obs. da base de teste
    #kable(testing[1:25,]) %>%
    #  kable_styling(full_width = F) %>%
    #  column_spec(1, bold = T) 
    testing[1:25,],
    filter = "top"
    
  )
  
  
  ######################################################################
  ######## RANDOM FOREST - CLASSIFICATION MODEL PREDICTION ######## 
  ################################
  ################################      
  resposta = reactive({
    (col_input = ncol(DB()))
    (lin_input = nrow(DB()))
    
    ##-> TOLOWER
    #Pedidos_eSIC_amostra <- read_excel("~/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/DATA/Pedidos_eSIC_amostra2.xlsx")
    #DB1 <- Pedidos_eSIC_amostra
    #DB1$DESCRI_PEDIDO = tolower(DB1$`Descrição do Pedido`)
    DB1 <- DB(); #colnames(DB1) <- c("PROTOCOLO", "DESCRI_PEDIDO")
    DB1$DESCRI_PEDIDO = tolower(DB()[[2]])
    
    
    ##-> STOP WORDS
    mystopwords = stopwords_pt %>% select (palavra = V1)
    mystopwords$palavra = as.character(mystopwords$palavra)
    DB1$DESCRI_PEDIDO <- removeWords(DB1$DESCRI_PEDIDO, mystopwords$palavra)
    
    withProgress(message = 'Processando', value = 0.35, {
      # Number of times we'll go through the loop
      n <- 1
      
      for (z in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
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
        #exclui_termos <- as.character(c())
        # cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
        #z=0
        #for (k in 1:dim(fe)[2]) {
        #  if (colSums(fe)[k] <= 4) {
        #    exclui_termos[z] <- colnames(fe)[k]
        #    z = z+1
        #  }
        # # length(exclui_termos) [1] 433
        
        #cat(paste0("Existem ", length(exclui_termos), " termos com freq. menor ou igual a 4. Logo, se removermos estes o número de variáveis (termos) resultante será igual a ", dim(fe)[2] - length(exclui_termos), " termos únicos."))
        #fe <- fe %>% select(-exclui_termos)
        
        ##-> Select only the terms (variables) where the input-data terms are equal to db_modelo terms
        #(levels(as.factor(colnames(db_modelo))))
        #(levels(as.factor(colnames(fe))))
        
        
        # dim(db_modelo); dim(fe)
        #cbind(Termos = colnames(db_modelo[-1]), Freq_Termos = colSums(db_modelo[-1]))
        #cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
        
        
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
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste0("Etapa ", z, "/",n))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    resposta
  })
  
  
  
  output$model = renderDataTable({
    
    (col_input = ncol(DB()))
    (lin_input = nrow(DB()))
    
    ##-> TOLOWER
    #Pedidos_eSIC_amostra <- read_excel("~/Desktop/ESIC_TCC/TCC_v2.1/RMARKDOWN/DATA/Pedidos_eSIC_amostra2.xlsx")
    #DB1 <- Pedidos_eSIC_amostra
    #DB1$DESCRI_PEDIDO = tolower(DB1$`Descrição do Pedido`)
    DB1 <- DB(); #colnames(DB1) <- c("PROTOCOLO", "DESCRI_PEDIDO")
    DB1$DESCRI_PEDIDO = tolower(DB()[[2]])
    
    
    ##-> STOP WORDS
    mystopwords = stopwords_pt %>% select (palavra = V1)
    mystopwords$palavra = as.character(mystopwords$palavra)
    DB1$DESCRI_PEDIDO <- removeWords(DB1$DESCRI_PEDIDO, mystopwords$palavra)
    
    withProgress(message = 'Processando', value = 0.15, {
      # Number of times we'll go through the loop
      n <- 2
      
      for (z in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
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
        #exclui_termos <- as.character(c())
        # cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
        #z=0
        #for (k in 1:dim(fe)[2]) {
        #  if (colSums(fe)[k] <= 4) {
        #    exclui_termos[z] <- colnames(fe)[k]
        #    z = z+1
        #  }
        # # length(exclui_termos) [1] 433
        
        #cat(paste0("Existem ", length(exclui_termos), " termos com freq. menor ou igual a 4. Logo, se removermos estes o número de variáveis (termos) resultante será igual a ", dim(fe)[2] - length(exclui_termos), " termos únicos."))
        #fe <- fe %>% select(-exclui_termos)
        
        ##-> Select only the terms (variables) where the input-data terms are equal to db_modelo terms
        #(levels(as.factor(colnames(db_modelo))))
        #(levels(as.factor(colnames(fe))))
        
        
        # dim(db_modelo); dim(fe)
        #cbind(Termos = colnames(db_modelo[-1]), Freq_Termos = colSums(db_modelo[-1]))
        #cbind(Termos = colnames(fe), Freq_Termos = colSums(fe))
        
        
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
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste0("Etapa ", z, "/",n))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    resposta
    
  })
  
  
  
  ######################################################################
  ######## RANDOM FOREST - CLASSIFICATION MODEL INFO ######## 
  ################################
  ################################  
  
  ### RANDOM FOREST RESULTS
  output$RF_model <- renderPrint({
    
    rf3
    
  })
  
  ### TAXA DE ERRO OOB
  output$RFOOB_plot <- renderPlot({
    
    plot(rf3, main = "Taxa de erro OOB - Out of Bag") #, xlab="Erro", ylab="Árvores geradas")
    legend('topright', colnames(rf3$err.rate), col=1:5, fill=1:5)
    
  })
  
  ### MATRIZ DE CONFUSÃO BASE TESTE
  output$RF_confMatrix <- renderPrint({
    
    confusionMatrix(predictions3, as.factor(testing$DIRETORIA))
    
  })
  
  
  ### VARIÁVEIS IMPORTANTES MODELO
  output$highchart1 <- renderHighchart({
    
    hchart1
    
  }) 
  
  ### EXTRAINDO UMA ÁRVORE
  #output$arvore1 <- renderPlot({
  
  #  rf <- rpart(DIRETORIA ~ ., data=training, method="class", xval = 4)
  #rpart::plotcp(rf)
  #  rpart.plot(rf)
  #rpart.plot.version1(rf) 
  #remove(RF_importance, RF1)
  
  #}) 
  
  
  ### EXTRAINDO UMA ÁRVORE
  output$arvore1 <- renderPlot({
    
    #rpart::plotcp(rf)
    rpart.plot(rf)
    #rpart.plot.version1(rf) 
    
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
          tags$div(
            "Esse aplicativo web (webapp) compõe o trabalho de conclusão de curso de Alize de Fátima Antunes Lea e Ewerson Carneiro Pimenta com o título ",
            tags$i("'Mineração de Texto e Aprendizado de Máquina Supervisionado Aplicados à Lei de Acesso a Informação' "),
            " apresentado à Escola Nacional de Ciências Estatísticas do Instituto Brasileiro de Geografia e Estatística como requisito parcial à obtenção do título de Bacharel em Estatística."
          ),
          br(),
          p("Essa aplicação tem por finalidade simular uma demonstração de utilidade de um modelo de classificação de texto via Random Forest (RF), donde o problema inicial proposto é derivado de uma classificação multinomial com 3 classes: "),
          p("1. DEA; 2. DEE; 3. OUTROS."),
          #img(src = "https://uploads.toptal.io/blog/post_image/120783/nlp-tutorial-text-classification-60b609c0b7a2622d2b0d6122f2b27f97.png", width = 370), #MOBILE
          img(src = "https://uploads.toptal.io/blog/post_image/120783/nlp-tutorial-text-classification-60b609c0b7a2622d2b0d6122f2b27f97.png", width = 750),
          br(),
          p("Para iniciar a demonstração, faça o upload (envio) de arquivo no menu inicial."),
          br(),
          #helpText("(Esse webapp é inteiramente desenvolvido utilizando recursos Open Source)"),
          #img(src = "https://ih0.redbubble.net/image.512525295.6965/stf,small,600x600-c,0,0,1000,1000.u3.jpg", width = 100),
          
          list(
            
            img(src = "https://www.rstudio.com/wp-content/uploads/2018/09/cropped-rstudio.png", height = 30),
            img(src = "https://leeoesterreich.org/wp-content/uploads/2018/10/shiny.png", height = 30),
            img(src = "https://l3software.com.br/wp-content/uploads/2018/05/highcharts.jpg", height = 30),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/dplyr.png", height = 60),
            img(src = "https://www.tidyverse.org/images/hex-tidyverse.png", height = 30),
            #img(src = "https://d33wubrfki0l68.cloudfront.net/5f8c22ec53a1ac61684f3e8d59c623d09227d6b9/b15de/images/hex-tidyr.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/stringr.png", height = 60),
            #img(src = "https://d33wubrfki0l68.cloudfront.net/0ab849ed51b0b866ef6895c253d3899f4926d397/dbf0f/images/hex-ggplot2.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/tibble-200x232@2x.png", height = 60),
            #img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/magrittr-200x232@2x.png", height = 60),
            img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/devtools.png", height = 30),
            #img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 34),
            
            img(src = "http://transparencia.confea.org.br/wp-content/uploads/2017/06/icone-sic.png", height = 30)),
          #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = 100)),
          br()
          
          
        )
      )
      
    )}
    else
      tabsetPanel(
        navbarMenu("Arquivo de envio",
                   tabPanel("Tabela orig.",fluidRow(titlePanel("Tabela de envio de Dados"),
                                                    DT::dataTableOutput("table"))),
                   #tabPanel("Info. prelim.", tableOutput("filedf"), tableOutput("filedf2"),verbatimTextOutput("fileob"), verbatimTextOutput("summ")),
                   tabPanel("Tabela após mineração", fluidRow(titlePanel("Tabela de envio de Dados após mineração de texto"),
                                                              DT::dataTableOutput("textmining")))
        ),
        
        navbarMenu("Mineração de Texto",
                   tabPanel("Processamento e Partição", fluidRow(titlePanel("Histórico de pedidos"), p("Descrevemos a seguir os passos de pre-processamento, mineração de texto e processamento de língua natural (PLN) para reestruturada da base de acordo com abordagem Bag Of Words (BOW). Também descrevemos a partição de dados executada nos dados históricos."),
                                                                 tableOutput("dbmodelo0"),
                                                                 p("A tabela abaixo mostra a proporção de histórico de pedidos por categoria da variável resposta (DIRETORIA) em cada uma das 3 bases (cheia, treino e teste), antes da reclassificação:"),
                                                                 tableOutput("dbmodelo01"),
                                                                 p("Após reclassificação das diretorias: "),
                                                                 tableOutput("dbmodelo1"), 
                                                                 p("As bases de treino e teste estão com as categorias da variável resposta bem balanceadas e aptas para implementação do modelo preditivo."))),
                   # tabPanel("Partição Treino+Teste",tableOutput("dbmodelo1")),
                   tabPanel("Base Teste (15 obs.)", fluidRow(titlePanel("Base Teste 15 primeiras obs."),
                                                             dataTableOutput("dbmodelo2")))
        ),
        
        navbarMenu("Random Forest (RF)",
                   tabPanel("Resultados treino", fluidRow(titlePanel("Resultados do modelo Treinado"),p("Taxa de erro Out of Bag (OOB) e matriz de confusãodo modelo treinado."),
                                                          verbatimTextOutput("RF_model"),
                                                          #plotOutput(outputId = "RFOOB_plot",  height = "350px", width = "400px"))), # MOBILE
                                                          plotOutput(outputId = "RFOOB_plot", height = "700px", width = "1200px"))),
                   tabPanel("Resultados teste", fluidRow(titlePanel("Matriz de confusão da base de teste"),
                                                         verbatimTextOutput("RF_confMatrix"))),
                   tabPanel("Import. termo", fluidRow(titlePanel("Gráfico de importância dos termos"),
                                                      #highchartOutput("highchart1", height = "650px", width = "400px"))),  # MOBILE
                                                      highchartOutput("highchart1", height = "800px", width = "1200px"))),
                   tabPanel("Árvore de decisão", fluidRow(titlePanel("Selecionando uma árvore de decisão"),
                                                          #plotOutput(outputId = "arvore1", width = "400px"))) # MOBILE
                                                          plotOutput(outputId = "arvore1", width = "1200px")))
        ),
        
        #plotOutput(outputId = "arvore1", height = "700px", width = "1200px")),
        tabPanel("Classificação", fluidRow(titlePanel("Resposta de classificação do modelo RF"),
                                           DT::dataTableOutput("model"))))
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
      paste("DadosClassificados_LealPimenta-", as.character(format.Date(Sys.time(), "RJ%Y%m%d%-%Hh%M")), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(resposta(), file, sep = ";")
    }
  )
  
  
  #################
  ######## SERVER CONFIG -END ######## 
  ################################
  ################################      
  
})
