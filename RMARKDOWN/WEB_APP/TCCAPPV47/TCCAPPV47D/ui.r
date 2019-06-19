#############################################################################################################################
### Esse código foi desenvolv. por Ewerson Pimenta com a versão atual V.4.1 concluída em 03/06/2019 às 04h19
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


library(shiny)
library(readxl)
library(dplyr)
library(DT)
#install.packages("shinythemes")
library(shinythemes)

#devtools::install_github("rstudio/fontawesome")
#library(processx)
#library(pkgload)
#library(fs)
#devtools::install_github("ropenscilabs/icon")
#library(fontawesome) # icon

library (e1071)
#library(icon)


options(OutDec= ",",digits = 4)
options(DT.options = list(pageLength = 15, searchHighlight = TRUE, filter = 'bottom',
                          language = list(search = 'Procurar:')))


######################################################################
######## UI CODE - BEGIN ######## 
################################
################################   
# Define UI for slider demo application
shinyUI(
  # Shiny themes https://rstudio.github.io/shinythemes/
  
  fluidPage(
    theme = shinytheme("slate"),
    
    #theme = shinytheme("darkly"),
    #theme = shinytheme("united"),
    #theme = shinytheme("yeti"),
    #theme = shinytheme("simplex"),
    #theme = shinytheme("sandstone"),
    #theme = shinytheme("flatly"),
    titlePanel("Demonstração: Modelo de Intelig. Artificial p/ Classificação de Textos"),
    
    #sidebarPanel( ),
    
    sidebarLayout(
      sidebarPanel(
        list(
          fileInput("file","Enviar Arquivo .xlsx", multiple = FALSE,buttonLabel = "Procurar", accept = c(#".txt",".csv",
            ".xlsx")), # fileinput() function is used to get the file upload contorl option
          list(helpText(strong("Obs:")), helpText("Apenas arquivos em formato Excel (.xlsx). Tamanho máx. 5MB."))
          
        ),
        
        #h5("Configure a opção de leitura de título do arquivo:"),
        #checkboxInput(inputId = 'header', label = 'Título', value = TRUE),
        #checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
        #selectInput(inputId = 'sep', label = 'Separador', choices = c(','=',',';'=';',Tab='\t', Espaço=' ', 'NULL'=''), selected = ';')
        #,uiOutput("selectfile")
        # This is intentionally an empty object.
        h6(textOutput("save.results")),
        
        #br(),
        helpText(strong("Instruções:")),
        p("1. Faça o envio de um arquivo contendo 2 colunas apenas."),
        p("2. Após envio do arquivo clique na aba 'Classificação' para obter resposta do modelo de preditivo."), 
        p("3. Finalmente, após 1. e 2. clique no botão de Download para baixar os reultados."),
        # This displays the action Button Executar
        #actionButton("run","Executar"),
        list(
          downloadButton('downloadData', 'Download')
          #img(src = "https://upload.wikimedia.org/wikipedia/commons/4/4a/Logo-ence.svg", height = 75),
          #img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 70)
        ),
        
        br(), br(),
        
        #h5("Objetivo da demonstração:"),
        #p("Essa aplicação tem por finalidade simular uma demonstração de utilidade de um modelo de classificação de texto via Random Forest (RF), donde o problema inicial proposto é derivado de uma classificação multinomial com 3 categorias principais: 1: DEA, 2: DEE e 3: OUTROS."),
        
        h5("Projeto de autoria de:"),
        h5("Alize Leal e Ewerson Pimenta (2019)"),
        tags$b("", 
               href=paste0("https://github.com/eupimenta/textmining_pt")),
        h5("Desenvolvido por:"),
        h5("Ewerson Pimenta",tags$a("(Github)", 
                                    href=paste0("https://github.com/eupimenta"))),
        
        # Display the page counter text.
        h5(textOutput("counter")), 
        br(),
        
        #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200),
        list(
          img(src = "https://upload.wikimedia.org/wikipedia/commons/4/4a/Logo-ence.svg", height = 45),
          img(src = "https://logodownload.org/wp-content/uploads/2018/02/ibge-logo.png", height = 38),
          #https://upload.wikimedia.org/wikipedia/commons/4/4a/Logo-ence.svg
          #img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 60)
          h6(helpText("Versão atual V.4.7.2 concluída em 03/06/2019 às 08h14."))
        )
      ),
      
      mainPanel(
        uiOutput("tb"),
        #tableOutput("result")
        ### add your style inline css values here
        
        ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
        tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
### ADD THIS HERE ###
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}

###To change text and background color of the `Select` box ###
                    .dataTables_length select {
                           color: #0E334A;
                           background-color: #0E334A
                           }

###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #0E334A
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    background-color: #bababa
                    }

                   "
                        
                        
        ))
        
      )
      
    )
  )
)


######################################################################
######## UI CODE - END ######## 
################################
################################  
