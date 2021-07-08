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


library(shiny)
library(readxl)
library(dplyr)
library(DT)

#devtools::install_github("rstudio/fontawesome")
#devtools::install_github("ropenscilabs/icon")
library(fontawesome) # icon
library (e1071)
library(icon)


options(OutDec= ",",digits = 4)
options(DT.options = list(pageLength = 50, searchHighlight = TRUE, filter = 'bottom',
                          language = list(search = 'Filter:')))


######################################################################
######## UI CODE - BEGIN ######## 
################################
################################   
# Define UI for slider demo application
shinyUI(
 
  fluidPage(
    titlePanel("Demonstração - Modelo de Intelig. Artificial p/ Classificação de Textos"),
    
    #sidebarPanel( ),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file","Enviar Arquivo .xlsx", multiple = FALSE,buttonLabel = "Procurar", accept = c(#".txt",".csv",
          ".xlsx")), # fileinput() function is used to get the file upload contorl option
        p(strong("Obs: Apenas arquivos em formato Excel (.xlsx)")),
        helpText("Tamanho de arquivo máx. 5MB"),
        #h5("Configure a opção de leitura de título do arquivo:"),
        #checkboxInput(inputId = 'header', label = 'Título', value = TRUE),
        #checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
        #selectInput(inputId = 'sep', label = 'Separador', choices = c(','=',',';'=';',Tab='\t', Espaço=' ', 'NULL'=''), selected = ';')
        #,uiOutput("selectfile")
        # This is intentionally an empty object.
        h6(textOutput("save.results")),
        
        br(),
        helpText("Após envio de arquivo clique na aba Classificação de Texto para obter resposta do modelo de preditivo."), 
        helpText("Finalmente, após executar o programa clique em Download para download dos dados com escores obtidos do modelo preditivo."),
        # This displays the action Button Executar
        #actionButton("run","Executar"),
        downloadButton('downloadData', 'Download'),
        br(), br(),
        
        h5("Objetivo da demonstração:"),
        p("Essa aplicação tem por finalidade simular uma demonstração de utilidade de um modelo de classificação de texto via Random Forest (RF), donde o problema inicial proposto é derivado de uma classificação multinomial com 3 categorias principais: 1: DEA, 2: DEE e 3: OUTROS.
"),
        
        h5("Projeto de autoria de:"),
        tags$a("Alize Leal e Ewerson Pimenta (2019)", 
               href=paste0(" https://drive.ibge.gov.br/index.php/f/5695027")),
        tags$b("", 
               href=paste0("https://github.com/eupimenta/textmining_pt")),
        h5("Desenvolvido por:"),
        h6("Ewerson Pimenta",tags$a("(Github)", 
                                    href=paste0("https://github.com/eupimenta"))),
        helpText("Versão atual V.4.5 concluída em 24/05/2019 às 06h26."),
        
        # Display the page counter text.
        h5(textOutput("counter")), 
        br(),
        
        #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200),
        list(
          img(src = "https://logodownload.org/wp-content/uploads/2018/02/ibge-logo.png", height = 50),
          img(src = "https://www.educabras.com/media/faculdades/image/ence.png", height = 50),
          img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 60)
        )
      ),
      
      mainPanel(
        uiOutput("tb")
        #tableOutput("result")
        
      )
      
    )
  )
)
  
  
  ######################################################################
  ######## UI CODE - END ######## 
  ################################
  ################################  
  