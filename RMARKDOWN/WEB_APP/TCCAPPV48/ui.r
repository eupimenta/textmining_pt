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
#install.packages("shinythemes")
library(shinythemes)
library(shinydashboard)
library(devtools)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)

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

#install.packages("shinythemes")
library(shinythemes)
library(shinydashboard)
library(devtools)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)


### creating custom theme object
theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


######################################################################
######## UI CODE - BEGIN ######## 
################################
################################   
# Define UI for slider demo application
shinyUI(
 # Shiny themes https://rstudio.github.io/shinythemes/
    
      dashboardPage(
        dashboardHeader(title = "MineraLAI"), 
        dashboardSidebar(width = 350,
                         sidebarUserPanel("Tester Beta",
                                          subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                                          # Image file should be in www/ subdir
                                          image = "userimage.png"
                         ),
                         #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
                         sidebarMenu(
                           #id = "tabs",
                           #menuItem("Menu Item"),
                           list(
                             fileInput("file","Enviar Arquivo .xlsx", multiple = FALSE,buttonLabel = "Procurar", accept = c(#".txt",".csv",
                               ".xlsx")), # fileinput() function is used to get the file upload contorl option
                             list(helpText(HTML(" Apenas arquivos em formato Excel (.xlsx). <br/> Tamanho máx. 5MB.")))
                             
                           ),
                           menuItem(helpText((" Instruções:")),
                                    helpText(" 1. Faça o envio de um arquivo contendo 2 colunas."),
                                    helpText(HTML(" 2. Após envio do arquivo clique na aba 'Classificação' <br/>para obter resposta do modelo de preditivo.")), 
                                    helpText(HTML(" 3. Finalmente, após 1. e 2. clique no botão Download <br/>para baixar os reultados."))
                           ),
                           
                           menuItem("Arquivo enviado", icon = icon("file-excel"),
                                    menuSubItem("Tabela orig.", tabName = "Tabela_orig"), #, icon = icon("table")),
                                    menuSubItem("Tabela após mineração", tabName = "subitem2") #, icon = icon("table"))
                           ),
                           menuItem("Mineração de Texto", icon = icon("comment-dots"),
                                    menuSubItem("Process. e partição", tabName = "subitem1", icon = icon("vials")),
                                    menuSubItem("Base Teste (25 obs.)", tabName = "subitem2", icon = icon("table"))
                           ),
                           menuItem("Random Forest", icon = icon("laptop-code"),
                                    menuSubItem("Treino", tabName = "subitem1", icon = icon("window-restore")),
                                    menuSubItem("Teste", tabName = "subitem1", icon = icon("clone")),
                                    menuSubItem("Termos Importantes", tabName = "subitem2", icon = icon("chart-bar"))
                           ),
                           #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                           #menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                           #        badgeColor = "green"),
                           #menuItem("Charts", icon = icon("bar-chart-o"),
                           #          menuSubItem("Sub-item 1", tabName = "subitem1"),
                           #         menuSubItem("Sub-item 2", tabName = "subitem2")
                           # ),
                           menuItem("Classificação", icon = icon("sitemap"), tabName = "widgets", badgeLabel = "resposta",
                                            badgeColor = "green"
                           ),
                           
                                     h6(textOutput("save.results")),
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
                                     
                                     h5(" Projeto de autoria de:"),
                                     tags$a(" Alize Leal e Ewerson Pimenta (2019)", 
                                            href=paste0(" https://drive.ibge.gov.br/index.php/f/5695027")),
                                     tags$b("", 
                                            href=paste0("https://github.com/eupimenta/textmining_pt")),
                                     h5(" Desenvolvido por:"),
                                     h6("",tags$a("Ewerson Pimenta (Github)", 
                                                                 href=paste0("https://github.com/eupimenta"))),
                                     helpText(" Versão atual V.5.0 concluída em 30/05/2019 às 03h10."),
                                     
                                     # Display the page counter text.
                                     h5(textOutput("counter")), 
                                     br(),
                                     
                                     #img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Gray.png", height = 70, width = 200),
                                     list(
                                       img(src = "https://upload.wikimedia.org/wikipedia/commons/4/4a/Logo-ence.svg", height = 45),
                                       img(src = "https://logodownload.org/wp-content/uploads/2018/02/ibge-logo.png", height = 38)
                                       #https://upload.wikimedia.org/wikipedia/commons/4/4a/Logo-ence.svg
                                       #img(src = "https://opensource.org/files/osi_standard_logo_0.png", height = 60)
                                     ))),
        dashboardBody(
          shinyDashboardThemes(theme = "onenote"),
          #shinyDashboardThemes(theme = "poor_mans_flatly"),
          #shinyDashboardThemes(theme = "grey_dark"),
          #shinyDashboardThemes(theme = "blue_gradient"),
          #theme_blue_gradient,
        uiOutput("tb")))
        #tableOutput("result")
      )

  
  
  ######################################################################
  ######## UI CODE - END ######## 
  ################################
  ################################  
  