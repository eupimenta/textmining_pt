---
title: "Programa de retenção de clientes de telecomunicações"
subtitle: "R Notebook"
author: "Ewerson C. Pimenta e Marcos Antônio E. de Oliveira"
date: "Rio de Janeiro, 16 de agosto de 2018"
output:
    html_document:
    highlight: tango
    mathjax: null
    number_sections: yes
    theme: lumen
    toc: yes
    toc_float: yes
    code: hide
---
  
  
  
  <!-- highlight: tango pygments kate monochrome zenburn haddock textmate -->
  <!-- theme: cerulean, journal, flatly, readable, spacelab, united, cosmo, lumen, paper, sandston, simplex, yeti -->
  
  <!----------  INÍCIO CONFIG  ---------->
  



  
  
## Banco de Dados
  
  
### Descrição
  
  O conjunto de dados utilizado foi o de título *Telco Customer Churn* extraído da plataforma [kaggle](https://www.kaggle.com/). As informações contidas no banco são do programa de retenção de 7044 clientes de telecomunicações contendo 21 variáveis descritas a seguir:
  
- Se deixou de ser cliente
- Serviços aderidos por cada cada cliente mobile  
    + qtd. de linhas  
    + internet  
    + segurança online  
    + backup online  
    + seguro  
    + suporte técnico  
    + TV a cabo  

- Informações da conta do cliente   
    +  Tempo da conta  
    +  Contrato  
    +  Método de pagamento  
    +  Conta online (sem impressão)  
    +  Encargos mensais  
    +  Total da conta  

* Informações Demográficas do cliente  
    + Gênero  
    + Faixa etária  
    + Possui parceiros ou dependentes  

*Os dados em questão são públicos e disponíveis para download clicando [AQUI](https://www.kaggle.com/blastchar/telco-customer-churn/home).* 
  
### Leitura
  
Lendo o BD e verificando pacotes

```r
db = read.csv2("D:\\2018-2\\IC\\PROJETOS\\PROJ1\\CLUSTER\\DATASET\\WA_Fn-UseC_-Telco-Customer-Churn.csv", sep = ";", header = TRUE)
#class(db)
```

### Estrutura


```r
db$customerID =  as.character(db$customerID)
db$tenure =  as.numeric(db$tenure)
db$MonthlyCharges =  as.numeric(db$MonthlyCharges)
db$TotalCharges =  as.numeric(db$TotalCharges)
db$SeniorCitizen =  factor(db$SeniorCitizen)
str(db)
```

```
## 'data.frame':	7043 obs. of  21 variables:
##  $ customerID      : chr  "7590-VHVEG" "5575-GNVDE" "3668-QPYBK" "7795-CFOCW" ...
##  $ gender          : Factor w/ 2 levels "Female","Male": 1 2 2 2 1 1 2 1 1 2 ...
##  $ SeniorCitizen   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Partner         : Factor w/ 2 levels "No","Yes": 2 1 1 1 1 1 1 1 2 1 ...
##  $ Dependents      : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 2 1 1 2 ...
##  $ tenure          : num  1 34 2 45 2 8 22 10 28 62 ...
##  $ PhoneService    : Factor w/ 2 levels "No","Yes": 1 2 2 1 2 2 2 1 2 2 ...
##  $ MultipleLines   : Factor w/ 3 levels "No","No phone service",..: 2 1 1 2 1 3 3 2 3 1 ...
##  $ InternetService : Factor w/ 3 levels "DSL","Fiber optic",..: 1 1 1 1 2 2 2 1 2 1 ...
##  $ OnlineSecurity  : Factor w/ 3 levels "No","No internet service",..: 1 3 3 3 1 1 1 3 1 3 ...
##  $ OnlineBackup    : Factor w/ 3 levels "No","No internet service",..: 3 1 3 1 1 1 3 1 1 3 ...
##  $ DeviceProtection: Factor w/ 3 levels "No","No internet service",..: 1 3 1 3 1 3 1 1 3 1 ...
##  $ TechSupport     : Factor w/ 3 levels "No","No internet service",..: 1 1 1 3 1 1 1 1 3 1 ...
##  $ StreamingTV     : Factor w/ 3 levels "No","No internet service",..: 1 1 1 1 1 3 3 1 3 1 ...
##  $ StreamingMovies : Factor w/ 3 levels "No","No internet service",..: 1 1 1 1 1 3 1 1 3 1 ...
##  $ Contract        : Factor w/ 3 levels "Month-to-month",..: 1 2 1 2 1 1 1 1 1 2 ...
##  $ PaperlessBilling: Factor w/ 2 levels "No","Yes": 2 1 2 1 2 2 2 1 2 1 ...
##  $ PaymentMethod   : Factor w/ 4 levels "Bank transfer (automatic)",..: 3 4 4 1 3 3 2 4 3 1 ...
##  $ MonthlyCharges  : num  447 803 741 571 1034 ...
##  $ TotalCharges    : num  2506 1467 158 1401 926 ...
##  $ Churn           : Factor w/ 2 levels "No","Yes": 1 1 2 1 2 2 1 1 2 1 ...
```

O banco não possui dados faltantes.


## Análise Exploratória


```r
summary(db)
```

```
##   customerID           gender     SeniorCitizen Partner    Dependents
##  Length:7043        Female:3488   0:5901        No :3641   No :4933  
##  Class :character   Male  :3555   1:1142        Yes:3402   Yes:2110  
##  Mode  :character                                                    
##                                                                      
##                                                                      
##                                                                      
##      tenure      PhoneService          MultipleLines     InternetService
##  Min.   : 0.00   No : 682     No              :3390   DSL        :2421  
##  1st Qu.: 9.00   Yes:6361     No phone service: 682   Fiber optic:3096  
##  Median :29.00                Yes             :2971   No         :1526  
##  Mean   :32.37                                                          
##  3rd Qu.:55.00                                                          
##  Max.   :72.00                                                          
##              OnlineSecurity              OnlineBackup 
##  No                 :3498   No                 :3088  
##  No internet service:1526   No internet service:1526  
##  Yes                :2019   Yes                :2429  
##                                                       
##                                                       
##                                                       
##             DeviceProtection              TechSupport  
##  No                 :3095    No                 :3473  
##  No internet service:1526    No internet service:1526  
##  Yes                :2422    Yes                :2044  
##                                                        
##                                                        
##                                                        
##               StreamingTV              StreamingMovies
##  No                 :2810   No                 :2785  
##  No internet service:1526   No internet service:1526  
##  Yes                :2707   Yes                :2732  
##                                                       
##                                                       
##                                                       
##            Contract    PaperlessBilling                   PaymentMethod 
##  Month-to-month:3875   No :2872         Bank transfer (automatic):1544  
##  One year      :1473   Yes:4171         Credit card (automatic)  :1522  
##  Two year      :1695                    Electronic check         :2365  
##                                         Mailed check             :1612  
##                                                                         
##                                                                         
##  MonthlyCharges    TotalCharges  Churn     
##  Min.   :   1.0   Min.   :   1   No :5174  
##  1st Qu.: 339.0   1st Qu.:1610   Yes:1869  
##  Median : 762.0   Median :3250             
##  Mean   : 784.2   Mean   :3259             
##  3rd Qu.:1205.0   3rd Qu.:4902             
##  Max.   :1585.0   Max.   :6531
```
![Caption for the picture.](D:\\2018-2\\IC\\PROJETOS\\PROJ1\\CLUSTER\\Imagem1.png)

### Gráficos


Matriz de Correlação

```r
correlationM = db %>% select(tenure, MonthlyCharges, TotalCharges)
hchart(cor(correlationM))
```

```
## Warning: package 'bindrcpp' was built under R version 3.5.1
```

<!--html_preserve--><div id="htmlwidget-22cd3cc05ccb0c195093" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-22cd3cc05ccb0c195093">{"x":{"hc_opts":{"title":{"text":null},"yAxis":{"title":{"text":""},"categories":["tenure","MonthlyCharges","TotalCharges"],"reversed":true},"credits":{"enabled":false},"exporting":{"enabled":false},"plotOptions":{"series":{"turboThreshold":0,"showInLegend":false,"boderWidth":0,"dataLabels":{"enabled":false}},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10,"formatter":"function(){\n                 return this.point.name + ': ' +\n                   Highcharts.numberFormat(this.point.value, 2)\n               }"},"series":[{"data":[{"x":0,"y":0,"value":1,"name":"tenure ~ tenure"},{"x":0,"y":1,"value":-0.0771503020730591,"name":"tenure ~ MonthlyCharges"},{"x":0,"y":2,"value":0.158522960900223,"name":"tenure ~ TotalCharges"},{"x":1,"y":0,"value":-0.0771503020730591,"name":"MonthlyCharges ~ tenure"},{"x":1,"y":1,"value":1,"name":"MonthlyCharges ~ MonthlyCharges"},{"x":1,"y":2,"value":-0.008627716395838,"name":"MonthlyCharges ~ TotalCharges"},{"x":2,"y":0,"value":0.158522960900223,"name":"TotalCharges ~ tenure"},{"x":2,"y":1,"value":-0.008627716395838,"name":"TotalCharges ~ MonthlyCharges"},{"x":2,"y":2,"value":1,"name":"TotalCharges ~ TotalCharges"}],"type":"heatmap"}],"legend":{"enabled":true},"colorAxis":{"auxarg":true,"stops":[[0,"#FF5733"],[0.5,"#F8F5F5"],[1,"#2E86C1"]],"min":-1,"max":1},"xAxis":{"categories":["tenure","MonthlyCharges","TotalCharges"],"title":{"text":""},"opposite":true}},"theme":{"chart":{"backgroundColor":"transparent"}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script><!--/html_preserve-->


```r
hcboxplot(x = db$tenure, var = db$Churn, var2 = db$gender,
          outliers = FALSE) %>% 
  hc_title(text = "Boxplot do tempo de fidelização do cliente X Sexo X Desistência") %>% 
  hc_chart(type = "column") %>% # to put box vertical 
  hc_exporting(enabled = TRUE)  # enable exporting option
```

<!--html_preserve--><div id="htmlwidget-7831e77660673bd98070" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-7831e77660673bd98070">{"x":{"hc_opts":{"title":{"text":"Boxplot do tempo de fidelização do cliente X Sexo X Desistência"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"plotOptions":{"series":{"turboThreshold":0,"marker":{"symbol":"circle"}},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10},"chart":{"type":"column"},"xAxis":{"type":"category"},"series":[{"name":"Female","data":[{"name":"No","low":0,"q1":15,"median":39,"q3":61,"high":72},{"name":"Yes","low":1,"q1":2,"median":9,"q3":27.5,"high":65}],"type":"boxplot","id":"Female"},{"name":"Male","data":[{"name":"No","low":0,"q1":15,"median":37,"q3":61,"high":72},{"name":"Yes","low":1,"q1":2,"median":10,"q3":31,"high":72}],"type":"boxplot","id":"Male"}]},"theme":{"chart":{"backgroundColor":"transparent"}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
hcboxplot(x = db$tenure, var = db$Churn, var2 = db$InternetService,
          outliers = FALSE) %>% 
  hc_title(text = "Boxplot do tempo de fidelização do cliente X Serviços de internet X Desistência") %>% 
  hc_chart(type = "column") %>% # to put box vertical 
  hc_exporting(enabled = TRUE)  # enable exporting option
```

<!--html_preserve--><div id="htmlwidget-f345a896d026a6113f17" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-f345a896d026a6113f17">{"x":{"hc_opts":{"title":{"text":"Boxplot do tempo de fidelização do cliente X Serviços de internet X Desistência"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"plotOptions":{"series":{"turboThreshold":0,"marker":{"symbol":"circle"}},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10},"chart":{"type":"column"},"xAxis":{"type":"category"},"series":[{"name":"DSL","data":[{"name":"No","low":0,"q1":14,"median":37,"q3":60,"high":72},{"name":"Yes","low":1,"q1":1,"median":5,"q3":21,"high":51}],"type":"boxplot","id":"DSL"},{"name":"Fiber optic","data":[{"name":"No","low":1,"q1":22,"median":45,"q3":64,"high":72},{"name":"Yes","low":1,"q1":3,"median":13,"q3":33,"high":72}],"type":"boxplot","id":"Fiber optic"},{"name":"No","data":[{"name":"No","low":0,"q1":10,"median":28,"q3":54,"high":72},{"name":"Yes","low":1,"q1":1,"median":1,"q3":9,"high":20}],"type":"boxplot","id":"No"}]},"theme":{"chart":{"backgroundColor":"transparent"}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
pe <- db[,c("gender","MonthlyCharges","tenure","TotalCharges")]
#names(db)
#library(HardyWeinberg)
#e <- MakeFactor(pe,c("PaymentMethod"))
#pe <- makeFactor(pe,c("Churn"))
pe <- discretize(pe,nlevels = 5) 
discparcoord(pe,k=10)
```

<!--html_preserve--><div id="htmlwidget-c1b688ed6a52feeecac5" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-c1b688ed6a52feeecac5">{"x":{"visdat":{"290028b61934":["function () ","plotlyVisDat"]},"cur_data":"290028b61934","attrs":{"290028b61934":{"line":{"color":[198,190,184,183,177,145,142,136,135,124],"colorscale":"Jet","showscale":true,"reversescale":true,"cmin":124,"cmax":198},"dimensions":[{"range":[1,2],"constraintrange":[1,2],"label":"gender","values":[2,1,2,1,2,2,2,1,1,1],"tickmode":"array","tickvals":[1,2],"ticktext":["  Female  ","  Male  "]},{"range":[1,2],"constraintrange":[1,2],"label":"MonthlyCharges","values":[2,2,1,1,2,2,2,2,2,2],"tickmode":"array","tickvals":[1,2],"ticktext":["     1  ","  1285  "]},{"range":[1,3],"constraintrange":[1,3],"label":"tenure","values":[3,3,3,3,2,1,3,1,2,3],"tickmode":"array","tickvals":[1,2,3],"ticktext":["  20  ","  40  ","  60  "]},{"range":[1,4],"constraintrange":[1,4],"label":"TotalCharges","values":[4,4,4,4,2,1,3,1,2,3],"tickmode":"array","tickvals":[1,2,3,4],"ticktext":["  1341  ","  2573  ","  3907  ","  5249  "]}],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"parcoords"}},"layout":{"margin":{"b":40,"l":60,"r":10},"title":"Parcoords","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"line":{"color":[198,190,184,183,177,145,142,136,135,124],"colorscale":"Jet","showscale":true,"reversescale":true,"cmin":124,"cmax":198},"dimensions":[{"range":[1,2],"constraintrange":[1,2],"label":"gender","values":[2,1,2,1,2,2,2,1,1,1],"tickmode":"array","tickvals":[1,2],"ticktext":["  Female  ","  Male  "]},{"range":[1,2],"constraintrange":[1,2],"label":"MonthlyCharges","values":[2,2,1,1,2,2,2,2,2,2],"tickmode":"array","tickvals":[1,2],"ticktext":["     1  ","  1285  "]},{"range":[1,3],"constraintrange":[1,3],"label":"tenure","values":[3,3,3,3,2,1,3,1,2,3],"tickmode":"array","tickvals":[1,2,3],"ticktext":["  20  ","  40  ","  60  "]},{"range":[1,4],"constraintrange":[1,4],"label":"TotalCharges","values":[4,4,4,4,2,1,3,1,2,3],"tickmode":"array","tickvals":[1,2,3,4],"ticktext":["  1341  ","  2573  ","  3907  ","  5249  "]}],"type":"parcoords","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->



