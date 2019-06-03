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

preserved2c52781f0c3e6af


```r
hcboxplot(x = db$tenure, var = db$Churn, var2 = db$gender,
          outliers = FALSE) %>% 
  hc_title(text = "Boxplot do tempo de fidelização do cliente X Sexo X Desistência") %>% 
  hc_chart(type = "column") %>% # to put box vertical 
  hc_exporting(enabled = TRUE)  # enable exporting option
```

preserve3402a539da1f0fb5


```r
hcboxplot(x = db$tenure, var = db$Churn, var2 = db$InternetService,
          outliers = FALSE) %>% 
  hc_title(text = "Boxplot do tempo de fidelização do cliente X Serviços de internet X Desistência") %>% 
  hc_chart(type = "column") %>% # to put box vertical 
  hc_exporting(enabled = TRUE)  # enable exporting option
```

preservec1b250a3b61901fd


```r
pe <- db[,c("gender","MonthlyCharges","tenure","TotalCharges")]
#names(db)
#library(HardyWeinberg)
#e <- MakeFactor(pe,c("PaymentMethod"))
#pe <- makeFactor(pe,c("Churn"))
pe <- discretize(pe,nlevels = 5) 
discparcoord(pe,k=10)
```

preservee50eaafe6df196ab



