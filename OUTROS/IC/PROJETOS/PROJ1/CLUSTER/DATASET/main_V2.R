---
  title: "Clusteriza��o de clientes propen�os"
subtitle: "R Notebook "
author: "Ewerson C. Pimenta e Marcos Ant�nio E. de Oliveira"
date: "`r format(Sys.time(), 'Rio de Janeiro, %d de %B de %Y')`"
output:
  html_notebook:
  highlight: tango
mathjax: null
number_sections: no
theme: lumen
toc: yes
toc_float: yes
code: hide
---
  
  
  
  <!-- highlight: tango pygments kate monochrome zenburn haddock textmate -->
  <!-- theme: cerulean, journal, flatly, readable, spacelab, united, cosmo, lumen, paper, sandston, simplex, yeti -->
  
  <!----------  IN�CIO CONFIG  ---------->
  
  ```{r INSTALPACK, echo = FALSE, eval = TRUE, message=FALSE, include = FALSE}
list.of.packages <- c("arules","dplyr","HardyWeinberg","cdparcoord", "knitr", "knitLatex", "kableExtra", "tidyverse", "tidyverse", "cdparcoord")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

```{r READPACK, echo = FALSE, eval = TRUE, message=FALSE, include = FALSE}
# install.packages("devtools")
#devtools::install_github("hadley/emo")
library(emo)
library(knitr)
library(knitLatex)
library(kableExtra)
library(tidyverse)
library(highcharter)
library(cdparcoord)
```

<!----------  FIM CONFIG  ---------->
  
  <!--
  ## �ndice
  
  + Banco de Dados
+ An�lise Explorat�ria
+ An�lise de Cluster

-->
  
  ## �ndice
  
  #  {.tabset .tabset-fade .tabset-pills}
  ## Banco de Dados
  
  
  ### Descri��o
  
  O conjunto de dados utilizado foi o de t�tulo *Telco Customer Churn* extra�do da plataforma [kaggle](https://www.kaggle.com/). As informa��es contidas no banco s�o do programa de reten��o de 7044 clientes de telecomunica��es contendo 21 vari�veis descritas a seguir:
  
  - Se deixou de ser cliente
- Servi�os aderidos por cada cada cliente mobile  
+ qtd. de linhas  
+ internet  
+ seguran�a online  
+ backup online  
+ seguro  
+ suporte t�cnico  
+ TV a cabo  

- Informa��es da conta do cliente   
+  Tempo da conta  
+  Contrato  
+  M�todo de pagamento  
+  Conta online (sem impress�o)  
+  Encargos mensais  
+  Total da conta  

* Informa��es Demogr�ficas do cliente  
+ G�nero  
+ Faixa et�ria  
+ Possui parceiros ou dependentes  

*Os dados em quest�o s�o p�blicos e dispon�veis para download clicando [AQUI](https://www.kaggle.com/blastchar/telco-customer-churn/home).* 
  
  ### Leitura
  
  Lendo o BD e verificando pacotes
```{r}
getwd()
db = read.csv2("DATASET/WA_Fn-UseC_-Telco-Customer-Churn.csv", sep = ";", header = TRUE)
#class(db)
```

### Estrutura

```{r}
db$customerID =  as.character(db$customerID)
db$tenure =  as.numeric(db$tenure)
db$MonthlyCharges =  as.numeric(db$MonthlyCharges)
db$TotalCharges =  as.numeric(db$TotalCharges)
db$SeniorCitizen =  factor(db$SeniorCitizen)
str(db)
```

O banco n�o possui dados faltantes.


## An�lise Explorat�ria

```{r}
summary(db)
```

Boxplot cruzamento de tr�s vari�veis: Sexo X Churn X Tempo de fidelidade
```{r}
hcboxplot(x = db$tenure, var = db$Churn, var2 = db$gender,
          outliers = FALSE) %>% 
  hc_chart(type = "column") # to put box vertical
```
```{r}
pe <- db[,c("gender","MonthlyCharges","tenure","TotalCharges")]
#names(db)
#library(HardyWeinberg)
#e <- MakeFactor(pe,c("PaymentMethod"))
#pe <- makeFactor(pe,c("Churn"))
pe <- discretize(pe,nlevels = 5) 
discparcoord(pe,k=10)
```

```{r}
library(plotly)
db %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~Churn),
          dimensions = list(
            #list(range = c(0,1),
            #     label = 'Gender', values = ~gender),
            #list(range = c(0,1),
            #    label = 'Partner', values = ~Partner),
            list(
              label = 'MonthlyCharges', values = ~MonthlyCharges),
            list(
              label = 'TotalCharges', values = ~tenure)
          )
  )
```


<!--
  Matriz de Correla��o
```{r}
correlationM = db %>% select(tenure, MonthlyCharges, TotalCharges)
hchart(cor(correlationM))
```
-->
  
  ## CLuster
  
  
  > Data Preparation

```{r}
db[] <- lapply(db, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(db, class)
#         a         b
db1 = as.numeric(unlist(db))
db1 = apply(db, as.numeric).
class(db)
mydata <- na.omit(db) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
```

> Partitioning

```{r}
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
```


```{r}
# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
```



> Hierarchical Agglomerative

There are a wide range of hierarchical clustering approaches. I have had good luck with Ward's method described below.
```{r}
# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
```


The pvclust( ) function in the pvclust package provides p-values for hierarchical clustering based on multiscale bootstrap resampling. Clusters that are highly supported by the data will have large p values. Interpretation details are provided Suzuki. Be aware that pvclust clusters columns, not rows. Transpose your data before using.
```{r}
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)
```

Model Based
```{r}
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model
```

Usando `MASS:::parcoord()` 
```{r}
require(MASS)
cols = c('green', 'red')
parcoord(iris[ ,-5], col = cols[iris$Species])
parcoord(cbind(db$tenure, db$MonthlyCharges, db$TotalCharge), col = cols[db$Churn])
```
Parti��o dos dados
```{r}
set.seed(1234)
ind <- sample(2,nrow(db), replace = T, prob = c(.7, .3))
treino <- db[ind==1, 1:20]
test <- db[ind==2, 1:20]
treino_Churn <- db[ind==1, 21]
test_Churn <- db[ind==2, 21]
```

base Teste
```{r}
dbT = cbind(test,Churn = test_Churn)
```

Labels
```{r}
library(keras)
# Os argumentos (variaveis) devem ser numericos e n�o factors as.numeric()
treinoLabels <- to_categorical(treino_Churn)
testLabels <-  to_categorical(test_Churn)
print(testLabels)
```


Usando `ggplot2` 
```{r}
require(ggplot2)
require(reshape2)
iris$ID <- 1:nrow(iris)
iris_m <- melt(iris, id.vars=c('Species', 'ID'))
ggplot(iris_m) + 
geom_line(aes(x = variable, y = value, group = ID, color = Species))
db_m <- melt(dbT, id.vars = c('Churn', 'customerID'))
ggplot(db_m) + 
geom_line(aes(x = variable, y = value, group = customerID, color = Churn))
```


```{r}
library(rggobi)
mydata <- read.table("E:/Thesis/Experiments/R/input.cvs",header = TRUE,sep = ",")
g <- ggobi(mydata)
```



### End tabset