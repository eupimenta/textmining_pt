---
title: "Programa de retenção de clientes de telecomunicações"
subtitle: "R Notebook"
author: "Ewerson C. Pimenta e Marcos Antônio E. de Oliveira"
date: "Rio de Janeiro, 21 de agosto de 2018"
output:
  html_document:
    fig_caption: yes
    df_print: paged
    code_folding: hide
    mathjax: null
    pandoc_args: ["-V", "classoption=twocolumn"]
    highlight: tango
    keep_md: yes
    number_sections: no
    theme: lumen
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: yes
      toc_depth: 7
  pdf_document:
    fig_caption: yes
    number_sections: yes
    toc: yes
  md_document:
    variant: markdown_github
csl: apa.csl
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




|![](E:/2018-2/IC/PROJETOS/PROJ1/CLUSTER/IMG/xivqxtyhbvapmpjxyuhh.jpg)|
------------------------------------

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

![](teste1_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
