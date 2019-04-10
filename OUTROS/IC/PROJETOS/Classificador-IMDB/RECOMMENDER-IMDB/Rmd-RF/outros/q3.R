library(dplyr)
library(stringr)
library(purrr)

n <- 5

set.seed(123)

colors <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d")
colors2 <- c("#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF")
data.frame(x = seq_len(n) - 1)
seq_len(n) - 1
df <- data.frame(x = seq_len(n) - 1) %>% 
  mutate(
    db
    y = 10 + x + 10 * sin(x),
    y = round(y, 1),
    z = (x*y) - median(x*y),
    e = 10 * abs(rnorm(length(x))) + 2,
    e = round(e, 1),
    low = y - e,
    high = y + e,
    value = y,
    name = sample(fruit[str_length(fruit) <= 5], size = n),
    color = rep(colors, length.out = n),
    segmentColor = rep(colors2, length.out = n)
  )

glimpse(df)
glimpse(missing_data)

##   x    y     z    e  low high value  name   color segmentColor
## 1 0 10.0 -25.6  7.6  2.4 17.6  10.0  plum #d35400      #000004
## 2 1 19.4  -6.2  4.3 15.1 23.7  19.4 lemon #2980b9      #3B0F70
## 3 2 21.1  16.6 17.6  3.5 38.7  21.1 mango #2ecc71      #8C2981
## 4 3 14.4  17.6  2.7 11.7 17.1  14.4  pear #f1c40f      #DE4968
## 5 4  6.4   0.0  3.3  3.1  9.7   6.4 apple #2c3e50      #FE9F6D

create_hc <- function(t) {
  t = df[,c(8,9,10)]
  
  dont_rm_high_and_low <- c("arearange", "areasplinerange",
                            "columnrange", "errorbar")
  
  is_polar <- str_detect(t, "polar")
  
  t <- str_replace(t, "polar", "")
  
  if(!t %in% dont_rm_high_and_low) df <- df %>% select(-e, -low, -high)
  
  
  highchart() %>%
    hc_title(text = paste(ifelse(is_polar, "polar ", ""), t),
             style = list(fontSize = "15px")) %>% 
    hc_chart(type = t,
             polar = is_polar) %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, name = "Fruit Consumption", showInLegend = FALSE) 
  
}

hcs <- c("line", "spline",  "area", "areaspline",
         "column", "bar", "waterfall" , "funnel", "pyramid",
         "pie" , "treemap", "scatter", "bubble",
         "arearange", "areasplinerange", "columnrange", "errorbar",
         "polygon", "polarline", "polarcolumn", "polarcolumnrange",
         "coloredarea", "coloredline")  %>% 
  map(create_hc) 