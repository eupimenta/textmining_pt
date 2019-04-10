library(purrr) # map function to make grouped categories argument
library(dplyr) # for select function 

data(mpg, package = "ggplot2")
mpgg <- mpg %>% 
  filter(class %in% c("suv", "compact", "midsize")) %>% 
  group_by(class, manufacturer) %>% 
  dplyr::summarise(count = n())

mpgg <- data.frame(VAR_MISS = rep("VAR_MISS", dim(db_miss)[2]),db_miss)

categories_grouped <- mpgg %>% 
  group_by(name = class) %>% 
  do(categories = .$manufacturer) %>% 
  list_parse()

categories_grouped <- db_miss %>% 
  group_by(name = "VAR_MISS") %>% 
  do(categories = .$variables) %>% 
  list_parse()

highchart() %>% 
  hc_xAxis(categories = categories_grouped) %>% 
  hc_add_series(data = mpgg, type = "bar", hcaes(y = variable, color = percent_missing),
                showInLegend = FALSE)
