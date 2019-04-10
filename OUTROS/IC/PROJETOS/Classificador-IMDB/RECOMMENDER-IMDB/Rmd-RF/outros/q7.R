db_miss = missing_data
db_miss$percent_missing = 100*db_miss$percent_missing
db_miss[order(db_miss$percent_missing, decreasing = TRUE), ]


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_yAxis(max = 100, min = 0) %>% 
  hc_add_series(name = "Missing", data = db_miss$percent_missing, zIndex = -10) %>% 
  hc_add_series(name = "B",
                data = list(
                  list(sequence = db_miss$percent_missing,
                  list(sequence = c(3,2,1,3)),
                  list(sequence = c(2,5,4,3))
                )) %>% 
  hc_add_series(name = "C",
                data = list(
                  list(sequence = c(3,2,1,3)),
                  list(sequence = c(2,5,4,3)),
                  list(sequence = c(1,2,3,4))
                )) %>% 
  hc_motion(enabled = TRUE,
            labels = 2000:2003,
            series = c(1,2))

  xy.df <- data.frame(x = runif(10),  y = runif(10))
  
  xy.list <- split(xy.df, seq(nrow(xy.df)))
  
  xy.list <- setNames(split(xy.df, seq(nrow(xy.df))), rownames(xy.df))