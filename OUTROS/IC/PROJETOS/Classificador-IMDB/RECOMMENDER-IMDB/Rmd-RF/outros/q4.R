data("favorite_bars")
data("favorite_pies")

db_miss = missing_data
favorite_bars = db_miss
favorite_pies = db_miss

hc_add_series(data = abs(rnorm(5)), type = "column")

highchart() %>% 
  # Data
  hc_add_series(db_miss, "bar", 
                hcaes(name = db_miss$variables, y = db_miss$percent_missing*100), name = "Bars") %>%
  hc_add_series()
  # Optiosn for each type of series
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%"
    ),
    bar = list(
      colorByPoint = TRUE
    )) %>%
  # Axis
  hc_yAxis(
    title = list(text = "porcentagem de missing"),
    labels = list(format = "{value}%"), max = 100
  ) %>% 
  hc_xAxis(categories = favorite_pies$pie) %>%
  # Titles and credits
  hc_title(
    text = "This is a bar graph describing my favorite pies
    including a pie chart describing my favorite bars"
  ) %>%
  hc_subtitle(text = "In percentage of tastiness and awesomeness") %>% 
  hc_credits(
    enabled = TRUE, text = "Source: HIMYM",
    href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
    style = list(fontSize = "12px")
  )
