data("favorite_bars")
data("favorite_pies")

highchart() %>% 
  # Data
  hc_add_series(favorite_pies, "column", hcaes(x = pie, y = percent), name = "Pie") %>%
  hc_add_series(favorite_bars, "pie", hcaes(name = bar, y = percent), name = "Bars") %>%
  # Optiosn for each type of series
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%"
    ),
    column = list(
      colorByPoint = TRUE
    ),
    pie = list(
      colorByPoint = TRUE, center = c('30%', '10%'),
      size = 120, dataLabels = list(enabled = FALSE)
    )) %>%
  # Axis
  hc_yAxis(
    title = list(text = "percentage of tastiness"),
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
