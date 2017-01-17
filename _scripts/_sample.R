library(dplyr)


n <- 5

set.seed(123)

df <- data_frame(
  id = seq_len(n),
  name = sample(fruit, size = n),
  x = id - 1,
  y = 1 + x + sample(x/n*5),
  value = y
)

hc <- highchart() %>%
  hc_xAxis(categories = df$name) %>% 
  hc_add_series(df, name = "Fruit Consumption", showInLegend = FALSE) %>%
  hc_add_theme(hc_theme_smpl())

cbp <- c("line", "spline", "area")

c("line", "column", "waterfall" , "funnel", "pie" , "treemap", "scatter",
  "spline", "area")  %>% 
  map(function(t) {
    hc %>%
      hc_title(text = t) %>% 
      hc_chart(type = t) %>% 
      hc_plotOptions(series = list(colorByPoint = ! t %in% cbp))
  }) %>% 
  hw_grid() %>% 
  htmltools::browsable()


