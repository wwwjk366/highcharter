library(dplyr)
library(highcharter)
library(stringr)
library(purrr)

options(highcharter.theme = hc_theme_smpl())

n <- 4

set.seed(124)

df <- data.frame(x = seq_len(n) - 1) %>% 
  mutate(
    y = 10 + x + 10 * sin(x),
    y = round(y, 1),
    z = (x*y) - median(x*y),
    e = 10 * abs(rnorm(length(x))) + 2,
    e = round(e, 1),
    low = y - e,
    high = y + e,
    value = x,
    name = sample(fruit, size = n),
    color = head(getOption("highcharter.theme")$color, n)
  )

df

create_hc <- function(t) {
  
  dont_rm_high_and_low <- c("arearange", "areasplinerange",
                            "columnrange", "errorbar")
  
  if(! t %in% dont_rm_high_and_low) df <- df %>% select(-e, -low, -high)
  
  highchart() %>%
    hc_title(text = t) %>% 
    hc_chart(type = t) %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, name = "Fruit Consumption", showInLegend = FALSE) 

}

c("line", "spline",  "area", "areaspline",
  "column", "bar", "waterfall" , "funnel", "pyramid",
  "pie" , "treemap", "scatter", "bubble",
  "arearange", "areasplinerange", "columnrange", "errorbar",
  "polygon")  %>% 
  map(create_hc) %>% 
  hw_grid() %>% 
  htmltools::browsable()
