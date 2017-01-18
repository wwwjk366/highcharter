library(dplyr)
library(highcharter)
library(stringr)
library(purrr)

options(highcharter.theme = hc_theme_smpl())

n <- 5

set.seed(123)

df <- data.frame(x = seq_len(n) - 1) %>% 
  mutate(
    y = 10 + x + 10 * sin(x),
    y = round(y, 1),
    z = (x*y) - median(x*y),
    e = 10 * abs(rnorm(length(x))) + 2,
    e = round(e, 1),
    low = y - e,
    high = y + e,
    value = y,
    name = sample(fruit[str_length(fruit) <= 5], size = n),
    color = head(getOption("highcharter.theme")$color, n)
  )

df

create_hc <- function(t) {
  
  dont_rm_high_and_low <- c("arearange", "areasplinerange",
                            "columnrange", "errorbar")
  
  is_polar <- str_detect(t, "polar")
  
  t <- str_replace(t, "polar", "")
  
  if(!t %in% dont_rm_high_and_low) df <- df %>% select(-e, -low, -high)
  
  highchart() %>%
    hc_title(text = t, style = list(fontSize = "15px")) %>% 
    hc_chart(type = t, polar = is_polar) %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, name = "Fruit Consumption", showInLegend = FALSE) 

}

c("line", "spline",  "area", "areaspline",
  "column", "bar", "waterfall" , "funnel", "pyramid",
  "pie" , "treemap", "scatter", "bubble",
  "arearange", "areasplinerange", "columnrange", "errorbar",
  "polygon", "polarline", "polarcolumn")  %>% 
  map(create_hc) %>% 
  map(hc_size, height = 300) %>% 
  hw_grid() %>% 
  htmltools::browsable()
