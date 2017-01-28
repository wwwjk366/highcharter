#' ---
#' layout: post
#' toc: true
#' ---
#+echo=FALSE
rm(list = ls())
library(highcharter)
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

#'
#' ## Highcharts
#' 
#' <div id ="toc"></div>
#' 
#' ### Some flexibility
#' 
#' Same data a lot of charts!
#' 
library(dplyr)
library(stringr)
library(purrr)

n <- 5

set.seed(123)

colors <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d")
colors2 <- c("#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF")


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
    color = rep(colors, length.out = n),
    segmentColor = rep(colors2, length.out = n)
  )

df

create_hc <- function(t) {
  
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

#+echo=FALSE
hcs %>% 
  map(hc_size, height = 300) %>% 
  map(tags$div, class = "col-sm-4") %>% 
  tags$div(class = "row")


#'
#'
#' ### HIMYM example
#' 
#' > _This is a bar graph describing my favorite pies including a pie chart describing my favorite bars._
#'
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


#'
#' ### Chart Color Gradient
#'
#' Idea from http://jsfiddle.net/bilelz/sxy1cwnv/
#' 

highchart() %>% 
  hc_chart(backgroundColor = "#") %>% 
  hc_title(text = "Chart color gradient it's on fire", style = list(color = "#CCC")) %>% 
  # hc_xAxis(categories = month.abb) %>% 
  hc_yAxis(labels = list(style = list(color = "#CCC")),
           gridLineColor = "#111111") %>% 
  hc_series(
    list(
      data =  abs(rnorm(100)) + 1,
      type = "areaspline",
      marker = list(enabled = FALSE),
      color =  list(
        linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
        stops = list(
          list(0, "transparent"),
          list(0.33, "yellow"),
          list(0.66, "red"),
          list(1, "#ccc")
        )
      ),
      fillColor = list(
        linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
        stops = list(
          list(0, "transparent"),
          list(0.1, "yellow"),
          list(0.5, "red"),
          list(1, "black")
          )
        )
      )
    ) %>% 
  hc_add_theme(hc_theme_null())



#+echo=FALSE

# 
# rainfall <- c(49.9, 71.5, 106.4, 129.2, 144, 176,
#               135.6, 148.5, 216.4, 194.1, 95.6, 54.4)
# 
# temperature <- c(7, 6.9, 9.5, 14.5, 18.2, 21.5,
#                  25.2, 26.5, 23.3, 18.3, 13.9, 9.6)
# 
# col1 <- "#90ED7D"
# col2 <- "#5C5C61"
# 
# highchart() %>%
#   hc_title(text = "Tokyo Climate") %>%
#   hc_legend(enabled = FALSE) %>%
#   hc_xAxis(categories = month.abb) %>%
#   hc_yAxis_multiples(
#     list(
#       title = list(text = "Temperature"),
#       align = "left",
#       showFirstLabel = FALSE,
#       showLastLabel = FALSE,
#       labels = list(format = "{value} &#176;C", useHTML = TRUE)
#     ),
#     list(
#       title = list(text = "Rainfall"),
#       align = "right",
#       showFirstLabel = FALSE,
#       showLastLabel = FALSE,
#       labels = list(format = "{value} mm"),
#       opposite = TRUE
#     )
#   ) %>%
#   hc_tooltip(formatter = JS("function(){
#                             if('Sunshine' == this.series.name){
#                             return  '<b>' + this.point.name + ': </b>' + this.y
#                             } else {
#                             unts = this.series.name == 'Rainfall' ? 'mm' : '&#176;C';
#                             return (this.x + ': ' + this.y + ' ' + unts)
#                             }}"),
#              useHTML = TRUE) %>%
#   hc_add_series(name = "Rainfall", type = "column", data = rainfall, yAxis = 1) %>%
#   hc_add_series(name = "Temperature", type = "spline", data = temperature) %>%
#   hc_add_series(name = "Sunshine", type = "pie",
#                 data = list(list(y = 2020, name = "Sunshine hours",
#                                  sliced = TRUE, color = col1),
#                             list(y = 6740, name = "Non sunshine hours (including night)",
#                                  color = col2,
#                                  dataLabels = list(enabled = FALSE))),
#                 center = c('20%', 45),
#                 size = 80)
# #' ### A spiderweb chart 
# #'
# #' Highcharts support a range of different chart types so data 
# #' can be displayed in a meaningfull way. Highcharter inherit
# #' all this awesome features. Here's is an example how make a 
# #' spiderweb chart^[Example proposed by
# #' @johnstantongeddes (https://github.com/johnstantongeddes)].
# #' 
# 
# highchart() %>% 
#   hc_chart(polar = TRUE, type = "line") %>% 
#   hc_title(text = "Budget vs Spending") %>% 
#   hc_xAxis(categories = c('Sales', 'Marketing', 'Development', 'Customer Support', 
#                           'Information Technology', 'Administration'),
#            tickmarkPlacement = 'on',
#            lineWidth = 0) %>% 
#   hc_yAxis(gridLineInterpolation = 'polygon',
#            lineWidth = 0,
#            min = 0) %>% 
#   hc_series(
#     list(
#       name = "Allocated Budget",
#       data = c(43000, 19000, 60000, 35000, 17000, 10000),
#       pointPlacement = 'on'
#     ),
#     list(
#       name = "Actual Spending",
#       data = c(50000, 39000, 42000, 31000, 26000, 14000),
#       pointPlacement = 'on'
#     )
#   )
#
# #' ### Gauges like Apple Watch
# #' 
# #' http://www.highcharts.com/blog/206-activity-gauges-like-apple-watch
# #' 
# highchart(width = 400, height = 400) %>% 
#   hc_chart(
#     type = "solidgauge",
#     backgroundColor = "#F0F0F0",
#     marginTop = 50
#   ) %>% 
#   hc_title(
#     text = "Activity",
#     style = list(
#       fontSize = "24px"
#     )
#   ) %>% 
#   hc_tooltip(
#     borderWidth = 0,
#     backgroundColor = 'none',
#     shadow = FALSE,
#     style = list(
#       fontSize = '16px'
#     ),
#     pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
#     positioner = JS("function (labelWidth, labelHeight) {
#                     return {
#                     x: 200 - labelWidth / 2,
#                     y: 180
#                     };
#                     }")
#     ) %>% 
#   hc_pane(
#     startAngle = 0,
#     endAngle = 360,
#     background = list(
#       list(
#         outerRadius = '112%',
#         innerRadius = '88%',
#         backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),
#         borderWidth =  0
#       ),
#       list(
#         outerRadius = '87%',
#         innerRadius = '63%',
#         backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),
#         borderWidth = 0
#       ),
#       list(
#         outerRadius = '62%',
#         innerRadius =  '38%',
#         backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),
#         borderWidth = 0
#       )
#     )
#   ) %>% 
#   hc_yAxis(
#     min = 0,
#     max = 100,
#     lineWidth = 0,
#     tickPositions = list()
#   ) %>% 
#   hc_plotOptions(
#     solidgauge = list(
#       borderWidth = '34px',
#       dataLabels = list(
#         enabled = FALSE
#       ),
#       linecap = 'round',
#       stickyTracking = FALSE
#     )
#   ) %>% 
#   hc_add_series(
#     name = "Move",
#     borderColor = JS("Highcharts.getOptions().colors[0]"),
#     data = list(list(
#       color = JS("Highcharts.getOptions().colors[0]"),
#       radius = "100%",
#       innerRadius = "100%",
#       y = 80
#     ))
#   ) %>% 
#   hc_add_series(
#     name = "Exercise",
#     borderColor = JS("Highcharts.getOptions().colors[1]"),
#     data = list(list(
#       color = JS("Highcharts.getOptions().colors[1]"),
#       radius = "75%",
#       innerRadius = "75%",
#       y = 65
#     ))
#   ) %>% 
#   hc_add_series(
#     name = "Stand",
#     borderColor = JS("Highcharts.getOptions().colors[2]"),
#     data = list(list(
#       color = JS("Highcharts.getOptions().colors[2]"),
#       radius = "50%",
#       innerRadius = "50%",
#       y = 50
#     ))
#   )






