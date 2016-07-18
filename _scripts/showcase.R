#' ---
#' layout: post
#' ---
#+echo=FALSE
rm(list = ls())
library("highcharter")
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
options(highcharter.theme = NULL)
# printhc <- function(x) {
#   try(hc <- readRDS(sprintf("_showcase/%s.rds", x)))
#   try(hc <- readRDS(sprintf("../_showcase/%s.rds", x)))
#   hc
# }

#'
#' ## Showcase
#' 
#' <div id ="toc"></div>
#'

#'
#' ### The Impact of Vaccines
#' 
#' From [WSJ graphic: Battling Infectious Diseases in the 20th Century](http://graphics.wsj.com/infectious-diseases-and-vaccines/): 
#' 
data("vaccines")
library("viridis")

fntltp <- JS("function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
  Highcharts.numberFormat(this.point.value, 2);
}")

stpscol <- color_stops(10, rev(viridis(10)))

plotline <- list(
  color = "#fde725", value = 1963, width = 2, zIndex = 5,
  label = list(
    text = "Vaccine Intoduced", verticalAlign = "top",
    style = list(color = "#606060"), textAlign = "left",
    rotation = 0, y = -5)
)

thm <- hc_theme_smpl(
  yAxis = list(
    offset = -20,
    tickLength =  0,
    gridLineWidth = 0,
    minorGridLineWidth = 0,
    labels = list(style = list(fontSize = "8px"))
  )
)

hchart(vaccines, "heatmap", x = year, y = state, value = count) %>% 
  hc_colorAxis(stops = stpscol, type = "logarithmic") %>% 
  hc_yAxis(reversed = TRUE) %>% 
  hc_tooltip(formatter = fntltp) %>% 
  hc_xAxis(plotLines = list(plotline)) %>%
  hc_title(text = "Infectious Diseases and Vaccines") %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0) %>% 
  hc_add_theme(thm) %>% 
  hc_size(height = 800)

#'
#' ### Weathers Radials
#' 
#' From [here](http://bl.ocks.org/bricedev/458a01917183d98dff3c) and 
#' replicated in http://jkunst.com/r/how-to-weather-radials/
#'       
data("weather")
x <- c("Min", "Mean", "Max")
y <- sprintf("{point.%s}",
             c("min_temperaturec", "mean_temperaturec", "max_temperaturec"))
tltip <- tooltip_table(x, y)

hchart(weather, type = "columnrange",
       x = date,
       low = min_temperaturec,
       high = max_temperaturec,
       color = mean_temperaturec) %>% 
  hc_chart(polar = TRUE) %>%
  hc_yAxis(
    max = 30,
    min = -10,
    labels = list(format = "{value} C"),
    showFirstLabel = FALSE
  ) %>% 
  hc_xAxis(
    title = list(text = ""),
    gridLineWidth = 0.5,
    labels = list(format = "{value: %b}")
  ) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")),
    pointFormat = tltip
  )


#'
#' ### Global temperatures
#'
data("globaltemp")

thm <- hc_theme_darkunica(
  chart  = list(
    style = list(fontFamily = "Roboto Condensed"),
    backgroundColor = "#323331"
  ),
  yAxis = list(
    gridLineColor = "#B71C1C",
    labels = list(format = "{value} C", useHTML = TRUE)
  ),
  plotOptions = list(series = list(showInLegend = FALSE))
)

x <- c("Min", "Median", "Max")
y <- sprintf("{point.%s}", c("lower", "median", "upper"))
tltip <- tooltip_table(x, y)

hchart(globaltemp, type = "columnrange", x = year, low = lower, high = upper,
       color = median) %>% 
  hc_yAxis(tickPositions = c(-2, 0, 1.5, 2)) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x:%Y}")),
    pointFormat = tltip
  ) %>% 
  hc_plotOptions(
    columnrange = list(borderColor = "transparent")
  ) %>% 
  hc_add_theme(thm)


