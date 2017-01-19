#' ---
#' layout: post
#' ---
#' 
#' 
#' <h1><div id="brand">h|1i|0g|3h|2c|1h|2a|1r|3t|2e|1r|2{rpackage}</div></h1>
#' 
#+echo=FALSE
rm(list = ls())
try(source("_scripts/helpers.R"))
try(source("helpers.R"))
options(download.file.method = "libcurl")
get_demos()
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(highcharter.theme = hc_theme_smpl())

#'
#' Highcharter is a [R](https://cran.r-project.org/) wrapper for **Highcharts** javascript
#' libray and its modules. Highcharts is very mature and flexible javascript charting library and
#'  it has a great and powerful API^[See http://www.highcharts.com/demo].
#' 
#' The main features of this package are:
#' 
#' * Various chart type with the same style: scatters, bubble, line, 
#' time series, heatmaps, treemap, bar charts, networks.
#' * Chart various R object with one function. With hchart(x) you can 
#' chart: data.frames, numeric, histogram, character, density, factors, ts,
#'  mts, xts, stl, ohlc, acf, forecast, mforecast, ets, igraph, dist,
#'   dendrogram, phylo, survfit classes.
#' * Support Highstock charts. You can create a candlestick charts in 2 lines 
#' of code. Support xts objects from the quantmod package.
#' * Support Highmaps charts. It's easy to create choropleths or add 
#' information in geojson format.
#' * Piping styling.
#' * Themes: you configurate your chart in multiples ways. There are
#'  implemented themes like economist, financial times, google, 538 among 
#'  others.
#' * Plugins: motion, drag points, fontawesome, url-pattern, annotations.
#' 
#' 
#' ### Hello World Example 
#' 
#' This is a simple example using `hchart` function.
#' 
library("highcharter")
data(diamonds, mpg, package = "ggplot2")

hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))

#'
#' Or using the highcharts API
#' 

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "A highcharter chart") %>% 
  hc_xAxis(categories = 2012:2016) %>% 
  hc_add_series(data = c(3900,  4200,  5700,  8500, 11900),
                name = "Downloads")


#' 
#' ### Generic Function `hchart`
#' 
#' Among its features highcharter can chart various objects depending of
#' its class with the generic^[I want to say *magic*]  `hchart` function.

hchart(diamonds$cut, colorByPoint = TRUE, name = "Cut")

hchart(diamonds$price, color = "#B71C1C", name = "Price") %>% 
  hc_title(text = "You can zoom me")

#' 
#' One of the nicest class which `hchart` can plot is the `forecast`
#' class from the forecast package.
#' 

library("forecast")

airforecast <- forecast(auto.arima(AirPassengers), level = 95)

hchart(airforecast)


#' 
#' ### Highstock
#' 
#' With highcharter you can use the highstock library which 
#' include sophisticated navigation options like a small navigator 
#' series, preset date ranges, date picker, scrolling and panning.
#' With highcarter it's easy make candlesticks or ohlc charts using
#' time series data. For example data from [quantmod](http://www.quantmod.com/)
#' package.
#' 
library("quantmod")

x <- getSymbols("GOOG", auto.assign = FALSE)
y <- getSymbols("AMZN", auto.assign = FALSE)

highchart(type = "stock") %>% 
  hc_add_series(x) %>% 
  hc_add_series(y, type = "ohlc")

#' 
#' ### Highmaps
#' 
#' You can chart maps and choropleth using the highmaps module.

data(unemployment)

hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


