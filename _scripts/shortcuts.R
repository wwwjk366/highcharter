#' ---
#' layout: post
#' ---
#+echo=FALSE
rm(list = ls())
library("highcharter")
options(download.file.method = "libcurl")
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

#'
#' ## Shortcuts for add data from R objects
#' 
#' <div id ="toc"></div>
#' 
#' There are some functions to add data from differents
#' objects like data frames, lists, vectors, time series, treemaps,
#' geojson, or make special charts like boxplots. 
#' 
#' 

#' ### Data frame
#' 
#' With `hc_add_series_df` the data frame is *kind of* ggplot geoms. You
#' need to specify the type (scatter, line) and the variables for each
#' aesthetic. 
#' 
library("dplyr")
library("broom")

data(mpg, package = "ggplot2")

fit <- loess(displ ~ hwy, data = mpg) %>% 
  augment() %>% 
  arrange(hwy)

highchart() %>% 
  hc_title(text = "Package Broom rocks!") %>% 
  hc_add_series_df(mpg, type = "scatter", x = hwy, y = displ, group = class) %>%
  hc_add_series_df(fit, type = "arearange",
                   x = hwy,
                   low = .fitted - .se.fit,
                   high = .fitted + .se.fit)


#' ### List of series
#' 
#' Sometimes you create a list of data where each
#' element is a data series (a list with data values and name). So,
#' for dont repeat `hc_add_series` you can use `hc_add_series_list`
#' 
ds <- lapply(seq(10), function(x){
  list(data = cumsum(rnorm(50, 1, 5)), name = x)
})

highchart() %>%
  hc_plotOptions(series = list(showInLegend = FALSE,marker = list(enabled = FALSE))) %>%
  hc_add_series_list(ds)

#' 
#' ### From `ts` objects
#'
#' There's a `hc_add_series_ts` which recieve a `ts` object.
#' and we can enable highstock instead of highcharts.

highchart(type = "stock") %>% 
  hc_title(text = "Monthly Deaths from Lung Diseases in the UK") %>% 
  hc_subtitle(text = "Deaths from bronchitis, emphysema and asthma") %>% 
  hc_add_series_ts(fdeaths, name = "Female") %>%
  hc_add_series_ts(mdeaths, name = "Male")

#' 
#' ### From `xts` objects
#' 
#' Support `ohlc` series  from the  `quantmod` package.
#' You can also add markes or flag with `hc_add_series_flags`: 

library("quantmod")
usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
eurkpw <- getSymbols("EUR/KPW", src = "oanda", auto.assign = FALSE)

dates <- as.Date(c("2015-05-08", "2015-09-12"), format = "%Y-%m-%d")

highchart(type = "stock") %>% 
  hc_add_series_xts(usdjpy, id = "usdjpy") %>% 
  hc_add_series_xts(eurkpw, id = "eurkpw") %>% 
  hc_add_series_flags(dates, title = c("E1", "E2"), 
                      text = c("This is event 1",
                               "This is the event 2"),
                      id = "usdjpy") %>% 
  hc_rangeSelector(inputEnabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) 


#' 
#' ### Treemaps
#'
#' Here we use the `treemap` package to create a treemap object and then
#' we create the same treemap via highcharts ;).
#+fig.keep="none"
library("treemap")
library("viridisLite")

data(GNI2014)

tm <- treemap(GNI2014, index = c("continent", "iso3"),
              vSize = "population", vColor = "GNI",
              type = "value", palette = viridis(6))

hc_tm <- highchart() %>% 
  hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified",
                        name = "tmdata") %>% 
  hc_title(text = "Gross National Income World Data") %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
             Pop: {point.value:,.0f}<br>
             GNI: {point.valuecolor:,.0f}")

hc_tm

#' 
#' ### Labels & Values
#' 
#' From labels a numeric values
#' 

data("favorite_bars")
data("favorite_pies")

highchart() %>% 
  hc_title(text = "This is a bar graph describing my favorite pies
           including a pie chart describing my favorite bars") %>%
  hc_subtitle(text = "In percentage of tastiness and awesomeness") %>% 
  hc_add_series_labels_values(favorite_pies$pie, favorite_pies$percent, name = "Pie",
                              colorByPoint = TRUE, type = "column") %>% 
  hc_add_series_labels_values(favorite_bars$bar, favorite_bars$percent,
                              colors = substr(terrain.colors(5), 0 , 7), type = "pie",
                              name = "Bar", colorByPoint = TRUE, center = c('35%', '10%'),
                              size = 100, dataLabels = list(enabled = FALSE)) %>% 
  hc_yAxis(title = list(text = "percentage of tastiness"),
           labels = list(format = "{value}%"), max = 100) %>% 
  hc_xAxis(categories = favorite_pies$pie) %>% 
  hc_legend(enabled = FALSE) %>% 
  hc_tooltip(pointFormat = "{point.y}%") %>% 
  hc_credits(enabled = TRUE, text = "Source: HIMYM",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

#' ### Boxplot
#' 
data(diamonds, package = "ggplot2")

highchart() %>% 
  hc_add_series_boxplot(diamonds$x, diamonds$color,
                        name = "X", color = "#2980b9") 

#' 
#' ### Maps & geojson
#' 
#' You use geo-data. You can chart them too!
#' 

data(worldgeojson)
data(GNI2014, package = "treemap")

dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)), c = substring(viridis(5 + 1), 0, 7)) %>% 
  list_parse2()

highchart() %>% 
  hc_title(text = "Charting GNI data") %>% 
  hc_add_series_map(worldgeojson, GNI2014,
                    value = "GNI", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops)

