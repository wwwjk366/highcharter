#' ---
#' layout: post
#' toc: true
#' ---
#+echo=FALSE
rm(list = ls())
library(highcharter)
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
options(download.file.method = "libcurl")
#'
#' ## `hchart` function
#' 
#' <div id ="toc"></div>
#' 
#' This generic function can chart various R objects on the fly.
#' The resulting chart is a highchart object
#' so you can keep modifying with the implmented API
#' 
#' Some implemented classes are: numeric, histogram, character,
#' factor, ts, mts, xts (and OHLC), forecast, acf, dist.
#' 

#' ### Data Frames
#' 
#' This function works like `qplot`: You pass the data, choose the type
#' of chart and then define the aesthetics for each variable.
#' 
data(diamonds, economics_long, mpg, package = "ggplot2")
library(dplyr)

hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))

mpgman2 <- mpg %>% 
  count(class, year) %>% 
  glimpse()

hchart(mpgman2, "column", hcaes(x = class, y = n, group = year))

mpgman3 <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n(), unique = length(unique(model))) %>% 
  arrange(-n, -unique) %>% 
  glimpse()

hchart(mpgman3, "treemap", hcaes(x = manufacturer, value = n, color = unique))

#' 
#' Check automatically if the x column is date class
#' 
economics_long2 <- economics_long %>% 
  filter(variable %in% c("pop", "uempmed", "unemploy")) %>% 
  print()
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable))

#' ### Numeric & Histograms
hchart(diamonds$price) 

#' ### Densities
hchart(density(diamonds$price), type = "area", color = "#B71C1C", name = "Price")

#' ### Character & Factor
hchart(diamonds$cut, type = "column")

#' ### Time Series
hchart(LakeHuron)

#' ### Seasonal Decomposition of Time Series by Loess
x <- stl(log(AirPassengers), "per")
hchart(x)

#' ### Forecast package
library("forecast")

x <- forecast(ets(USAccDeaths), h = 48, level = 95)
hchart(x)

#' ### igraph
library("igraph")
N <- 40

net <- sample_gnp(N, p = 2/N)
wc <- cluster_walktrap(net)

V(net)$label <- seq(N)
V(net)$name <- paste("I'm #", seq(N))
V(net)$page_rank <- round(page.rank(net)$vector, 2)
V(net)$betweenness <- round(betweenness(net), 2)
V(net)$degree <- degree(net)
V(net)$size <- V(net)$degree
V(net)$comm <- membership(wc)
V(net)$color <- colorize(membership(wc))

hchart(net, layout = layout_with_fr)

#' ### `xts` from quantmod package
library(quantmod)

x <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
hchart(x)

#' ### `xts ohlc` objects
x <- getSymbols("YHOO", auto.assign = FALSE)
hchart(x)

#' ### Autocovariance & Autocorrelation
x <- acf(diff(AirPassengers), plot = FALSE)
hchart(x)

#' ### Multivariate Time series
x <- cbind(mdeaths, fdeaths)
hchart(x)

#' ### Survival Models
library(survival)

data(lung)
lung <- mutate(lung, sex = ifelse(sex == 1, "Male", "Female"))
fit <- survfit(Surv(time, status) ~ sex, data = lung) 

hchart(fit, ranges = TRUE)

#' ### Principal Components
hchart(princomp(USArrests, cor = TRUE))

#' ### Matrix
#' 
data(volcano)
hchart(volcano) %>% 
  # changing default color
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))

#' ### Distance matrix 
#' 
mtcars2 <- mtcars[1:20, ]
x <- dist(mtcars2)
hchart(x)

#' ### Correlation matrix
#' 
hchart(cor(mtcars))
