#' ---
#' layout: post
#' toc: true
#' ---
#+echo=FALSE
rm(list = ls())
library("highcharter")
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(highcharter.theme = hc_theme_smpl())
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

#' ### Numeric & Histograms
data(diamonds, package = "ggplot2")

hchart(diamonds$price) 

#' ### Densities
hchart(density(diamonds$price), area = TRUE, color = "#B71C1C", name = "Price")

#' ### Character & Factor
hchart(diamonds$cut)


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
library("RColorBrewer")

net <- barabasi.game(50)

wc <- cluster_walktrap(net)

V(net)$label <- 1:50
V(net)$page_rank <- round(page.rank(net)$vector, 2)
V(net)$betweenness <- round(betweenness(net), 2)
V(net)$degree <- degree(net)
V(net)$size <- V(net)$degree
V(net)$comm <- membership(wc)
V(net)$color <- brewer.pal(length(unique(membership(wc))), "Accent")[membership(wc)]

hchart(net, layout = layout_with_fr)

#' ### `xts` from quantmod package
#+eval=TRUE
library("quantmod")

x <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)

hchart(x)

#' ### `xts ohlc` objects
#+eval=TRUE 
x <- getSymbols("YHOO", auto.assign = FALSE)

hchart(x)

#' ### Autocovariance & Autocorrelation
x <- acf(diff(AirPassengers), plot = FALSE)

hchart(x)

#' ### Multivariate Time series
x <- cbind(mdeaths, fdeaths)

hchart(x)

#' ### Survival Models
library("survival")
library("dplyr")

data(lung)

lung <- mutate(lung, sex = ifelse(sex == 1, "Male", "Female"))

fit <- survfit(Surv(time, status) ~ sex, data = lung) 

hchart(fit, ranges = TRUE)

#' ### Principal Components
hchart(princomp(USArrests))

#' ### Maxtrix
#' 
data(volcano)

hchart(volcano) %>% 
  hc_add_theme(hc_theme_null()) %>% 
  hc_colorAxis(stops = color_stops())


#' ### Distance matrix 
#' 
mtcars2 <- mtcars[1:20, ]
x <- dist(mtcars2)

hchart(x)

#' ### Phylo
library("ape")

x <- mtcars %>% dist() %>% hclust() %>% as.phylo()

set.seed(10)
hchart(x)

