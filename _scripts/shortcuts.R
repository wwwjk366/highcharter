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
#' There are some functions to generate specific
#' types of chart.
#' 
#' ### Boxplot
#' 
data(diamonds, package = "ggplot2")

hcboxplot(x = diamonds$x, var = diamonds$color,
          name = "Length", color = "#2980b9") 

hcboxplot(x = diamonds$x, var = diamonds$color, var2 = diamonds$cut,
          outliers = FALSE) %>% 
  hc_chart(type = "column") # to put box vertical

#' ### Parallel Coordinates
require(viridisLite)

n <- 15

hcparcords(head(mtcars, n), color = hex_to_rgba(magma(n), 0.5))


#' ### Icon Arrays
#' 
set.seed(123)

icons <- c("motorcycle", "taxi", "bus", "plane")

n <- sample(3:10, length(icons)) %>% 
  sort(decreasing = TRUE) %>% 
  {. *  seq(length(icons), 1) } 

hciconarray(icons, n, icons = icons, size = 5)


#' 
#' ### Treemaps
#'
#' Here we use the `treemap` package to create a treemap object and then
#' we create the same treemap via highcharts ;).
#+fig.keep="none"
library(treemap)
library(viridisLite)

data(GNI2014)

tm <- treemap(GNI2014, index = c("continent", "iso3"),
              vSize = "population", vColor = "GNI",
              type = "value", palette = viridis(6))

hctreemap(tm)

