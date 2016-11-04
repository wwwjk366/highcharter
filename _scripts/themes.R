#' ---
#' layout: post
#' ---
#+echo=FALSE
rm(list = ls())
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library("highcharter")
options(highcharter.theme = NULL)

#'
#' ## Themes
#' 
#' <div id ="toc"></div>
#' 
#' Highcharts is super flexible to add and create themes.
#' In Highcarter there are some predefined themes and some 
#' functions to create or merge themes.
#' 
#' ### Default

hc <- highcharts_demo()

hc

#' ### Fivethirtyeight

hc %>% hc_add_theme(hc_theme_538())

#' ### Economist

hc %>% hc_add_theme(hc_theme_economist())

#' ### Financial Times

hc %>% hc_add_theme(hc_theme_ft())

#' ### Dotabuff
#' 
#' Extracted/inspired from  http://www.dotabuff.com/.

hc %>% hc_add_theme(hc_theme_db())

#' ### Flat
#' 
#' Inspired by https://github.com/cttobin/ggthemr#flat

hc %>% hc_add_theme(hc_theme_flat())

hc %>% hc_add_theme(hc_theme_flatdark())

#' ### Simple
#' 
#' Desing inspired by https://github.com/hrbrmstr/hrbrmisc/blob/master/R/ggplot.r
#'  and color by http://www.materialui.co/flatuicolors

hc %>% hc_add_theme(hc_theme_smpl())

#' ### Elementary
#' 
#' Desing inspired by https://elementary.io

hc %>% hc_add_theme(hc_theme_elementary())

#' ### Google
#' 
#' http://www.google.com/.

hc %>% hc_add_theme(hc_theme_google())

#' ### Firefox
#' 
#' Theme inspired by https://www.mozilla.org/en-US/styleguide/.

hc %>% hc_add_theme(hc_theme_ffx())

#' ### Monokai
#' 
#' A well know text editor theme.

hc %>% hc_add_theme(hc_theme_monokai())

#' ### Tufte
#' 
#' Inspired by E. Tufte style.
#' 
n <- 15
dta <- dplyr::data_frame(
  x = rnorm(n),
  y = 1.5 * x + rnorm(n))

highchart() %>%
  hc_chart(type = "scatter") %>% 
  hc_add_series(data = list_parse(dta)) %>% 
  hc_add_theme(hc_theme_tufte())


values <- 1 + abs(rnorm(12))
highchart() %>%
  hc_chart(type = "column") %>%
  hc_add_series(data = values) %>%
  hc_xAxis(categories = month.abb) %>%
  hc_add_theme(hc_theme_tufte2())

#' ### Sparkline
#' 

# You can test:
# hc %>% hc_add_theme(hc_theme_sparkline())

data(economics_long, package = "ggplot2")
library(dplyr)

economics_long %>%
  group_by(variable) %>%
  do(spark = hcts(.$value, type = "area",
                  name = first(.$variable), showInLegend = FALSE) %>%
       hc_add_theme(hc_theme_sparkline())) %>% 
  .[["spark"]] %>% 
  as.list() %>% 
  hw_grid(rowheight = 100)

#' ### Grid Light

hc %>% hc_add_theme(hc_theme_gridlight())

#' ### Sand Signika

hc %>% hc_add_theme(hc_theme_sandsignika())

#' ### Dark Unica

hc %>% hc_add_theme(hc_theme_darkunica())

#' ### Chalk
#'
#' Insipired in https://www.amcharts.com/inspiration/chalk/.

hc %>% hc_add_theme(hc_theme_chalk())

#' ### Hand Drawn
#'
#' Insipired in https://www.amcharts.com/inspiration/hand-drawn/ (again!).

hc %>% hc_add_theme(hc_theme_handdrawn())

#' ### Null

hc %>% hc_add_theme(hc_theme_null())

#' ### Create themes

thm <- hc_theme(
  colors = c('red', 'green', 'blue'),
  chart = list(
    backgroundColor = NULL,
    divBackgroundImage = "http://media3.giphy.com/media/FzxkWdiYp5YFW/giphy.gif"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = 'Tangerine',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )   
  )
)

hc %>% hc_add_theme(thm)

#' #### Merge Themes

thm <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    chart = list(
      backgroundColor = "transparent",
      divBackgroundImage = "http://cdn.wall-pix.net/albums/art-3Dview/00025095.jpg"
    ),
    title = list(
      style = list(
        color = 'white',
        fontFamily = "Open Sans"
      )
    )
  )
)

hc %>% hc_add_theme(thm)

