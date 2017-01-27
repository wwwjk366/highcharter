#' ---
#' layout: post
#' ---
#+echo=FALSE
rm(list = ls())
library("highcharter")
options(highcharter.theme = hc_theme_smpl())

#' ## Plugins 
#' 
#' <div id ="toc"></div>
#' 
#' Highcharter include some plugins for highcharts so you have almost
#' no limits to implement whatever you want.
#' 

#' ### Motion ####
#' 
#' Example 1
#' 
library("idbr")
library("purrr")
library("dplyr")

idb_api_key("35f116582d5a89d11a47c7ffbfc2ba309133f09d")
yrs <-  seq(1980, 2030, by = 5)

df <- map_df(c("male", "female"), function(sex){
  mutate(idb1("US", yrs, sex = sex), sex_label = sex)
})

names(df) <- tolower(names(df))

df <- df %>%
  mutate(population = pop*ifelse(sex_label == "male", -1, 1))

series <- df %>% 
  group_by(sex_label, age) %>% 
  do(data = list(sequence = .$population)) %>% 
  ungroup() %>% 
  group_by(sex_label) %>% 
  do(data = .$data) %>%
  mutate(name = sex_label) %>% 
  list_parse()

maxpop <- max(abs(df$population))

xaxis <- list(categories = sort(unique(df$age)),
              reversed = FALSE, tickInterval = 5,
              labels = list(step = 5))

highchart() %>%
  hc_chart(type = "bar") %>%
  hc_motion(enabled = TRUE, labels = yrs, series = c(0,1), autoplay = TRUE, updateInterval = 1) %>% 
  hc_add_series_list(series) %>% 
  hc_plotOptions(
    series = list(stacking = "normal"),
    bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
  ) %>% 
  hc_tooltip(shared = TRUE) %>% 
  hc_yAxis(
    labels = list(
      formatter = JS("function(){ return Math.abs(this.value) / 1000000 + 'M'; }") 
    ),
    tickInterval = 0.5e6,
    min = -maxpop,
    max = maxpop) %>% 
  hc_xAxis(
    xaxis,
    rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
  ) %>% 
  hc_tooltip(shared = FALSE,
             formatter = JS("function () { return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
  ) 

#' 
#' Example 2
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_yAxis(max = 6, min = 0) %>% 
  hc_add_series(name = "A", data = c(2,3,4), zIndex = -10) %>% 
  hc_add_series(name = "B",
                data = list(
                  list(sequence = c(1,2,3,4)),
                  list(sequence = c(3,2,1,3)),
                  list(sequence = c(2,5,4,3))
                )) %>% 
  hc_add_series(name = "C",
                data = list(
                  list(sequence = c(3,2,1,3)),
                  list(sequence = c(2,5,4,3)),
                  list(sequence = c(1,2,3,4))
                )) %>% 
  hc_motion(enabled = TRUE,
            labels = 2000:2003,
            series = c(1,2))


#' <br><br>
#' 
#' ### Multicolor series
library(dplyr)

n <- 10
colors <- sample(colorize(1:n))

df <- data_frame(
  x = 1:n,
  y = runif(n, 1, 5),
  color =  colorize(y)
)

hchart(df, "coloredarea", hcaes(x, y, segmentColor = color))

hchart(df, "coloredline", hcaes(x, y, segmentColor = color))

#' 
#' ### FontAwesome Integration
library(MASS)

dscars <- round(mvrnorm(n = 20, mu = c(1, 1), Sigma = matrix(c(1,0,0,1),2)), 2)
dsplan <- round(mvrnorm(n = 10, mu = c(3, 4), Sigma = matrix(c(2,.5,2,2),2)), 2)
dstrck <- round(mvrnorm(n = 15, mu = c(5, 1), Sigma = matrix(c(1,.5,.5,1),2)), 2)

highchart() %>%
  hc_chart(type = "scatter", zoomType = "xy") %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = paste0("<span style=\"color:{series.color};\">{series.options.icon}</span>",
                         "{series.name}: <b>[{point.x}, {point.y}]</b><br/>")
  ) %>% 
  hc_add_series(data = list_parse2(as.data.frame(dscars)),
                marker = list(symbol = fa_icon_mark("car")),
                icon = fa_icon("car"), name = "car") %>% 
  hc_add_series(data = list_parse2(as.data.frame(dsplan)),
                marker = list(symbol = fa_icon_mark("plane")),
                icon = fa_icon("plane"), name = "plane") %>% 
  hc_add_series(data = list_parse2(as.data.frame(dstrck)),
                marker = list(symbol = fa_icon_mark("truck")),
                icon = fa_icon("truck"), name = "truck")

#' ### Grouped Categories ####
library(purrr) # map function to make grouped categories argument
library(dplyr) # for select function 

data(mpg, package = "ggplot2")

mpgg <- mpg %>% 
  filter(class %in% c("suv", "compact", "midsize")) %>% 
  group_by(class, manufacturer) %>% 
  summarize(count = n())

categories_grouped <- mpgg %>% 
  group_by(name = class) %>% 
  do(categories = .$manufacturer) %>% 
  list_parse()

highchart() %>% 
  hc_xAxis(categories = categories_grouped) %>% 
  hc_add_series(data = mpgg, type = "bar", hcaes(y = count, color = manufacturer),
                showInLegend = FALSE)


#' ### Exporting CSV ####
#'
#' You can export the data to excel file, csv, or show in the html.

hc <- highcharts_demo()

hc %>% 
  hc_exporting(
    enabled = TRUE
  )


#' ### Draggable points ####
#' 
data("citytemp")

highchart() %>% 
  hc_chart(animation = FALSE) %>% 
  hc_title(text = "draggable points demo") %>% 
  hc_subtitle(text = "Drang my points plz") %>% 
  hc_xAxis(categories = month.abb) %>% 
  hc_plotOptions(
    series = list(
      point = list(
        events = list(
          drop = JS("function(){
                    alert(this.series.name + ' ' + this.category + ' ' + Highcharts.numberFormat(this.y, 2))
                    }")
        )
          ),
      stickyTracking = FALSE
        ),
    column = list(
      stacking = "normal"
    ),
    line = list(
      cursor = "ns-resize"
    )
    ) %>% 
  hc_tooltip(yDecimals = 2) %>% 
  hc_add_series(
    data = citytemp$tokyo,
    draggableY = TRUE,
    dragMinY = 0,
    type = "column",
    minPointLength = 2
  ) %>% 
  hc_add_series(
    data = citytemp$new_york,
    draggableY = TRUE,
    dragMinY = 0,
    type = "column",
    minPointLength = 2
  ) %>% 
  hc_add_series(
    data = citytemp$berlin,
    draggableY = TRUE
  )

#' ### More Symbols

dss <- map(c("square", "triangle",  "cross", "plus"), function(s){

x <- rnorm(20, runif(1)*2)
y <- (rnorm(1) + 1)*x + runif(20)

list(name = s,
     data = list_parse(data_frame(x, y)),
     marker = list(symbol = s, enabled = TRUE), lineColor = NULL)

})


highchart() %>% 
  hc_chart(type = "scatter") %>% 
  hc_add_series_list(dss)

#' ### Pattern fill

highchart() %>% 
  hc_title(text = "I'm a pirate looking chart") %>% 
  hc_xAxis(categories = month.abb) %>% 
  hc_defs(patterns = list(
    list(id = 'custom-pattern',
         path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11',
                     stroke = "black",
                     strokeWidth = 1
         )
    )
  )) %>% 
  hc_add_series(data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2,
                         26.5, 23.3, 18.3, 13.9, 9.6),
                type = "area",
                fillColor = 'url(#custom-pattern)') %>% 
  hc_add_theme(hc_theme_handdrawn())

