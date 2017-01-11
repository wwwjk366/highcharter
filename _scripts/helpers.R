get_demos <- function(){
  library("highcharter")
  library("forecast")
  options(highcharter.theme = hc_theme_smpl())
  data("citytemp")
  data(diamonds, mpg, package = "ggplot2")
  
  p1 <- hchart(mpg, "scatter", hcaes(x = displ, y = hwy, color = hwy))
  
  p2 <- hchart(
    forecast(auto.arima(AirPassengers), level = 95, h = 12*3),
    showInLegend = FALSE) %>% 
    hc_tooltip(valueDecimals = 2) %>% 
    hc_add_theme(hc_theme_538())
  
  p3 <- highcharts_demo()%>% 
    hc_add_theme(hc_theme_economist()) %>% 
    hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
    hc_title(text = "") %>% hc_subtitle(text = "")
  
  data(worldgeojson)
  data(GNI2014, package = "treemap")
  head(GNI2014)
  library("viridisLite")
  
  dshmstops <- data.frame(
    q = c(0, exp(1:5)/exp(5)),
    c = substring(viridis(5 + 1), 0, 7)) %>% 
    list_parse2()
  
  p4 <- hcmap(data = GNI2014, name = "", value = "GNI", joinBy = c("iso-a3", "iso3")) %>% 
    hc_colorAxis(stops = dshmstops) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_db()) %>% 
    hc_mapNavigation(enabled = FALSE)
  
  p5 <- hcboxplot(diamonds$price, var = diamonds$color, outliers = FALSE, name = "price") %>% 
    hc_yAxis(min = 0)
  
  
  library("igraph")
  N <- 20
  net <- sample_gnp(N, p = .1)
  wc <- cluster_walktrap(net)
  V(net)$label <- 1:N
  V(net)$name <- 1:N
  V(net)$page_rank <- round(page.rank(net)$vector, 2)
  V(net)$betweenness <- round(betweenness(net), 2)
  V(net)$degree <- degree(net)
  V(net)$size <- V(net)$degree
  V(net)$comm <- membership(wc)
  V(net)$color <- colorize(membership(wc), viridis::inferno(10))
  
  p6 <- hchart(net, layout = layout_with_fr)
  
  
  H <- 250
  p1$height <- H
  p2$height <- H
  p3$height <- H
  p4$height <- H
  p5$height <- H
  p6$height <- H
  
  demos <- htmltools::tagList(
    tags$div(
      class = "row",
      tags$div(class = "col-sm-6", p1),
      tags$div(class = "col-sm-6", p2)
    ),
    tags$div(
      class = "row",
      tags$div(class = "col-sm-6", p3),
      tags$div(class = "col-sm-6", p4)
    ),
    tags$div(
      class = "row",
      tags$div(class = "col-sm-6", p5),
      tags$div(class = "col-sm-6", p6)
    )
  )
  
  # htmltools::browsable(demos)
  demos  
  
  
}
