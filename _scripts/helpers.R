get_demos <- function(){
  library(highcharter)
  library(forecast)
  library(purrr)
  library(viridisLite)
  library(igraph)
  library(dplyr)
  library(mvtnorm)
  library(tidyr)
  
  options(highcharter.theme = hc_theme_smpl(
    plotOptions = list(
      series = list(showInLegend = FALSE),
      line = list(lineWidth = 1)),
    tooltip = list(valueDecimals = 2)
    )
  )
  
  data(diamonds, mpg, package = "ggplot2")
  data(GNI2014, package = "treemap")
  
  
  p1 <- hchart(mpg, "scatter", hcaes(x = displ, y = hwy, color = hwy), name = "Cars")
  
  p2 <- hchart(forecast(auto.arima(AirPassengers), level = 95, h = 12*3))
  
  p3 <- hcmap(data = GNI2014, name = "", value = "GNI",
              joinBy = c("iso-a3", "iso3"), nullColor = "#FAFAFA") %>% 
    hc_colorAxis(stops = color_stops(colors = inferno(10, begin = 0.1)),
                 type = "logarithmic") %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_mapNavigation(enabled = FALSE)
  
  p4 <- hcboxplot(diamonds$price, var = diamonds$color,
                  outliers = FALSE, name = "price") %>% 
    hc_yAxis(min = 0)
  
  
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
  V(net)$color <- colorize(membership(wc), magma(length(wc)))
  p5 <- hchart(net, layout = layout_with_fr)
  
  p6 <- hcwaffle(c("car", "truck", "plane"), c(10, 7, 5),
                 icons = c("car", "truck", "plane")) %>% 
    hc_colors(hex_to_rgba(viridis(3, end = .8)))
  
  p7 <- highchart() %>% 
    hc_add_series(density(rnorm(100000)),
                  type = "area", name = "Normal Distribution") %>% 
    hc_add_series(density(rgamma(100000, 5, 0.8)),
                  type = "area", name = "Gamma(5. 0.8) Distribution") %>%
    hc_add_series(density(rgamma(100000, 3, 0.8)),
                  type = "area", name = "Gamma(3. 0.8) Distribution") %>% 
    hc_plotOptions(series = list(fillOpacity = 0.5)) %>% 
    hc_xAxis(min = -5, max = 12) 
  
  
  brks <- seq(-3, 3, length.out = 40)
  grid <- expand.grid(brks, brks)
  m <- as.data.frame(grid) %>% 
    mutate(value =
             dmvnorm(grid, mean = c(1, 1), sigma = matrix(c(1, .7, .7, 1), nrow = 2)) +
             dmvnorm(grid, mean = c(-1, -1), sigma = matrix(c(1, -.7, -.7, 1), nrow = 2)) +
             dmvnorm(grid, mean = c(2, -2), sigma = matrix(c(1, 0, 0, 1), nrow = 2))) %>% 
    spread(Var2, value) %>% 
    select(-Var1) %>% 
    as.matrix() 
  
  p8 <- hchart(m) %>% 
    hc_add_theme(hc_theme_null()) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_colorAxis(stops = color_stops(colors = inferno(10, begin = 0.1)))
  
  
  plots <- list(p1, p2, p3, p4, p5, p6, p7, p8)
  
  plots <- map(plots, hc_size, height = 250)
  
  divplots <- map(plots, tags$div, class = "col-md-6")
  
  divplots <- htmltools::tagList(
    tags$div(class = "row", divplots)
  )
  
  # htmltools::browsable(divplots)
  
  divplots  
  
  
}
