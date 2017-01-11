#' ---
#' layout: post
#' ---
#+echo=FALSE
rm(list = ls())
library("highcharter")
options(download.file.method = "libcurl",
        highcharter.theme = hc_theme_smpl())

#'
#' ## Highstock Examples
#' 
#' <div id ="toc"></div>
#' 
#' ### Basics
#' 
#' Highstock work well with the quantmod package. It's 
#' easy chart symbols. 
#'
library("quantmod")

x <- getSymbols("GOOG", auto.assign = FALSE)

hchart(x)

#' ### Candlestick and OHLC charts
#'
#' If you want to chart more symbols in you can
#' use the `hc_add_series` function.  
#'

x <- getSymbols("AAPL", auto.assign = FALSE)
y <- getSymbols("AMZN", auto.assign = FALSE)

highchart(type = "stock") %>% 
  hc_add_series(x) %>% 
  hc_add_series(y, type = "ohlc")


#'
#' ### Time series
#' 

library("quantmod")

usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
eurkpw <- getSymbols("EUR/KPW", src = "oanda", auto.assign = FALSE)

dates <- as.Date(c("2015-05-08", "2015-09-12"), format = "%Y-%m-%d")

highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(usdjpy, id = "usdjpy") %>% 
  hc_add_series(eurkpw, id = "eurkpw") %>% 
  hc_add_series_flags(dates,
                      title = c("E1", "E2"), 
                      text = c("Event 1", "Event 2"),
                      id = "usdjpy") %>% 
  hc_add_theme(hc_theme_flat()) 

#' 
#' ### A More Interesting Example
#' 
#' You can do what you want. Add axis, more series, etc.
#' 
SPY <- getSymbols("SPY", from = Sys.Date() - lubridate::years(1), auto.assign = FALSE)
SPY <- adjustOHLC(SPY)

SPY.SMA.10 <- SMA(Cl(SPY), n = 5)
SPY.SMA.200 <- SMA(Cl(SPY), n = 100)
SPY.RSI.14 <- RSI(Cl(SPY))
SPY.RSI.SellLevel <- xts(rep(70, NROW(SPY)), index(SPY))
SPY.RSI.BuyLevel <- xts(rep(30, NROW(SPY)), index(SPY))


highchart(type = "stock") %>% 
  # create axis :)
  hc_yAxis_multiples(
    create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
  ) %>% 
  # series :D
  hc_add_series(SPY, yAxis = 0, name = "SPY") %>% 
  hc_add_series(SPY.SMA.10, yAxis = 0, name = "Fast MA") %>% 
  hc_add_series(SPY.SMA.200, yAxis = 0, name = "Slow MA") %>% 
  hc_add_series(SPY$SPY.Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
  hc_add_series(SPY.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
  hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                yAxis = 2, name = "Sell level") %>% 
  hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                yAxis = 2, name = "Buy level") 

