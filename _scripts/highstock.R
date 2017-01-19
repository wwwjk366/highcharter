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
#' easy chart symbols. And then you can add more series
#' using `hc_add_series` (see below).
#'
library("quantmod")

x <- getSymbols("GOOG", auto.assign = FALSE)

hchart(x)

#' ### Candlestick and OHLC charts
#'
#' If you want to chart more symbols in you can
#' use the `hc_add_series` function.  
#'

x <- getSymbols("GOOG", auto.assign = FALSE)
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

hc <- highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(usdjpy, id = "usdjpy") %>% 
  hc_add_series(eurkpw, id = "eurkpw")

hc

#' ### Flags
#' 
#' Previously we used the `id` parameter. This is necessary 
#' to add flags:
#' 
library(dplyr)

set.seed(123)

data_flags <- data_frame(
  date = sample(time(usdjpy), size = 5),
  title = sprintf("E #%s", seq_along(date)),
  text = sprintf("An interesting event #%s in %s", seq_along(date), date)
)

glimpse(data_flags)

hc %>% 
  hc_add_series(data_flags, hcaes(x = date),
                type = "flags", onSeries = "usdjpy")

#' 
#' ### A More Interesting Example
#' 
#' You can do what you want. Add axis, series, bands, etc.
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

