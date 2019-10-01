library(highcharter)
library(forecast)
#> 
#> Attaching package: 'ggfortify'
#> The following object is masked from 'package:forecast':

load("ch10data.Rdata")
library(dplyr)
library(magrittr)
library(lubridate)
estimation_data <- ch10data %>% 
  filter(date > as.Date("1967-12-01") & date < as.Date("1994-12-01"))
estimation_data %<>% mutate(lsales = log(liquor))

y <- estimation_data$lsales
library(xts)

liq <- xts(estimation_data[,1], order.by=estimation_data[,2])

lsales <- xts(estimation_data[,3], order.by=estimation_data[,2])

highchart(type = "stock") %>% 
  hc_add_series(liq, color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "Monthly liquor sell"),
           labels = list(format = "${value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat())

highchart(type = "stock") %>% 
  hc_add_series(lsales, color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "Log of Monthly liquor sell"),
           labels = list(format = "{value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat())

auto.arima(lsales) %>% 
  forecast(h = 6) %>% 
  hchart() %>% 
  hc_title(text = "Log of Monthly liquor sell") %>% 
  hc_yAxis(title = list(text = "monthly price"),
           labels = list(format = "{value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = TRUE)
