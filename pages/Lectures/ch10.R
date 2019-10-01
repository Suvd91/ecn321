load("C:/Users/MBA01/Downloads/ch10data.Rdata")
library(dplyr)
library(magrittr)
library(lubridate)
estimation_data <- ch10data %>% 
  filter(date > as.Date("1967-12-01") & date < as.Date("1994-12-01"))
estimation_data %<>% mutate(lsales = log(liquor))

y <- estimation_data$lsales
load("C:/Users/MBA01/Downloads/ch10data.Rdata")
library(dplyr)
library(magrittr)
library(lubridate)
estimation_data <- ch10data %>% 
  filter(date > as.Date("1967-12-01") & date < as.Date("1994-12-01"))
estimation_data %<>% mutate(lsales = log(liquor))
hchart(estimation_data)

y <- estimation_data$lsales
time <- 1:length(y)
seasonal <- as.factor(month(estimation_data$date))
X <- cbind(time, time2 = time^2, model.matrix(~seasonal - 1))
reg <- arima(y, order = c(3, 0, 0), xreg = X, include.mean = FALSE)
summary(reg)
reg <- arima(y, order = c(3, 0, 0), xreg = X, include.mean = FALSE)
library("forecast")

plot <- forecast(reg, h = 12, level = 95)
predict(reg, 12)

fore