load("ch10data.Rdata")
library(dplyr)
library(magrittr)
library(lubridate)
library(forecast)

estimation_data <- ch10data %>% 
  filter(date > as.Date("1967-12-01") & date < as.Date("1994-12-01"))
estimation_data %<>% mutate(lsales = log(liquor))

y <- estimation_data$lsales

# estimation procedure
y <- estimation_data$lsales
time <- 1:length(y)
seasonal <- as.factor(month(estimation_data$date))
X <- cbind(time, time2 = time^2, model.matrix(~seasonal - 1))
reg <- Arima(y, order = c(3, 0, 0), xreg = X, include.mean = FALSE)
summary(reg)

