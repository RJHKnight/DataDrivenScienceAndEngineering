library(tseries)

set.seed(1)


y <- -4 + arima.sim(
  n = 100,
  model = list(ar = c(0.2, 0.5)),
  rand.gen = function(n, ...) rnorm(n, sd = sqrt(2)))

ar_1 <- arima(y, order = c(1, 0, 0), include.mean = FALSE)
ar_2 <- arima(y, order = c(2, 0, 0), include.mean = FALSE)
ar_3 <- arima(y, order = c(3, 0, 0), include.mean = FALSE)

AIC(ar_1)
AIC(ar_2)
AIC(ar_3)

BIC(ar_1)
BIC(ar_2)
BIC(ar_3)
