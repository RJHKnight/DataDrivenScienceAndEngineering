library(tuneR)
library(signal)

t <- seq(0, 2, by = 0.01)
f_0 <- 50
f_1 <- 250
t_1 <- 2

x <- cos(2 * pi * t * ((f_0 + (f_1 - f_0) * t^2) / (3 * t_1^2)))

w = Wave((2^15-1) * x, samp.rate = 8000, bit = 16)
play(w)

plot(t, x, type = "l")

specgram(x, 128, Fs = 8000)
