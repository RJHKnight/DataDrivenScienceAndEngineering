library(control)
library(tidyverse)
source("Misc/Bode.R")

G <- tf(1, c(1, 1, 2))

plot_bode(bode(s))

a <- matrix(c(0, 1, -2, -1), nrow = 2, byrow = TRUE)
b <- matrix(c(0, 1), ncol = 1)
c <- matrix(c(1, 0), nrow = 1)
d <- matrix(0)

G <- ss2tf(a, b, c, d)

plot_impulse(impulse(G))

plot_step(step(G, t = seq(0, 10, by = 0.05)))
