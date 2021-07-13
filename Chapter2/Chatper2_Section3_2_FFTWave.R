library(tidyverse)
library(deSolve)
library(pracma)
library(gganimate)
source("Misc/Utils.R")

# Define the problem
c <- 2
l <- 20
n <- 1000
dx <- l/n
x <- seq(-l/2, (l/2-dx), by = dx)

# Wave numbers
kappa <- (2 * pi / l) * seq(-n/2, ((n/2)-1))
kappa <- fftshift(kappa)

# Initial condition - sech shaped wave
u_0 <- sech(x)

# Right hand side of the wave eqn:
# u_t + c * u_x = 0, so
# du_hat_dt = - i * c * kappa * u_hat
wave_equation <- function(t, x, params)
{
  (list(-1i * c * kappa * x))
}

# Simulate in frequency domain
dt <- 0.025
t <- seq(0, 100 * dt, by = dt)
u_hat_0 <- fft(u_0)

# Use zvode as y is complex
res <- zvode(y = u_hat_0, times = t, func = wave_equation)

# Inverse fft to get back to u(x,t)
u_t <- matrix(NA, nrow = length(t), ncol = length(x))

for (k in 1:nrow(res))
{
  # -1 on column to drop the time column.
  this_u <- Re(fft(res[k, -1], inverse = TRUE) / n) 
  u_t[k,] <- this_u
}

u_t_df <- data.frame(u_t) %>% 
  mutate(t = t) %>% 
  pivot_longer(names_to = "x", names_prefix = "X", values_to = "u", -t) %>% 
  mutate(x = as.numeric(x))

# Plot animation
ggplot(u_t_df, aes(x, u)) + 
  geom_point() + 
  transition_time(t) 
