library(tidyverse)
library(deSolve)
library(gganimate)
source("Misc/Utils.R")

# Thermal diffusivity constant
alpha <- 1
# Length of domain
l <- 100
# Number of discrete points
n <- 1000
dx <- l/n
x <- seq(-l/2, l/2-dx, dx)

# Discrete wavenumbers
kappa <- (2 * pi / l) * seq(-n/2, ((n/2)-1))
kappa <- fftshift(kappa)

# Initial conditions - top hat with u = 1 for l/10 above and below midpoint
u_0 <- 0 * x
u_0[((l/2-l/10)/dx):((l/2+l/10)/dx)] <- 1


# Second derivative wrt. kappa of u_hat - linear and diagonal makes for an easy ODE!
heat_equation <- function(t, x, params)
{
  (list(-alpha^2 * t(kappa^2) * x))
}


# Simulate the evolution in time of u_hat (so in frequency domain)
t <- seq(0, 10, by = 0.025)
u_hat_0 <- fft(u_0)

# Use zvode as y is complex
res <- zvode(y = u_hat_0, times = t, func = heat_equation)

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


ggplot(u_t_df, aes(x, u)) + 
  geom_point() + 
  transition_time(t) 
