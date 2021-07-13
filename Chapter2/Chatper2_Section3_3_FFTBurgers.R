library(tidyverse)
library(deSolve)
library(pracma)
library(gganimate)
source("Misc/Utils.R")

# Diffusion constant
nu <- 0.001

# Spatial Domain
l <- 20
n <- 1000
dx <- l/n
x <- seq(-l/2, (l/2)-dx, dx)

# Wavenumbers
kappa <- (2*pi/l) * seq(-n/2, (n/2)-1)
kappa <- fftshift(kappa)

# Initial Conditions - as before, hyperbolic sec
u_0 <- sech(x)

# du_dt + u . du_dx = v.d2u_dx2
# Because of the cross term, all the fft goodness is inside the function now
burgers_equation <- function(t, x, params)
{
  u_hat <- fft(x)
  
  # 1st derivative in freq space
  du_hat <- 1i * kappa * u_hat
  # 2nd derivative in freq space
  ddu_hat <- -(kappa^2) * u_hat
  
  # Back to regular space
  du <- Re(fft(du_hat, inverse = TRUE) / n)
  ddu <- Re(fft(ddu_hat, inverse = TRUE) / n)
  
  return (list((-x * du) + (nu * ddu)))
}

dt <- 0.025
t <- seq(0, 100*dt, by = dt)

res <- ode(u_0, t, burgers_equation, parms = NULL)


u_t_df <- data.frame(res) %>% 
  pivot_longer(names_to = "x", names_prefix = "X", values_to = "u", -time) %>% 
  mutate(x = as.numeric(x))


u_t_df %>% 
  filter(time %% 0.25 == 0) %>% 
  ggplot(aes(x, u, colour = as.factor(time))) + 
  geom_line()
         

# Plot animation
ggplot(u_t_df, aes(x, u)) + 
  geom_point() + 
  transition_time(time) 


plot_matrix(res[,-1])
