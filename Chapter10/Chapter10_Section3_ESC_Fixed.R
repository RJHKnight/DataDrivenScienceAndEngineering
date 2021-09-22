library(signal)
library(tidyverse)
library(patchwork)

freq <- 10 * 2 * pi
dt <- 1/freq
t <- 10
a <- 0.2
omega <- 10 * 2 * pi
phase <- 0
K <- 5

esc <- function(cost_function)
{
  y_0 <- cost_function(0,0)
  
  # High pass filter --------------------------------------------------------
  
  butter_order <- 1
  butter_freq <- 2
  res <- butter(butter_order, butter_freq * dt * 2, type = "high")
  ys <- rep(0, butter_order + 1) + y_0
  hpf <- rep(0, butter_order + 1)
  
  # Initial Guess
  u <- 0
  u_hat <- u
  
  # Output
  y_vals <- rep(NA, t/dt)
  u_hats <- rep(NA, t/dt)
  u_vals <- rep(NA, t/dt)
  
  for (i in 1:(t/dt))
  {
    t <- (i-1)*dt
    y_vals[i] <- cost_function(u, t)
    
    for (k in 1:butter_order)
    {
      ys[k] <- ys[k+1]
      hpf[k] <- hpf[k+1]
    }
    
    ys[butter_order+1] <- y_vals[i]
    
    hpf_new <- 0
    
    for (k in 1:(butter_order+1))
    {
      hpf_new <- hpf_new + res$b[k] * ys[butter_order+2-k]
    }
    for (k in 2:(butter_order+1))
    {
      hpf_new <- hpf_new - res$a[k] * hpf[butter_order+2-k]
    }
    
    hpf[butter_order+1] <- hpf_new
    
    x_i <- hpf_new * sin(omega*t + phase)
    u_hat <- u_hat + x_i * K * dt
    
    u <- u_hat + a * sin(omega * t + phase)
    
    u_hats[i] <- u_hat
    u_vals[i] <- u
  }
  
  return (list(u_hats = u_hats, u_vals = u_vals, y_vals = y_vals))
}

# Fixed Cost Function -----------------------------------------------------

cost_function_fixed <- function(u, t)
{
  return (25 - (5-u)^2)
}

res_fixed <- esc(cost_function_fixed)

# Plot cost_function (with maximum at 5)
data.frame(u = seq(0,10, 0.1)) %>% 
  mutate(j = cost_function_fixed(u, 0 )) %>% 
  ggplot(aes(u, j)) + 
  geom_line() + 
  annotate("point", x = 5, y = 25, colour = "red") + 
  theme_bw()


# Results
ts <- 1:((t/dt)) * dt

u_plot <- data.frame(t = ts, u = res_fixed$u_vals, u_hat = res_fixed$u_hats) %>% 
  pivot_longer(-t, "type", "value") %>% 
  ggplot(aes(t, value, colour = type)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "bottom")

j_plot <- data.frame(t = ts, j = res_fixed$y_vals) %>% 
  ggplot(aes(t, j)) + 
  geom_line() + 
  theme_bw()


u_plot / j_plot + plot_annotation(title = 'Fixed Cost Function')



# Time Varying Cost Function ----------------------------------------------

freq <- 100
dt <- 1/freq
t <- 100
a <- 0.2
omega <- 10 * 2 * pi
phase <- 0
K <- 5
ts <- 1:((t/dt)) * dt

cost_function_varying <- function(u, t)
{
  return (25 - (5-u - sin(t))^2)
}

res_varying <- esc(cost_function_varying)

u_plot_var <- data.frame(t = ts, u = res_varying$u_vals, u_hat = res_varying$u_hats) %>% 
  pivot_longer(-t, "type", "value") %>% 
  ggplot(aes(t, value, colour = type)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "bottom")

j_plot_var <- data.frame(t = ts, j = res_varying$y_vals) %>% 
  ggplot(aes(t, j)) + 
  geom_line() + 
  theme_bw()


u_plot_var / j_plot_var + plot_annotation(title = 'Time Varying Cost Function')
