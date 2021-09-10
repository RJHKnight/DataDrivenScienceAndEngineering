library(control)
library(pracma)
library(tidyverse)
source("Misc/LQR.R")

m <- 1
M <- 5
l <- 2
g <- -10
d <- 1

# Pendulum down
b <- -1

A <- matrix(c(
  0,          1,                0, 0,
  0,       -d/M,          b*m*g/M, 0,
  0,          0,                0, 1,
  0, -b*d/(M*l), -b*(m+M)*g/(M*l), 0
), ncol = 4, byrow = TRUE)

B <- matrix(c(0, 1/M, 0, b*1/(M*l)), ncol = 1)

# We only measure the cart's position (x)
C <- matrix(c(1, 0, 0, 0), nrow = 1)

Rank(obsv(A, C))

D <- matrix(0, nrow = nrow(C), ncol = ncol(B))

V_d <- diag(1, 4)
V_n <- 1

# Kalman Filter

k_f <- matrix(my_lqr(t(A), t(C), V_d, V_n)$k, ncol = 1)

# Augment system with additional inputs
B_aug <- cbind(B, diag(1, 4), 0)
D_aug <- c(0,0,0,0,0,1)

# System with only measurements of x
sys_c <- ss(A, B_aug, C, D_aug)

# True system for comparison
sys_truth <- ss(A, B_aug, diag(1, 4), matrix(0, nrow = 4, ncol = ncol(B_aug)))

sys_kalman <- ss(A - (k_f %*% C), cbind(B, k_f), diag(1, 4), 0 * cbind(B, k_f))

dt <- 0.01
t <- seq(0.01, 50, by = dt)

u_dist <- sqrt(V_d) %*% matrix(rnorm(4 * length(t)), nrow = 4)
u_noise <- sqrt(V_n) * rnorm(length(t))

# Positive and negative shocks at t = 100, 1500
u <- rep(0, length(t))
u[100] <- 2000
u[1500] <- -2000

u_aug <- rbind(u, u_dist, u_noise)

sim_noisy <- lsim(sys_c, t(u_aug), t)
sim_true <- lsim(sys_truth, t(u_aug), t)
sim_kalman <- lsim(sys_kalman, t(rbind(u, sim_noisy$y)), t)



# Plots -------------------------------------------------------------------

y_df <- rbind(
  data.frame(t = sim_noisy$t, y = sim_noisy$y[1,], type = "Measured"),
  data.frame(t = sim_true$t, y = sim_true$y[1,], type = "True"),
  data.frame(t = sim_kalman$t, y = sim_kalman$y[1,], type = "KF Estimate")
)

y_df %>% 
  mutate(alpha = if_else(type == "Measured", 0.6, 1)) %>% 
  ggplot(aes(t, y, colour = type, alpha = alpha)) + 
  geom_line() + 
  scale_alpha(guide = 'none') + 
  theme_bw() + 
  theme(legend.position = "bottom")

x_df <- rbind(data.frame(t = sim_true$t, t(sim_true$x), type = "True"),
              data.frame(t = sim_kalman$t, t(sim_kalman$x), type = "KF Estimate"))

colnames(x_df) <- c("t", "x", "x_dot", "theta", "theta_dot", "type")

x_df <- x_df %>% 
  pivot_longer(-c("t", "type"), names_to = "state", values_to = "value")

ggplot(x_df, aes(t, value, colour = state, linetype = type)) + 
  geom_line() + 
  facet_wrap(~ state, scales = "free_y") + 
  theme_bw()