library(animation)
library(tidyverse)
library(control)
library(pracma)
source("Misc/CartPendulum.R")
source("Misc/LQR.R")
library(deSolve)


ani.options(interval = 0.1)

m <- 1
M <- 5
l <- 2
g <- -10
d <- 1

# Pendulum up
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
D <- matrix(0, nrow = nrow(C), ncol = ncol(B))

V_d <- diag(1, 4)
V_n <- 1

B_aug <- cbind(B, diag(1, 4), 0)
D_aug <- c(0,0,0,0,0,1)
sys_c <- ss(A, B_aug, C, D_aug)

# Kalman Filter
k_f <- t(my_lqr(t(A), t(C), V_d, V_n))
#k_f <- matrix(c(1.9222, 1.3474, -0.61816, -1.8016), ncol = 1)
sys_kalman <- ss(A - (k_f %*% C), cbind(B, k_f), diag(1, 4), 0 * cbind(B, k_f))


# Optimal k for main cart feedback using LQR
q <- diag(c(1,1,1,1))
r <- 0.1
k_r <- my_lqr(A, B, q, r)

eigen(A - B %*% k_r)

# Placeholders for t, u, x
state_hist <- matrix(NA, nrow = 10000, ncol = 3)
state_hist_count <- 1

# This function takes in the full state, applies the C matrix so we only actually record
# the x position, then applies the Kalman filter to back out x_hat - the estimate of the full
# state, which is then passed into our optimal controller k_r.
u <- function(x, t)
{
  x_target <- ifelse(t >= 10, 1, 0)
  w_r <- c(x_target,0,0,0)
  
  # Remove all bar the x position
  latest_x <- x[1,1]
  
  if (state_hist_count > 2)
  {
    i <- state_hist_count-1
    
    last_t <- state_hist[state_hist_count-1, 1]
    this_t <- state_hist[(i-1):i, 1]
    this_u <- state_hist[(i-1):i, 3]
    this_x <- state_hist[(i-1):i, 2]
    #this_x <- c(this_x[-1], latest_x)
    
    sim_kalman <- lsim(sys_kalman, t(rbind(this_u, this_x)), this_t)
    x_bar <- matrix((sim_kalman$x[,ncol(sim_kalman$x)]), ncol = 1)
    this_u <- -k_r %*% (x - w_r)
  }
  else
  {
    # Can't estimate state for step 1
    this_u <- 0
  }
  
  # Add to the history
  state_hist[state_hist_count, 1] <- t
  state_hist[state_hist_count, 2] <- latest_x
  state_hist[state_hist_count, 3] <- this_u
  
  state_hist_count <<- state_hist_count + 1
  state_hist <<- state_hist

  return (this_u)
}

x_0 <- c(0,0, 0, 0)
res <- NULL
res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,u(x, t)), 0, 20, x_0)


for (i in 1:length(res$t))
{
  dev.hold()
  draw_cart_pendulum(res$y[i,], m, M, l)
  ani.pause()
}


