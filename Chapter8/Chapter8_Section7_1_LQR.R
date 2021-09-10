library(animation)
library(QZ)
library(control)
library(pracma)
library(tidyverse)
source("Misc/CartPendulum.R")
source("Misc/LQR.R")
source("Misc/Place.R")

ani.options(interval = 0.1)

m <- 1
M <- 5
l <- 2
g <- -10
d <- 1

# Pendulum up
b <- 1

A <- matrix(c(
  0,          1,                0, 0,
  0,       -d/M,          b*m*g/M, 0,
  0,          0,                0, 1,
  0, -b*d/(M*l), -b*(m+M)*g/(M*l), 0
  ), ncol = 4, byrow = TRUE
)

B <- matrix(c(0, 1/M, 0, b*1/(M*l)), ncol = 1)

geigen(A)
Rank(ctrb(A, B))

# Simulate system with no feedback
x_0 <- c(-1,0, pi+0.1, 0)

res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,0), 0, 5, x_0)

for (i in 1:length(res$t))
{
  dev.hold()
  draw_cart_pendulum(res$y[i,], m, M, l)
  ani.pause()
}


# Now use place to define a K matrix so our system has the desired eigenvalues

p <- matrix(c(-1.1, -1.2, -1.3, -1.4), ncol = 1)
k <- t(place(A,B,p))

eigen(A - B %*% k)
w_r <- c(3,0,pi,0)

u <- function(x)
{
  -k %*% (x-w_r)
}

res <- NULL
res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,u(x)), 0, 10, x_0)

for (i in 1:length(res$t))
{
  dev.hold()
  draw_cart_pendulum(res$y[i,], m, M, l)
  ani.pause()
}


# Now using LQR to define a k with optimal properties for our given cost functions
q <- diag(c(1,1,1,1))
r <- 0.0001

k <- my_lqr(A, B, q, r)$k


eigen(A - B %*% k)
w_r <- c(3,0,pi,0)

u <- function(x)
{
  -k %*% (x-w_r)
}

res <- NULL
res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,u(x)), 0, 50, x_0)

res_df <- data.frame(t = res$t, x = res$y[,1], x_dot = res$y[,2],
                     theta = res$y[,3], theta_dot = res$y[,4])


this_cost <- rep(0, length(res$t))

for (i in 1:length(res$t))
{
  this_cost[i] <- (res$y[i,]-w_r) %*% q %*% (res$y[i,]-w_r) + u(res$y[i,])^2 %*% r
}

lqr_cost <- data.frame(
  t      = res$t,
  cost   = this_cost,
  run_id = "lqr"
)

lqr_cost <- lqr_cost %>% 
  arrange(t) %>% 
  mutate(cost = cumtrapz(t,cost))


res_df$time_bin <- cut(res_df$t, 100)

# Convert to equal time steps
y <- res_df %>% 
  mutate(time_bin = cut(t, 100)) %>% 
  group_by(time_bin) %>% 
  summarise(
    t = t[1],
    x = mean(x),
    x_dot = mean(x_dot),
    theta = mean(theta),
    theta_dot = mean(theta_dot)
  ) %>% 
  select(-time_bin) %>% 
  as.matrix(.)
  

for (i in 1:length(y[,'t']))
{
  dev.hold()
  draw_cart_pendulum(y[i,2:5], m, M, l)
  ani.pause()
}


# Comparison of LQR with various random pole placements

state_df <- NULL
cost_df <- NULL
r <- 0.0001

for (i in 1:100)
{
  # Random pole placement
  p <- matrix(c(-.5-(3*runif(1)), -.5-(3*runif(1)), -.5-(3*runif(1)), -.5-(3*runif(1))), ncol = 1)
  k <- place(A,B,p)
  
  # Run simulation
  res <- ode45(function(t, x) pend_cart(t,x,m,M,l,g,d,u(x)), 0, 50, x_0)
  
  # Store results
  state_df <- rbind(state_df,
                    data.frame(t         = res$t,
                               x         = res$y[,1],
                               x_dot     = res$y[,2],
                               theta     = res$y[,3],
                               theta_dot = res$y[,4],
                               run_id    = as.character(i)))
  
  this_cost <- rep(0, length(res$t))
  
  for (j in 1:length(res$t))
  {
    this_cost[j] <- (res$y[j,]-w_r) %*% q %*% (res$y[j,]-w_r) + u(res$y[j,])^2 %*% r
  }
  
  this_cost_df <- data.frame(
    t      = res$t,
    cost   = this_cost,
    run_id = as.character(i)
  )
  
  this_cost_df <- this_cost_df %>% 
    arrange(t) %>% 
    mutate(cost = cumtrapz(t,cost))
  
  cost_df <- rbind(cost_df, this_cost_df)
}

# Plot of state vs time
state_df %>% 
  pivot_longer(-c("t", "run_id"), "state", "value") %>% 
  ggplot(aes(t, value, colour = run_id)) + 
  geom_line() + 
  facet_wrap(~ state, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  theme(legend.position = "none")

# Plot of cost vs time
cost_df %>% 
  filter(run_id %in% sample(unique(run_id), 50)) %>% 
  ggplot(aes(t, cost, colour = run_id)) + 
  geom_line() + 
  geom_line(data = lqr_cost, colour = "black", size = 1.5, linetype = 2) +
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 10))



