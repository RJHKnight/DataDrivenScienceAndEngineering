library(deSolve)
library(tidyverse)
source("Misc/Utils.R")
source("Misc/PoolData.R")
source("Misc/SparsifyDynamics.R")

params <- c(rho = 28, sigma = 10, beta = 8/3)
x_0 <- c(-8, 8, 27)
names(x_0) <- c("x", "y", "z")

this_traj <- ode(x_0, seq(0.001,50, by = 0.001), lorenz, params, method = "ode45")

# Derivative
dx <- matrix(unlist(sapply(1:nrow(this_traj), function(i) lorenz(0, this_traj[i,-1], params))), ncol = 3, byrow = TRUE)
colnames(dx) <- c("dx", "dy", "dz")

polyorder <- 3
theta <- pool_data(this_traj[,-1], 3, c("x", "y", "z"))
lambda <- 0.025

x_i <- sparsify_dynamics(theta, dx, lambda)

# Only show the non-zero terms
x_i[apply(abs(x_i) > 0, 1, any), ]
