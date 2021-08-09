library(deSolve)
library(rgl)
library(tidyverse)
source("Misc/Utils.R")

params <- c(
  rho = 28,
  sigma = 10,
  beta = 8/3)

x_0 <- c(0, 1, 20)
names(x_0) <- c("x", "y", "z")

this_traj <- ode(x_0, seq(0.001,20, by = 0.001), lorenz, params, method = "ode45")
this_df <- data.frame(this_traj)

plot3d(this_df$x, this_df$y, this_df$z, 
       col = "red")ies
