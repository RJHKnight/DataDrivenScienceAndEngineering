library(deSolve)
library(QZ)
library(rgl)
library(tidyverse)

# Define non-linear system
mu <- -0.05
lambda <- -1
a <- matrix(c(mu, 0, 0, 0, lambda, -lambda, 0, 0, 2*mu), ncol = 3, byrow = TRUE)
eig_res <- geigen(a)

# Slope of stable manifold
slope_stable_man <- eig_res$V[3,3] / eig_res$V[2,3]

# Integrate Koopman Trajectories.
y0_a <- c(1.5, -1, 2.25)
y0_b <- c(1, -1, 1)
y0_c <- c(2, -1, 4)

this_function <- function(t, state, parms)
{
  return (list(a %*% state))
}

t_span <- seq(0,1000, by = 0.1)
sol_a <- ode(y0_a, t_span, this_function, NULL, method = "ode45")
sol_b <- ode(y0_b, t_span, this_function, NULL, method = "ode45")
sol_c <- ode(y0_c, t_span, this_function, NULL, method = "ode45")

xyz_df <- rbind(
  data.frame(x = sol_a[,2], y = sol_a[,3], z = sol_a[,4], type = "red"),
  data.frame(x = sol_b[,2], y = sol_b[,3], z = sol_b[,4], type = "green"),
  data.frame(x = sol_c[,2], y = sol_c[,3], z = sol_c[,4], type = "blue")
)

# Invariant surfaces
x_z_mesh <- mesh(x = seq(-2, 2, by = 0.05),
                 y = seq(-1, 4, by = 0.05))

x_z_mesh$z <- x_z_mesh$y
x_z_mesh$y <- x_z_mesh$x ^2


with(xyz_df,
  plot3d(x,y,z, col = type)
)

# Red manifold: y = x^2
surface3d(x = x_z_mesh$x, y = x_z_mesh$y, z = x_z_mesh$z, col = "red", alpha = 0.1)

# Blue manifold: z = x^2
x_z_mesh <- mesh(x = seq(-2, 2, by = 0.05),
                 y = seq(-1, 4, by = 0.05))


x_z_mesh$z <- x_z_mesh$x^2
surface3d(x = x_z_mesh$x, y = x_z_mesh$y, z = x_z_mesh$z, col = "blue", alpha = 0.1)

# Green manifold: Stable invariant subspace
x_z_mesh$z <- slope_stable_man * x_z_mesh$y
surface3d(x = x_z_mesh$x, y = x_z_mesh$y, z = x_z_mesh$z, col = "green", alpha = 0.1)
