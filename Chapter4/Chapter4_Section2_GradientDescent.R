library(rgl)
library(tidyverse)
library(patchwork)
library(pracma)
library(gganimate)
source("Misc/Utils.R")


# Example of local and global minima --------------------------------------

h <- 0.5
x <- seq(-6, 6, by = h)
y <- x

x_mat <- matrix(rep(x, length(y)), byrow = TRUE, nrow = length(y))
y_mat <- matrix(rep(y, length(x)), byrow = FALSE, nrow = length(x))

f_0 <- 1.5 - 1.0 * exp(-0.03 * ((3 * (x_mat)^2) + y_mat^2))
f   <- 1.5 - 1.6 * exp(-0.05 * ((3 * (x_mat + 3)^2) + (y_mat+3)^2))
f_2 <- f + (0.5 - exp(-0.1*(3 * (x_mat)^2 + (y_mat-3)^2)))


p1 <- plot_matrix(f_0) +
  geom_contour(aes(z=z), colour = "white") +
  ggtitle("Single minima example")


p2 <-plot_matrix(f_2) +
  geom_contour(aes(z=z), colour = "white") +
  ggtitle("Local and global minima example")

p1 / p2


# Gradient descent for simple polynomial ----------------------------------

poly_1 <- function(x, y)
{
  (x^2) + (3 * y^2)
}

f_quad <- poly_1(x_mat, y_mat)

n_steps <- 10
sol_x <- rep(NA, n_steps+1) # Guess for x at step j
sol_y <- rep(NA, n_steps+1) # Guess for y at step j
sol_f <- rep(NA, n_steps+1) # Value for f at step j

# Initial guesses
sol_x[1] <- 3
sol_y[1] <- 2
sol_f[1] <- poly_1(3,2)

for (j in 1:n_steps)
{
  # Optimal size of step, calculated by differentiation of F wrt delta
  this_delta <- (sol_x[j]^2 + 9*sol_y[j]^2) / (2 * sol_x[j]^2 + 51 * sol_y[j]^2)

  # Move in direction of min gradient
  sol_x[j+1] <- (1-2*this_delta) * sol_x[j]
  sol_y[j+1] <- (1-6*this_delta) * sol_y[j]
  sol_f[j+1] <- poly_1(sol_x[j+1], sol_y[j+1])

  # Check for convergence
  if (abs(sol_f[j+1] - sol_f[j]) < 1e-6)
  {
    break
  }
}

gd_progress <- data.frame(
  x = sol_x,
  y = sol_y) %>%
  mutate(step = 1:n())

p <- plot_matrix(f_quad, x_val = x, y_val = y) +
  geom_path(data = gd_progress) +
  geom_point(data = gd_progress) +
  transition_reveal(along = step)

animate(p, height = 800, width = 800)



# Using interpolation -------------------------------------------------------------

h <- 0.1
x <- seq(-6, 6, by = h)
y <- x

x_mat <- matrix(rep(x, length(y)), byrow = TRUE, nrow = length(y))
y_mat <- matrix(rep(y, length(x)), byrow = FALSE, nrow = length(x))

poly_2 <- function(x, y)
{
  f <- 1.5 - 1.6 * exp(-0.05 * ((3 * (x + 3)^2) + (y+3)^2))
  f_2 <- f + (0.5 - exp(-0.1*(3 * (x-3)^2 + (y-3)^2)))
  
  return (f_2)
}

f <- poly_2(x_mat, y_mat)
f_grad <- gradient(f,h,h)

delsearch <- function(x, this_x, this_y, dfx, dfy, x_mat, y_mat, fun)
{
  x_0 <- this_x - x * dfx
  y_0 <- this_y - x * dfy
  
  return  (interp2(x_mat,y_mat,fun, x_0, y_0))
}

# Run through a interpolation based optimisation based on starting conditions
get_one_optimisation <- function(x_start, y_start)
{
  n_steps <- 100
  sol_x <- rep(NA, n_steps+1) # Guess for x at step j
  sol_y <- rep(NA, n_steps+1) # Guess for y at step j
  sol_f <- rep(NA, n_steps+1) # Value for f at step j
  
  # Starting vals
  sol_x[1] <- x_start
  sol_y[1] <- y_start
  sol_f[1] <- interp2(x,y, f, sol_x[1], sol_y[1])
  
  # Derivatives in x and y direction
  df_dx <- interp2(x,y, f_grad$X, sol_x[1], sol_y[1])
  df_dy <- interp2(x,y, f_grad$Y, sol_x[1], sol_y[1])
  
  for (j in 1:n_steps)
  {
    delta <- optimize(f = delsearch, this_x = sol_x[j], this_y = sol_y[j], dfx = df_dx, dfy = df_dy, x_mat = x, y_mat = y, fun = f, lower = 0, upper = 10)$objective
    cat(paste("Delta = ", delta, "\n"))
    
    sol_x[j+1] <- sol_x[j] - delta * df_dx
    sol_y[j+1] <- sol_y[j] - delta * df_dy
    sol_f[j+1] <- interp2(x,y, f, sol_x[j+1], sol_y[j+1])
    
    df_dx <- interp2(x,y, f_grad$X, sol_x[j+1], sol_y[j+1])
    df_dy <- interp2(x,y, f_grad$Y, sol_x[j+1], sol_y[j+1])
    
    cat(paste("df_dx =", df_dx, "df_dy =", df_dy, "\n"))
    
    if (abs(sol_f[j+1] - sol_f[j]) < 1e-6)
      break
  }
  
  return (data.frame(
    x = sol_x,
    y = sol_y) %>% 
      mutate(step = 1:n(),
             starting_value = paste0("[", x_start, ",", y_start, "]")))
}

starting_values <- data.frame(x_start = c(4,0,-5), y_start = c(0,-5,2))

results <- pmap_dfr(starting_values, get_one_optimisation)

# Fill down last
results <- results %>% 
  group_by(starting_value) %>% 
  fill(x, y)

p <- plot_matrix(f, x_val = x, y_val = y) + 
  geom_contour(aes(z=z), colour = "white") +
  geom_path(aes(linetype = starting_value), data = results) + 
  geom_point(aes(shape = starting_value), size = 5, data = results) + 
  transition_reveal(along = step)

animate(p, height = 800, width = 800)


# Alternating Descent -----------------------------------------------------

get_one_optimisation_ad <- function(x_start, y_start)
{
  n_steps <- 3
  sol_x <- rep(NA, n_steps) # Guess for x at step j
  sol_y <- rep(NA, n_steps) # Guess for y at step j
  
  # Starting vals
  sol_x[1] <- x_start
  sol_y[1] <- y_start
  
  # Find the minimum in y direction with x fixed at initial value
  fixed_x <- interp2(x, y, f, rep(sol_x[1], length(y)), y)
  y_index <- which.min(fixed_x)
  
  sol_x[2] <- x_start
  sol_y[2] <- y[y_index]
  
  # Now the minimum in x direction with out previous y
  fixed_y <- interp2(x, y, f, x, rep(sol_y[2], length(x)))
  x_index <- which.min(fixed_y)
  
  sol_x[3] <- x[x_index]
  sol_y[3] <- sol_y[2]
  sol_f[3] <- f[x_index, y_index]
  
  return (data.frame(
    x = sol_x,
    y = sol_y) %>% 
      mutate(step = 1:n(),
             starting_value = paste0("[", x_start, ",", y_start, "]")))
}

results <- pmap_dfr(starting_values, get_one_optimisation_ad)

# Fill down last
results <- results %>% 
  group_by(starting_value) %>% 
  fill(x, y)

p2 <- plot_matrix(f, x_val = x, y_val = y) + 
  geom_contour(aes(z=z), colour = "white") +
  geom_path(aes(linetype = starting_value), data = results) + 
  geom_point(aes(shape = starting_value), size = 5, data = results) + 
  transition_reveal(along = step)

animate(p2, height = 800, width = 800)
