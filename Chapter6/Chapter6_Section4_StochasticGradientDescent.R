library(tidyverse)
library(pracma)
library(rgl)
library(gganimate)
source("Misc/Utils.R")

x <- seq(-6, 6, by = 0.1)
y <- x
n <- length(x)

xy_grid <- expand.grid(x, y)

f_1 <- 1.5 - 1.6 * exp(-0.05 * (3* (xy_grid[,1]+3)^2 + (xy_grid[,2]+3)^2))
f <- f_1 + (0.5 - exp(-0.1 * (3 * (xy_grid[,1]-3)^2 + (xy_grid[,2]-3)^2)))


# Pre-calc all of the gradients
f_mat <- matrix(f, ncol = n, byrow = FALSE)
df <- gradient(f_mat, h1 = 0.1, h2 = 0.1)
df_x <- df$X
df_y <- df$Y

# Plotting
f_df <- data.frame(x = xy_grid[,1],
                   y = xy_grid[,2],
                   z = f)

# Contour plot
ggplot(f_df, aes(x, y, z = z)) + 
  geom_contour()

# Surface plot
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(f_mat, nbcol)
persp3d(x, y, f_mat, col = color[zcol])


run_one_sgd <- function(x_init, y_init, f_mat, df_x, df_y, x, y, tau = 2, batch_size = 100)
{
  this_x <- x_init
  this_y <- y_init
  label <- paste0("(", x_init, ",", y_init, ")")
  
  results <- NULL
  last_f <- Inf
  
  for (i in 1:50)
  {
    # Sample from full space
    x_i <- sort(sample(1:nrow(f_mat), batch_size))
    y_i <- sort(sample(1:ncol(f_mat), batch_size))
    
    # Use interpolation to find the closest values for f, df_dx and df_dy on the restricted set.
    this_f <- my_interp2(x[x_i], y[y_i], f_mat[x_i, y_i], this_x, this_y)
    this_dfx <- my_interp2(x[x_i], y[y_i], df_x[x_i, y_i], this_x, this_y)
    this_dfy <- my_interp2(x[x_i], y[y_i], df_y[x_i, y_i], this_x, this_y)
    

    results <- rbind(results, data.frame(x = this_x, y = this_y, f = this_f, step = i, init_cond = label))
    
    # Update x,y
    this_x <- this_x - tau * this_dfx
    this_y <- this_y - tau * this_dfy
    

    # Check for convergence
    if (abs(this_f - last_f) <= 1e-6)
    {
      break
    }
    
    last_f = this_f
  }
  
  return (results)
}

initial_conditions <- tribble(
  ~x_init, ~y_init,
   4, 0,
   0, -5,
  -5, 2
)

sgd_res <- pmap_dfr(initial_conditions, run_one_sgd, f_mat, df_x, df_y, x, y)

p <- plot_matrix(f_mat, x_val = x, y_val = y) + 
  geom_contour(aes(z=z), colour = "white") + 
  geom_path(aes(linetype = init_cond), data = sgd_res) + 
  geom_point(aes(shape = init_cond), size = 5, data = sgd_res) + 
  transition_reveal(along = step)

animate(p, height = 800, width = 800)
