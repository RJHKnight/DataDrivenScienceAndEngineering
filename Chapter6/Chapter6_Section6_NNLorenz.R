library(deSolve)
library(rgl)
library(tidyverse)

# Simulate Lorenz attractor.
lorenz <- function(t, state, parms)
{
  with(as.list(c(state, parms)),
  {
    return (list(c(
       sigma * (y - x),
       (rho * x) - (x * z) - y,
       x * y - beta * z
    )))
  })
}

# Generate 100 training trajectories

params <- c(
rho = 28,
sigma = 10,
beta = 8/3)

training_df <- NULL

for (i in 1:100)
{
  x_0 <- 30 * (runif(3) - 0.5)
  names(x_0) <- c("x", "y", "z")
  
  this_traj <- ode(x_0, seq(0,8, by = 0.01), lorenz, params, method = "ode45")
  
  label <- paste(sprintf(x_0, fmt = '%#.2f'), collapse = ",")
  this_df <- data.frame(this_traj)
  this_df$label <- label
  
  training_df <- rbind(training_df, this_df)
}

# Plot training data.
nbcol = 100
colour = rev(rainbow(nbcol, start = 0/6, end = 4/6))
plot_cols <- colour[match(training_df$label, unique(training_df$label))]

plot3d(training_df$x, training_df$y, training_df$z, 
       col = plot_cols)


# Prepare data
x_train <- training_df %>% 
  group_by(label) %>% 
  slice(n = 1:(n()-1)) %>%  # remove last value per group
  ungroup() %>% 
  select(x,y,z) %>% 
  as.matrix()
  
y_train <- training_df %>% 
  group_by(label) %>% 
  slice(n = 2:(n())) %>%  # remove first value per group
  ungroup() %>% 
  select(x,y,z) %>% 
  as.matrix()

# Network structure
model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "sigmoid", input_shape = 3) %>% 
  layer_dense(units = 10, activation = "sigmoid") %>%
  layer_dense(units = 10, activation = "linear") %>% 
  layer_dense(unit = 3, activation =  "linear")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_sgd(),
  metrics = list("mean_absolute_error")
)

model %>% fit(
  x_train, y_train,
  batch_size = 2^10,
  epochs = 1000,
  validation_split = 0.2
)

# Compare actual vs NN for 10 random runs

for (i in 1:10)
{
  x_0 <- 30 * (runif(3) - 0.5)
  names(x_0) <- c("x", "y", "z")
  
  actual <- ode(x_0, seq(0,8, by = 0.01), lorenz, params, method = "ode45")

}