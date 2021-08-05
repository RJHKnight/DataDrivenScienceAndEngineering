library(deSolve)
library(rgl)
library(tidyverse)
library(keras)

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

log_sig <- function(x)
{
  k_log(1 /(1+ k_exp(-x)))
}

rad_bas <- function(x)
{
   k_exp(k_pow(-x,2))
}

# Network structure
model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = log_sig, input_shape = 3) %>% 
  layer_dense(units = 10, activation = rad_bas) %>%
  layer_dense(units = 10, activation = "linear") %>% 
  layer_dense(unit = 3, activation =  "linear")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae")
)

model %>% fit(
  x_train/65, y_train/65,
  epochs = 1000,
  batch_size = 32
)

save_model_tf(model, filepath = "Data/lorenzmodel")

# If you don't want to train - you can load the model using:
#model <- load_model_tf("Data/lorenzmodel/")

# Compare actual vs NN 
x_0 <- 30 * (runif(3) - 0.5)
names(x_0) <- c("x", "y", "z")

num_pred <- 50
actual <- ode(x_0, seq(0,8, by = 0.01), lorenz, params, method = "ode45")
predictions <- matrix(NA, nrow = num_pred, ncol = 3)
predictions[1,] <- x_0/65

for (j in 2:num_pred)
{
  cat(paste("Running for ", j, "\n"))
  predictions[j,] <- predict(model, matrix(predictions[j-1,], nrow = 1))
}


colnames(actual) <- c("t", "x_actual", "y_actual", "z_actual")
colnames(predictions) <- c("x_pred", "y_pred", "z_pred")

res <- cbind(data.frame(actual[1:num_pred,]),
             data.frame(65*predictions)) %>% 
  pivot_longer(-t, names_to = c("var", "type"), names_sep = "_", values_to = "val")


ggplot(res, aes(t, val, colour = type)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ var, ncol = 1, scales = "free_y") + 
  theme_bw()