library(tidyverse)
library(rmatio)
library(imager)
library(CVXR)
library(patchwork)
source("Misc/Utils.R")


# Preparation of testing and training -------------------------------------
all_faces <- rmatio::read.mat("Data/allFaces.mat")
x <- all_faces$faces
n_faces <- all_faces$nfaces

# Build training and testing sets
n_train  <- 30
n_test   <- 20
n_people <- 20

# For the 'n' people we are interested in, take the first 30 pictures as training
# and the next 20 as testing

train <- matrix(nrow = nrow(x), ncol = n_train * n_people)
test <- matrix(nrow = nrow(x), ncol = n_test * n_people)

for (i in 1:n_people)
{
  offset <- sum(n_faces[0:(i-1)])
  
  # Training set
  train_col_start <- ((i-1) * n_train) + 1
  train_col_end <- train_col_start + n_train - 1
  train[,train_col_start:train_col_end] <- x[,(offset+1):(offset+n_train)]
  
  # Testing set
  test_col_start <- ((i-1) * n_test) + 1
  test_col_end <- test_col_start + n_test - 1
  test[,test_col_start:test_col_end] <- x[,(offset+n_train+1):(offset+n_train+n_test)]
}

# Downsample images to build theta
m <- ncol(train)
theta <- matrix(nrow = 120, ncol = m)
norm_theta <- rep(0, m)

downsize_one <- function(x)
{
  x_img <- as.cimg(matrix(x, nrow = 192))
  x_img_resized <- resize(x_img, size_x = 12, size_y = 10, interpolation_type = 6)
  return (as.vector(x_img_resized))
}

for (i in 1:m)
{
  this_theta <- downsize_one(train[,i])
  norm_theta[i] <- sqrt(sum(this_theta^2))
  theta[,i] <- this_theta / norm_theta[i]
}

# Test image - person 7, image 6
x1 <- test[,126]
mustache <- load.image("data/mustache.jpg")
mustache <- as.vector(t(as.matrix(grayscale(mustache))))
x2 <- x1 * mustache

# 30% corrupted
corrupt_index <- sample(1:length(x1), floor(0.3 * length(x1)))
x3 <- x1
x3[corrupt_index] <- floor(255 * runif(length(corrupt_index)))

# Random noise on all points
x4 <- x1 + (50 * rnorm(length(x1)))

test_images <- cbind(x1, x2, x3, x4)
y <- matrix(nrow = 120, ncol = ncol(test_images))

for (i in 1:ncol(test_images))
{
  y[,i] <- downsize_one(test_images[,i])
}


# Plotting functions
plot_s_and_error <- function(s, this_x)
{
  s_df <- data.frame(j = 1:length(s), s = s)
  s_plot <- ggplot(s_df, aes(j, s)) + 
    geom_col() +
    theme_bw() 
  
  bin_error = rep(0, n_people)
   
  for (i in 1: n_people)
  {
    index <- ((i-1) * n_train + 1) : (i * n_train)
    bin_error[i] <- norm(this_x - train[,index] %*% (s[index]/norm_theta[index]), "2") / norm(this_x, "2")
  }
  
  bin_error_df <- data.frame(person = 1:n_people, error = bin_error) %>% 
    mutate(best_fit = error == min(error))
  
  error_plot <- ggplot(bin_error_df, aes(person, error, fill = best_fit)) + 
    geom_col() + 
    theme_bw() + 
    theme(legend.position = "none")
  
  return (s_plot / error_plot)
}

plot_images <- function(orignal, downsampled, s)
{
  par(mfrow = c(2, 2), mar = rep(1, 4))
  
  plot(get_one_image(orignal), main = "Original", axes = FALSE)
  plot(get_one_image(downsampled, n_row = 12), main = "Downsampled", axes = FALSE)
  plot(get_one_image(train %*% (s/norm_theta)), main = "Reconstruction", axes = FALSE)
  plot(get_one_image(orignal - train %*% (s/norm_theta)), main = "Error", axes = FALSE)
}


# Searching for clean image -----------------------------------------------
this_y <- y[,1]
epsilon <- 0.01

s_var <- Variable(m)
objective <- Minimize(norm(s_var, "1"))
constraint <- norm(theta %*% s_var - this_y, "2")  <= epsilon
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
s <- result$getValue(s_var)


# Plots
plot_images(x1, this_y, s)
plot_s_and_error(s, x1)


# Searching for image with mustache ---------------------------------------
this_y <- y[,2]
epsilon <- 500

s_var <- Variable(m)
objective <- Minimize(norm(s_var, "1"))
constraint <- norm(theta %*% s_var - this_y, "2")  <= epsilon
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
s <- result$getValue(s_var)

# Plots
plot_images(x2, this_y, s)
plot_s_and_error(s, x2)



# Searching for occlusion image -------------------------------------------
this_y <- y[,3]
epsilon <- 500

s_var <- Variable(m)
objective <- Minimize(norm(s_var, "1"))
constraint <- norm(theta %*% s_var - this_y, "2")  <= epsilon
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
s <- result$getValue(s_var)

# Plots
plot_images(x3, this_y, s)
plot_s_and_error(s, x3)


# White noise image -------------------------------------------------------
this_y <- y[,4]
epsilon <- 10

s_var <- Variable(m)
objective <- Minimize(norm(s_var, "1"))
constraint <- norm(theta %*% s_var - this_y, "2")  <= epsilon
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
s <- result$getValue(s_var)

# Plots
plot_images(x4, this_y, s)
plot_s_and_error(s, x4)


