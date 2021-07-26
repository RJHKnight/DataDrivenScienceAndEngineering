library(tidyverse)
library(gganimate)

n <- 150
train_index <- 1:100
test_index  <- 101:150

# Group 1: Random ellipse centered at (0,0)
x_1 <- rnorm(n)
y_1 <- 0.5 * rnorm(n)

# Group 2: Random ellipse centered at (1,-2) and rotated by theta
x_2 <- rnorm(n) + 1
y_2 <- (0.2 * rnorm(n)) - 2

theta <- pi/4

# Rotation matrix
rot <- matrix(c(cos(theta), -sin(theta),
                sin(theta), cos(theta)),
                nrow = 2, byrow = TRUE)

xy_2 <- rot %*% rbind(x_2, y_2)
x_2 <- xy_2[1,]
y_2 <- xy_2[2,]

xy_df <- rbind(
  data.frame(x = x_1, y = y_1, class = "1"),
  data.frame(x = x_2, y = y_2, class = "2")
)

ggplot(xy_df, aes(x, y, col = class)) + 
  geom_point()

x_train <- c(x_1[train_index], x_2[train_index])
x_test  <- c(x_1[test_index],  x_2[test_index])
y_train <- c(y_1[train_index], y_2[train_index])
y_test  <- c(y_1[test_index],  y_2[test_index])

xy_train <- cbind(x_train, y_train)

# KMeans ------------------------------------------------------------------

# Simple implementation of a k-means with k==2
my_kmeans <- function(initial_guess_1, initial_guess_2, xy, num_iterations = 4)
{
  res_df <- NULL
  centers <- NULL
  c_1 <- initial_guess_1
  c_2 <- initial_guess_2
   
  for (i in 1:num_iterations)
  {
    classification <- rep(NA, nrow(xy))
    # Assign each point to cluster 1 or 2, based in L2 distance
    for (j in 1:nrow(xy))
    {
      dist_1 <- sqrt(sum((c_1 - xy[j,])^2))
      dist_2 <- sqrt(sum((c_2 - xy[j,])^2))
      
      classification[j] <- ifelse(dist_1 > dist_2, 2, 1)
    }
    
    # Add diagnostics
    this_df <- data.frame(x = xy[,1], y = xy[,2], classification = classification, step = i)
    res_df <- rbind(res_df, this_df)
    
    # Update centers
    next_values <- this_df %>% 
      group_by(classification) %>% 
      summarise(
        x = mean(x),
        y = mean(y)
      )
    
    centers <- rbind(centers, data.frame(x = c(c_1[1], c_2[1]), y = c(c_1[2], c_2[2]), classification = c("1", "2"), step = i))
    
    c_1 <- c(next_values$x[1], next_values$y[1])
    c_2 <- c(next_values$x[2], next_values$y[2])
  }
  
  return (list(centers = centers, diagnostics = res_df))
}

k_means_res <- my_kmeans(c(-1,0), c(1,-0.5), xy_train)

# Calculate intercept and gradient of the separation line
separation_line <- k_means_res$centers %>% 
  group_by(step) %>% 
  arrange(classification) %>% 
  summarise(
    mid_x = (x[2] + x[1]) / 2,
    mid_y = (y[2] + y[1]) / 2,
    slope = (y[2] - y[1]) / (x[2] - x[1]),
    b     = mid_y + (1/slope) * mid_x 
  )


ggplot(k_means_res$diagnostics, aes(x,y, colour = as.factor(classification))) + 
  geom_point() + 
  geom_point(data = k_means_res$centers, colour = "black", size = 5, shape = 1) + 
  geom_abline(data = separation_line, aes(intercept = b, slope = -1/slope)) + 
  facet_wrap(~ step)