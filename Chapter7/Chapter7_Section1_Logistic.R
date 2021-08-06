library(tidyverse)
source("Misc/Utils.R")

logistic <- function(x, r)
{
  r * x * (1-x)
}

# Different values for r
r <- seq(1, 4, by = 0.0025)

# Initial conditions
xss <- 0.5

for (i in 1:10000)
{
  xss <- logistic(xss, r)
}

# Define matrix to hold the trajectory
x_mat <- matrix(NA, ncol = length(xss), nrow = 1001)
x <- xss
x_mat[1,] <- xss

for (i in 2:1001)
{
  x <- logistic(x, r)
  
  x[abs(x-xss) < 0.001] <- NA
  
  x_mat[i,] <- x
}

x_df <- data.frame(x_mat)
colnames(x_df) <- r
x_df$iteration <- 1:nrow(x_df)

x_df_long <- x_df %>% 
  pivot_longer(-iteration, names_to = "beta", values_to = "x") %>% 
  filter(!is.na(x)) %>% 
  mutate(beta = as.numeric(beta))

# Beta vs x
ggplot(x_df_long, aes(beta, x)) + 
  geom_point(size = 0.01) + 
  theme_bw()  

# x vs Beta
ggplot(x_df_long, aes(x, beta)) + 
  geom_point(size = 0.01) + 
  scale_y_reverse() +
  theme_bw()  

# Zoom of beta 3.45 -> 4
ggplot(filter(x_df_long, beta > 3.45), aes(x, beta)) + 
  geom_point(size = 0.01) + 
  scale_y_reverse() +
  theme_bw()  

