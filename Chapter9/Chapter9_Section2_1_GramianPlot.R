library(tidyverse)
library(control)
library(MASS)
library(pracma)
library(plotrix)
source("Misc/Gramian.R")
source("Misc/Balred.R")

a <- matrix(c(-0.75, 1, -0.3, -0.75), ncol = 2, byrow = TRUE)
b <- matrix(c(2, 1), ncol = 1)
c <- matrix(c(1,2), ncol = 2)
d <- 0

w_c <- ctrl_gram(a, b)

w_o <- obs_gram(a, c)

# Using balred
bal_res <- balred(a, b, c, d, nrow(b))

# Check gramians
all.equal(ctrl_gram(bal_res$a, bal_res$b), obs_gram(bal_res$a, bal_res$c))
 
# Manually compute Balanced Transformation
res <- eigen(w_c %*% w_o)

a_tu <- ginv(res$vectors) %*% a %*% res$vectors
b_tu <- ginv(res$vectors) %*% b
c_tu <- c %*% res$vectors

# Diagonal but not equal
sigma_c <- ctrl_gram(a_tu, b_tu)
sigma_o <- obs_gram(a_tu, c_tu)
sigma_s <- diag(sigma_c) * diag(MASS::ginv(sigma_o))

t <- res$vectors %*% diag(sigma_s) ^ (1/4)
sigma <- diag(sigma_c) ^ (1/2) * diag(sigma_o)^(1/2)

# Order in descending sigma
t <- t[order(-sigma),]

# Compute Balanced system
a_t <- ginv(t) %*% a %*% t
b_t <- ginv(t) %*% b
c_t <- c %*% t

# Check gramians
all.equal(ctrl_gram(a_t, b_t), obs_gram(a_t, c_t))


# Plotting
theta = seq(0, 2*pi, 0.01)

circle_df <- data.frame(theta = theta) %>% 
  mutate(x = cos(theta), y = sin(theta)) %>% 
  mutate(type = "circle")

control_mat <- t(sqrt(w_c) %*% rbind(circle_df$x, circle_df$y))
obs_mat <- t(sqrt(w_o) %*% rbind(circle_df$x, circle_df$y))

bw_c <- ctrl_gram(a_t, b_t)
# Remove the small values
bw_c[1,2] <- 0
bw_c[2,1] <- 0
bal_mat <- t(ginv(t) %*% sqrt(bw_c) %*% t %*% rbind(circle_df$x, circle_df$y)) * c(-1, 1)

plot_df <- rbind(circle_df,
                 data.frame(theta = theta, x = control_mat[,1], y = control_mat[,2], type = "control"),
                 data.frame(theta = theta, x = obs_mat[,1], y = obs_mat[,2], type = "obs"),
                 data.frame(theta = theta, x = bal_mat[,1], y = bal_mat[,2], type = "balanced"))

ggplot(plot_df, aes(x, y, colour = type)) + 
  geom_point() + 
  theme_bw()
