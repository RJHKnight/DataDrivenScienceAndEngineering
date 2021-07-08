library(tidyverse)
library(imager)

source("Misc/RSVD.R")

a <- load.image("data/jupiter.jpg")

# as.matrix returns the matrix in column then row order - we need to transpose to get alignment correct.
x <- t(as.matrix(grayscale(a)))

# Deterministic SVD
svd_res <- svd(x)

r <- 400 # target rank
q <- 1   # power iterations
p <- 5   # oversampling 

svdr_res <- rsvd(x, r, q, p)

# Reconstruction
x_svd      <- svd_res$u[,1:r] %*% diag(svd_res$d[1:r]) %*% t(svd_res$v[, 1:r])
err_x_svd  <- norm(x - x_svd, type = "2") / norm(x, type = "2")
x_svdr     <- svdr_res$u[,1:r] %*% diag(svdr_res$d[1:r]) %*% t(svdr_res$v[, 1:r])   
err_x_svdr <- norm(x - x_svdr, type = "2") / norm(x, type = "2")

# Plot
par(mfrow = c(1, 3), mar = c(1,0,1,0))

plot(as.cimg(t(x)), main = "Original", axes = FALSE)  
plot(as.cimg(t(x_svd)), main = "Det. SVD", axes = FALSE)  
plot(as.cimg(t(x_svdr)), main = "Rand. SVD", axes = FALSE)  


# Power iterations
x <- matrix(rnorm(1000 * 100), nrow = 1000)

svd_res <- svd(x)

# Dummy example with linearly decaying sigma
s <- seq(1, 0.01, -0.01)
x <- svd_res$u %*% diag(s) %*% t(svd_res$v)

create_sigma_df <- function(s, i)
{
  sigma_df <- data.frame(s = s) %>% 
    mutate(j = 1:n()) %>% 
    mutate(iteration = i)
}

y <- x

sigma_res <- create_sigma_df(s, 0)

for (q in 1:15)
{
  y <- t(x) %*% y
  y <- x %*% y
  this_svd_res <- svd(y)
  
  this_sigma <- create_sigma_df(this_svd_res$d, q)
  sigma_res <- rbind(sigma_res, this_sigma)
  
}

sigma_res %>% 
  mutate(iteration = as.factor(iteration)) %>% 
  ggplot(aes(j, s, colour = iteration)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  xlab("Rank") + 
  ylab("Sigma") +
  ggtitle("Impact of power iteration on sigma decay.")
