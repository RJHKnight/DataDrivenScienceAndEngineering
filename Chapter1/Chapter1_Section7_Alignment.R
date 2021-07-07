library(tidyverse)
library(imager)
source("Misc/Utils.R")

# Square matrix with "aligned" square of 1's in the center
n <- 1000

x <- matrix(0, nrow = n, ncol = n)
x[seq(n/4, 3 * n/4), seq(n/4, 3 * n/4)] <- 1

plot_matrix(x)

svd_res <- svd(x)

extract_sv_df <- function(svd_res, angle)
{
  data.frame(s = svd_res$d) %>% 
    mutate(r = 1:n()) %>% 
    mutate(angle = angle)
}

sigma_vals <- extract_sv_df(svd_res, 0)

x_img <- as.cimg(x)

# Store the Singular Values for rotations between 1 and 44 degrees.
for (j in seq(1,44, by = 4))
{
  y <- as.matrix(imrotate(x_img, j))
  this_svd <- svd(y)
  
  this_sigma <- extract_sv_df(this_svd, j)
  
  sigma_vals <- rbind(sigma_vals, this_sigma)
}


ggplot(sigma_vals, aes(r, s, colour = as.factor(angle))) + 
  geom_point() + 
  geom_line() + 
  scale_y_log10()