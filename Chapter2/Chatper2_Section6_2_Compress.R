library(tidyverse)
library(imager)
source("Misc/Utils.R")

a <- load.image("data/dog.jpg")
x <- as.matrix(grayscale(A))

b_t <- fft(x)


par(mfrow = c(2, 2), mar = rep(1, 4))

# Keep only the top 10%, 5%, 1% and 0.2%
for (threshold in c(0.1, 0.05, 0.01, 0.002))
{
  this_cutoff <- quantile(abs(b_t), 1-threshold)
  this_fft <- b_t
  
  # Set values below the threshold to 0
  this_fft[which(abs(this_fft) < this_cutoff, arr.ind = TRUE)] <- 0
  
  # Plot the resultant image
  plot(as.cimg(Re(fft(this_fft, inverse = TRUE))), main = paste("Threshold = ", threshold), axes = FALSE)
}