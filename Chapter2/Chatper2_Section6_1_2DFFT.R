library(tidyverse)
library(imager)
source("Misc/Utils.R")

A <- load.image("data/dog.jpg")
X <- as.matrix(grayscale(A))

plot(as.cimg(X))

# Row wise FFT
fft_2d <- matrix(NA, nrow = nrow(X), ncol = ncol(X))

for (j in 1:nrow(X))
{
  fft_2d[j,] <- fft(X[j,])
}

plot_matrix(log(Re(t(apply(fft_2d, 1, fftshift)))))

# Col wise FFT
for (j in 1:ncol(fft_2d))
{
  fft_2d[,j] <- fft(fft_2d[,j])
}

plot_matrix(log(Re(fftshift(fft_2d))))

# Or in 1 go... (no need for the fft2 command in R)
y <- fft(X)
plot_matrix(log(Re(fftshift(y))))
