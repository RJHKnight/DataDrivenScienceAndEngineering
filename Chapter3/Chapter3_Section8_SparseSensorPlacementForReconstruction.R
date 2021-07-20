library(tidyverse)
library(rsvd)
library(rmatio)
library(RSpectra)
library(imager)
source("Misc/Utils.R")

faces_mat <- read.mat("Data/allFaces.mat")
x <- faces_mat$faces
r <- 100 
p <- 100

# SVD Decomposition with r modes
svd_res <- svds(x, k = 100)
psi <- svd_res$u
qr_res <- qr(psi, LAPACK = TRUE)

# Optimal
c <- matrix(0, nrow = p, ncol = nrow(x))

for (j in 1:p)
{
  c[j,qr_res$pivot[j]] <- 1
}

theta <- c %*% psi

# First face samples at p points
y <- as.matrix(x[qr_res$pivot[1:p], 1], ncol = 1)
a <- ml_divide(theta, y)
face_recon_optim <- svd_res$u[,1:r] %*% a

# Random
raw_c <- rep(0, p * nrow(x))
raw_c[sample(1:length(raw_c), r)] <- 1
c <- matrix(raw_c, nrow = p, ncol = nrow(x))

theta <- c %*% psi

# First face samples at p points
y <- as.matrix(x[qr_res$pivot[1:p], 1], ncol = 1)
a <- ml_divide(theta, y)
face_recon_random <- svd_res$u[,1:r] %*% a

par(mfrow = c(2, 2), mar = rep(1, 4))
plot(get_one_image(x[,1]), main = "Original", axes = FALSE)
plot(get_one_image(face_recon_optim), main = "Optimal", axes = FALSE)
plot(get_one_image(face_recon_random), main = "Random", axes = FALSE)