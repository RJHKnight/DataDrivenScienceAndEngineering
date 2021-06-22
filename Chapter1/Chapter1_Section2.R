library(imager)
library(jpeg)

A <- load.image("data/dog.jpg")

# as.matrix returns the matrix in column then row order - we need to transpose to get alignment correct.
X <- t(as.matrix(grayscale(A)))

# In R, this will return a list containing:
# * d - a vector of the diagonal values of sigma
# * u - The full (m x m) U matrix
# * v - The full (n x n) V matrix
svd_decomp <- svd(X)

get_truncated_matrix <- function(decomp, rank)
{
  # Approximation is U_r * sigma_r * V_r* 
  x_approx <- decomp$u[,1:rank] %*% diag(decomp$d[1:rank]) %*% t(decomp$v[, 1:rank])
  
  return (t(x_approx))
}

# 4 Subplots
par(mfrow = c(2, 2))

plot(as.cimg(t(X)), main = "Original", axes = false)
plot(as.cimg(get_truncated_matrix(svd_decomp, 5)), main = "Rank 5", axes = false)
plot(as.cimg(get_truncated_matrix(svd_decomp, 20)), main = "Rank 20", axes = false)
plot(as.cimg(get_truncated_matrix(svd_decomp, 100)), main = "Rank 50", axes = false)