
# Randomised Singular Value Decomposition
# 
# x : the matrix to be decomposed
# r : the desired rank
# q : the number of power iterations
# p : the number of oversampling ranks
rsvd <- function(x, r, q, p)
{
  # Sample column space of x with a random projection matrix
  n <- ncol(x)
  proj_mat <- matrix(rnorm(n * (r+p)), ncol = r+p)
  z <- x %*% proj_mat
  
  for (k in 1:q)
  {
    z <- x %*% (t(x) %*% z)
  }
  
  qr_res <- qr(z)
  
  # Compute SVD of Y = Q' * x
  y <- t(qr.Q(qr_res)) %*% x
  svd_res <- svd(y)
  
  # Adjust U
  u <- qr.Q(qr_res) %*% svd_res$u
  
  return (list(d = svd_res$d, u = u, v = svd_res$v))
  
}