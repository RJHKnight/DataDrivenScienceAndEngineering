source("Misc/Utils.R")

dmd <- function(x, x_prime, r)
{
  # Step 1: Rank 'r' SVD decomposition of x
  svd_res <- RSpectra::svds(x, r)
  
  # Step 2: Calculate the reduced rank best fit (L2) operator that advances X -> X'
  a_tilda <- t(svd_res$u) %*% x_prime %*% svd_res$v / svd_res$d
  
  # Step 3: Spectral decomposition of a_tilda
  eig_a <- eigen(a_tilda)
  
  # Step 4: Reconstruct DMD modes (phi)
  phi <- x_prime %*% (svd_res$v / svd_res$d) %*% eig_a$vectors
  
  # Step 5: Calculate b, the amplitude of the DMD Modes
  alpha <- svd_res$d %*% t(svd_res$v[1,])
  b <- ml_divide((eig_a$vectors %*% eig_a$values), alpha)
  
  return(list(
   phi = phi,
   lambda = eig_a$values,
   b = b
  ))
}