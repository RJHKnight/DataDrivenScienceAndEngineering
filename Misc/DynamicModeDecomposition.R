source("Misc/Utils.R")

dmd <- function(x, x_prime, r)
{
  # Step 1: Rank 'r' SVD decomposition of x
  svd_res <- svd(x)
  sigma_1 <- ginv(diag(svd_res$d[1:r])) # Pre calc 1/sigma
  u_r <- svd_res$u[, 1:r]
  v_r <- svd_res$v[, 1:r]
  
  # Step 2: Calculate the reduced rank best fit (L2) operator that advances X -> X'
  a_tilda <- t(u_r) %*% x_prime %*% (v_r %*% sigma_1)
  
  # Step 3: Spectral decomposition of a_tilda - use QZ decomposition to match matlab results.
  eig_a <- QZ::geigen(a_tilda)
  
  # Step 4: Reconstruct DMD modes (phi)
  phi <- x_prime %*% (v_r %*% sigma_1) %*% eig_a$V
  
  # Step 5: Calculate b, the amplitude of the DMD Modes
  alpha <- diag(svd_res$d[1:r]) %*% v_r[1,]
  b <- ml_divide((eig_a$V %*% diag(eig_a$W)), alpha)
  
  return(list(
   phi = phi,
   lambda = diag(eig_a$W),
   b = b
  ))
}