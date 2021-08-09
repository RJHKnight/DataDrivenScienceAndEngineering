# Copyright 2015, All Rights Reserved
# Code by Steven L. Brunton
# For Paper, "Discovering Governing Equations from Data: 
#        Sparse Identification of Nonlinear Dynamical Systems"
# by S. L. Brunton, J. L. Proctor, and J. N. Kutz
source("Misc/Utils.R")

sparsify_dynamics <- function(theta, dxdt, lambda)
{
  # Initial guess, least squares
  x_i <- coef(lm(dxdt ~ -1 +theta))
  n <- ncol(dxdt)
  
  for (j in 1:10)
  {
    # Identify coefficients with small values and set them to zero
    small_inds <- abs(x_i) < lambda
    x_i[small_inds] <- 0
    big_inds <- !small_inds
    
    for (j in 1:n)
    {
      this_bid_ind <- big_inds[,j]
      x_i[this_bid_ind, j] <- coef(lm(dxdt[,j] ~ -1 + theta[,this_bid_ind]))
    }
  }
  
  rownames(x_i) <- colnames(theta)
  colnames(x_i) <- colnames(dxdt)
  return (x_i)
}