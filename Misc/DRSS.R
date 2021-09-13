# Code from Python control module.
drss <- function(states, outputs, inputs, type = "D", strictly_proper=FALSE)
{
  # Probability of repeating a previous root.
  p_repeat <- 0.05
  
  # Probability of choosing a real root.  Note that when choosing a complex
  # root, the conjugate gets chosen as well.  So the expected proportion of
  # real roots is pReal / (pReal + 2 * (1 - pReal)).
  p_real <- 0.6
  
  # Probability that an element in B or C will not be masked out.
  p_bc_mask <- 0.8
  
  # Probability that an element in D will not be masked out.
  p_d_mask <- 0.3
  
  # Probability that D = 0.
  p_d_zero = 0.5
  
  # Make some poles for A.
  poles <- complex(length.out = states, 0, 0)
 
  i <- 1
  
  while (i <= states)
  {
    # Probability of copying poles (not applicable for 1st or last)
    if (runif(1) < p_repeat & i!=1 & i != states)
    {
      if (Im(poles[i-1]) == 0)
      {
        poles[i] <- poles[i-1]
        i <- i + 1
      }
      else
      {
        poles[i:(i+2)] <- poles[(i-2):i]
        i <- i + 2
      }
    }
    else if ((runif(1) < p_real) | (i == states))
    {
      if (type == "C")
      {
        poles[i] <- complex(real = -exp(runif(1)), imaginary = Im(poles[i]))
      }
      else
      {
        poles[i] <- complex(real = 2 * runif(1) - 1, imaginary = Im(poles[i]))
      }
      
      i <- i + 1
    }
    else
    {
      # Complex conjugate pair of oscillating poles.
      if (type == "C")
      {
        poles[i] <- complex(real = -exp(runif(1)), imaginary = 3 * exp(runif(1)))
      }
      else
      {
        mag <- runif(1)
        phase <- 2 * pi * runif(1)
        
        poles[i] <- complex(real = mag * cos(phase), imaginary = mag * sin(phase))
      }
      
      poles[i+1] <- Conj(poles[i])
      i <- i+2
    }
  }
  
  # Create A from the poles
  A <- diag(Re(poles))
  mask <- which(Im(poles) != 0)
  mask <- mask[mask != states]
  
  if (length(mask) > 0)
  {
    A[mask+1, mask+1] <- Re(poles[mask])
    A[mask, mask+1] <- Im(poles[mask])
    A[mask+1, mask] <- -Im(poles[mask])
  }
  
  t <- matrix(rnorm(states * states), nrow = states, ncol = states)
  A <- solve(t, A) %*% t
  
  B <- matrix(rnorm(states * inputs),  nrow = states)
  C <- matrix(rnorm(outputs * states), nrow = outputs)
  D <- matrix(rnorm(outputs * inputs), nrow = outputs)
  
  B_mask <- matrix(runif(states * inputs), nrow = states) < p_bc_mask
  C_mask <- matrix(runif(outputs * states), nrow = outputs) < p_bc_mask
  
  D_mask <- NULL
  
  if (runif(1) < p_d_zero)
  {
    D_mask <- matrix(0, nrow = outputs, ncol = inputs)
  }
  else
  {
    D_mask <- matrix(runif(outputs * inputs), nrow = outputs) > p_d_mask
  }
  
  # Apply masks
  B <- B * B_mask
  C <- C * C_mask
  
  D <- matrix(0,  nrow = outputs, ncol = inputs)
  
  return (list(A = A, B = B, C = C, D = D))
}