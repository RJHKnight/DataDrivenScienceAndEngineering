library(tidyverse)
source("Misc/Utils.R")

n <- 1024
w <- exp((-1i * 2 * pi) / n)

# Slow looping
dft_slow <- function(n)
{
  dft <- matrix(nrow = n, ncol = n)
  
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      dft[i,j] <- w^((i-1)*(j-1))
    }
  }
  
  return (dft)
}

# Fast - vectorised
dft_fast <- function(n)
{
  # Fast
  i <- 1:n
  j <- 1:n
  
  dft_fast <- w^((i-1)%*%t(j-1))
  
  return (dft_fast)
}

system.time(dft_slow(n))
system.time(dft_fast(n))
all.equal(dft_slow(n), dft_fast(n))

dft <- dft_fast(n)

plot_matrix(Re(dft))
