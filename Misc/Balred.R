library(pracma)
library(slicotr)

balred <- function(A, B, C, D, orders)
{
  dico <- 'C'
  job <- 'B'
  alpha <- 0
  equil <- 'N'
  
  n <- nrow(A)
  m <- ncol(B)
  p <- nrow(C)
  ld_work <- n*(2*n+max(n,m,p)+5) + n*(n+1)/2
  
  # Check for stability
  if (any(Re(eig(A)) > 0))
  {
    # Unstable
    res <- ab09md(dico, job, equil, "A", n, m, p, orders, 0, A, B, C, 0.0 , ld_work)
  }
  else
  {
    # Stable
    res <- ab09ad(dico, job, equil, 'A', n, m, p, orders, A, B, C, 0.0 , ld_work)
  }
  
  return (res)
}