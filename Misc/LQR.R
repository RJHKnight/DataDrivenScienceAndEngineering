library(control)
library(slicotr)

my_lqr <- function(A,B,q,r){

  # Compute the G matrix
  G <- (B %*% 1/r) %*% t(B)
  
  res2 <- sb02md("C", "D", "U", "N", "S", ncol(A), A, G, q, 8*nrow(A))
  
  k = solve(r, t(B) %*% res2$q)
  s = res2$q
  e = complex(real = res2$wr, imaginary = res2$wi)
  
  return (list(k = k, s = s, e = e))
}
