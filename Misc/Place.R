library(slicotr)

place <- function(A, B, p)
{
  res <- sb01bd("D", ncol(A), ncol(B), ncol(A), 0, A, B, Re(p), Im(p), 0, 5*ncol(A))
  
  return (-res$f)
}