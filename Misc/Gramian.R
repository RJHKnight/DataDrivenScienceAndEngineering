obs_gram <- function(A, C)
{
  C <- -t(C) %*% C
  # A'W + WA + C'C = 0
  res <- sb03md("C", "X", "N", "N", ncol(A), A, matrix(0, nrow = nrow(A), ncol = ncol(A)), C, 2 * ncol(A)^2)
  
  return (res$c)
}

ctrl_gram <- function(A, B)
{
  C <- -B %*% t(B)

  res <- sb03md("C", "X", "N", "T", ncol(A), A, matrix(0, nrow = nrow(A), ncol = ncol(A)), C, 2 * ncol(A)^2)
  
  return (res$c)
}