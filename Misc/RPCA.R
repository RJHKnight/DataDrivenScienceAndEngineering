rpca <- function(x)
{
  dim_x <- dim(x)
  mu <- (dim_x[1] * dim_x[2]) / (4 * sum(abs(x)))
  lambda <- 1 / sqrt(max(dim_x[1], dim_x[2]))
  threshold <- 1e-7 * norm(x, "f")
  
  l <- matrix(0, nrow = dim_x[1], ncol = dim_x[2])
  s <- matrix(0, nrow = dim_x[1], ncol = dim_x[2])
  y <- matrix(0, nrow = dim_x[1], ncol = dim_x[2])
  count <- 0
  
  while ((norm(x-l-s, "f") > threshold) & (count < 1000))
  {
    this_threshold <- norm(x-l-s, "f")
    cat(paste("Iteration:", count, "xls norm:", this_threshold, "\n"))
    
    l <- svt(x-s+(1/mu)*y, 1/mu)
    s <- shrink_tau(x-l+(1/mu)*y, lambda/mu)
    y <- y + mu*(x-l-s)
    count <- count + 1
  }
  
  return (list(l = l, s = s))
}

shrink_tau <- function(x, tau)
{
  sign(x) * pmax(abs(x)-tau, 0)
}

svt <- function(s, tau)
{
  svd_res <- svd(s)
  
  return (svd_res$u %*% shrink_tau(diag(svd_res$d), tau) %*% t(svd_res$v))
}