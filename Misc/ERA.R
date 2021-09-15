ERA <- function(YY, m, n, n_in, n_out, r)
{
  d_r <- matrix(NA, nrow = n_out, ncol = n_in)
  t <- dim(YY)[3]
  y <- array(NA, dim = c(n_out, n_in, t-1))
  
  for (i in 1:n_out)
  {
    for (j in 1:n_in)
    {
      d_r[i, j] <- YY[i, j, 1]
      y[i,j,] <- YY[i,j,-1]
    }
  }
  
  h_row_size <- (n_out*m)
  h_col_size <- (n_in * n)
  
  h <- matrix(NA, nrow = h_row_size, ncol = h_col_size)
  h2 <- matrix(NA, nrow = h_row_size, ncol = h_col_size)
  
  for (i in 1:m)
  {
    for (j in 1:n)
    {
      for (q in 1:n_out)
      {
        for (p in 1:n_in)
        {
          h[(n_out*i)-n_out+q, (n_in * j)-n_in+p] <- y[q,p,i+j-1]
          h2[(n_out*i)-n_out+q, (n_in * j)-n_in+p] <- y[q,p,i+j]
        }
      }
    }
  }
  
  svd_res <- svd(h)
  sigma <- diag(svd_res$d[1:r])
  u_r <- svd_res$u[,1:r]
  v_r <- svd_res$v[,1:r]
  
  sigma_bar <- sigma ^ -0.5
  sigma_bar[is.infinite(sigma_bar)] <- 0
  
  a_r <- sigma_bar %*% t(u_r) %*% h2 %*% v_r %*% sigma_bar
  b_r <- sigma_bar %*% t(u_r) %*% h[,1:n_in]
  c_r <- h[1:n_out,] %*% v_r %*% sigma_bar
  
  return (list(a_r = a_r,
               b_r = b_r,
               c_r = c_r,
               d_r = d_r,
               hsv = svd_res$d))
}