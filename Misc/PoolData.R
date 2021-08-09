pool_data <- function(y_in, poly_order, col_names)
{
  n <- nrow(y_in)
  n_vars <- ncol(y_in)
  
  # Polynomial order 0
  y_out <- rep(1, n)
  
  # Polynomial order 1
  y_out <- cbind(y_out, y_in)
  colnames(y_out)[1] <- "intercept"
  colnames(y_out)[2:(n_vars+1)] <- col_names
  
  # Polynomial order 2
  if (poly_order >= 2)
  {
    y_tmp <- matrix(NA, nrow = n, ncol = n_vars^2)
    colnames(y_tmp) <- rep("", n_vars^2)
    
    for (i in 1:n_vars)
    {
      for (j in i:n_vars)
      {
        this_index <- ((i-1) * n_vars) + (j)
        y_tmp[,this_index] <- y_in[,i] * y_in[,j]
        colnames(y_tmp)[this_index] <- paste0(col_names[i], "*" , col_names[j])
      }
    }
    
    y_tmp <- y_tmp[,!is.na(y_tmp[1,])]
    y_out <- cbind(y_out, y_tmp)
  }
  
  # Polynomial order 3
  if (poly_order >= 3)
  {
    y_tmp <- matrix(NA, nrow = n, ncol = n_vars^3)
    colnames(y_tmp) <- rep("", n_vars^3)
    
    for (i in 1:n_vars)
    {
      for (j in i:n_vars)
      {
        for (k in j:n_vars)
        {
          this_index <- ((i-1) * (n_vars^2)) + ((j-1) * n_vars) + k
          y_tmp[,this_index] <- y_in[,i] * y_in[,j] * y_in[,k]
          colnames(y_tmp)[this_index] <- paste0(col_names[i], "*" , col_names[j], "*", col_names[k])
        }
      }
    }
    
    y_tmp <- y_tmp[,!is.na(y_tmp[1,])]
    y_out <- cbind(y_out, y_tmp)
  }
  
  return (y_out)
}