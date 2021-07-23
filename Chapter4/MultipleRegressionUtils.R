run_multiple_regression <- function(y, phi, run_num)
{
  # Least squares
  beta_1 <- ginv(phi) %*% y                                             
  # Lasso with no L2
  beta_2 <- as.numeric(coef(glmnet(phi, y, lambda = 0.2, alpha = 1, intercept = FALSE)))[-1]     
  # Lasso with L1 and L2 penalty
  beta_3 <- as.numeric(coef(glmnet(phi, y, lambda = 0.2, alpha = 0.8, intercept = FALSE)))[-1]   
  
  beta_res <- rbind(
    data.frame(beta = beta_1, type = "l2"),
    data.frame(beta = beta_2, type = "lasso (alpha = 1)"),
    data.frame(beta = beta_3, type = "lasso (alpha = 0.8")
  )
  
  beta_res <- beta_res %>% 
    group_by(type) %>% 
    mutate(power = m) %>% 
    ungroup() %>% 
    mutate(run_num = run_num)
  
  return (beta_res)
}

predict_multiple <- function(y, x, phi, beta_frame, run_num)
{
  beta_frame %>% 
    group_by(type) %>% 
    summarise(
      y_pred = (phi %*% beta)[,1],
      y_actual = y,
      x = x
    ) %>% 
    pivot_longer(contains("y_"), names_to = "pred_type", values_to = "y") %>% 
    mutate(run_num = run_num)
}


run_one_least_squares <- function(x, phi, run_num)
{
  y <- x^2 + (0.2 * rnorm(length(x)))
  this_beta <- ginv(phi) %*% y
  
  ret_df <- data.frame(beta = this_beta) %>% 
    mutate(coef = 1:n()) %>% 
    mutate(run_num = run_num)
  
  return (ret_df)
}