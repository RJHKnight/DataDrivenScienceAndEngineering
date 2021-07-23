library(tidyverse)
library(glmnet)
library(patchwork)
library(MASS)
source("Misc/Utils.R")

n <- 100
l <- 4
x <- seq(from = 0, to = l, length.out = n)
y <- x^2 
  
# Max polynomial degree
m <- seq(1, 4, length.out = 13)
phi <- sapply(m, function(p) x^(p-1))


# Least squares -----------------------------------------------------------

run_one_least_squares <- function(x, phi, run_num)
{
  y <- x^2 + (0.2 * rnorm(length(x)))
  this_beta <- ginv(phi) %*% y
  
  ret_df <- data.frame(beta = this_beta) %>% 
    mutate(coef = 1:n()) %>% 
    mutate(run_num = run_num)
  
  return (ret_df)
}

least_squares_res <- map_dfr(1:4, run_one_least_squares, x = x, phi = phi) %>% 
  mutate(run_num = as.factor(run_num))

beta_plot <- ggplot(least_squares_res, aes(coef, beta, fill = run_num)) +
  geom_col() +
  facet_wrap(~ run_num, nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none")

y_plot <- qplot(x, y, geom = "line") + 
  theme_bw()

y_plot / beta_plot



# Multiple Regressions ----------------------------------------------------

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

num_runs <- 100
mutli_reg_res <- NULL
mutli_reg_pred <- NULL

for (i in 1:num_runs)
{
  this_y <- x^2 + (0.1 * rnorm(length(x)))
  this_res <- run_multiple_regression(this_y, phi, i)
  
  this_prediction <- predict_multiple(this_y, x, phi, this_res, i)
  
  mutli_reg_res <- rbind(mutli_reg_res, this_res)
  mutli_reg_pred <- rbind(mutli_reg_pred, this_prediction)
}

mutli_reg_res <- mutate(mutli_reg_res, power = as.factor(power))

ggplot(mutli_reg_res, aes(power, beta, fill = type)) +
  geom_boxplot() + 
  facet_wrap(~ type, scales = "free", ncol = 1)

# Pick 1 run and show the fit
mutli_reg_pred %>% 
  filter(run_num == 1) %>% 
  ggplot(aes(x, y, colour = pred_type)) + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) + 
  theme_bw()


# Out of sample
x_test <- seq(from = l, to = 2*l, length.out = n)
y_test <- x_full^2 + (0.1 * rnorm(length(x_full)))
phi_test <- sapply(m, function(p) x_full^(p-1))

test_prediction <- predict_multiple(y_test, x_test, phi_test, filter(mutli_reg_res, run_num == 1), 1)
train_prediction <- mutli_reg_pred %>% filter(run_num == 1)
  
rbind(test_prediction, train_prediction) %>% 
  ggplot(aes(x, y, colour = pred_type)) + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) + 
  theme_bw() + 
  annotate("rect", xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf, alpha = 0.2) + 
  annotate("text", x = 0.25, y = 65, label = "Training") + 
  annotate("text", x = 4.25, y = 65, label = "Testing")

