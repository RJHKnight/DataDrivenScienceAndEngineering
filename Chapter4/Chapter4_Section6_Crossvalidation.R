library(tidyverse)
library(glmnet)
library(patchwork)
library(MASS)
source("Misc/Utils.R")
source("Chapter4/MultipleRegressionUtils.R")


# As before - x train is 1 -> 4, phi is 'm' powers of x from 1 -> 4
# x test is 4 -> 8
# True function is x^2
n <- 100
l <- 4
x_train <- seq(from = 0, to = l, length.out = n)
x_test <- seq(from = l, to = 2*l, length.out = n)

m <- seq(1, 4, length.out = 13)
phi_train <- sapply(m, function(p) x_train^(p-1))
phi_test <- sapply(m, function(p) x_test^(p-1))

# Run multiple times and return an average of beta
run_one_cv <- function(num_runs, x, phi)
{
  mutli_reg_res <- NULL
  mutli_reg_pred <- NULL
  
  for (i in 1:num_runs)
  {
    this_y <- x^2 + (0.1 * rnorm(length(x)))
    this_res <- run_multiple_regression(this_y, phi, i)
    mutli_reg_res <- rbind(mutli_reg_res, this_res)
  }
  
  mutli_reg_res <- mutate(mutli_reg_res, power = as.factor(power))
  
  return (
    mutli_reg_res %>%
      group_by(type, power) %>% 
      summarise(beta = mean(beta)) %>% 
      mutate(num_runs = num_runs)
  ) 
}

cv_res <- map_dfr(c(2, 100, 1000), run_one_cv, x = x_train, phi = phi_train)

ggplot(cv_res, aes(power, beta, fill = type)) + 
  geom_col(position = "dodge") +
  facet_wrap(~ num_runs, ncol = 1, scales = "free")


# Out of sample
y_test <- x_test^2 + (0.1 * rnorm(length(x_test)))
test_prediction <- predict_multiple(y_test, x_test, phi_test, filter(cv_res, num_runs == 1000), 1)
train_prediction <- predict_multiple(NA, x_train, phi_train, filter(cv_res, num_runs == 1000), 1)

rbind(test_prediction, train_prediction) %>% 
  ggplot(aes(x, y, colour = pred_type)) + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) + 
  theme_bw() + 
  annotate("rect", xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf, alpha = 0.2) + 
  annotate("text", x = 0.25, y = 65, label = "Training") + 
  annotate("text", x = 4.25, y = 65, label = "Testing")
