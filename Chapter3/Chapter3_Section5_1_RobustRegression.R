library(tidyverse)
library(CVXR)
library(MASS)

# Linear dependency with gaussian white noise
x <- sort(4 * (runif(25) - 0.5), decreasing = FALSE)
a <- 0.9
b <- (a * x) + (0.1 * rnorm(length(x)))

# L2 norm fit (least squares)
a_true <- ginv(x) %*% b

# Add outlier
b[length(x)] <- -5.5

# L2 norm with outlier
a_outlier <- ginv(x) %*% b

a_l1_var <- Variable(1)
objective <- Minimize(norm((a_l1_var * x)-b, "1"))
problem <- Problem(objective)
result <- solve(problem)
a_l1 <- result$getValue(a_l1_var)

true_df <- data.frame(x = x, b = b) %>% 
  mutate(is_outlier = row_number() == n())

fit_df <- data.frame(
  x = x,
  `l2 - no outlier`   = c(a_true) * x,
  `l2 - with outlier` = c(a_outlier) * x,
  `l1 - with outlier` = c(a_l1) * x,
  check.names = FALSE) %>% 
  pivot_longer(-x, names_to = "type", values_to = "b")

ggplot(fit_df, aes(x, b, colour = type)) + 
  geom_line() + 
  geom_point(data = true_df, aes(shape = is_outlier), colour = "black") + 
  theme_bw()