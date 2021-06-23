library(ggplot2)
library(MASS)
set.seed(0)

# Set up toy data ---------------------------------------------------------

# Gradient
x <- 3

# Range
a <- seq(-2, 2, by = 0.25)

# Add noise
b <- a * x + rnorm(length(a))

# SVD for Linear Fit
svd_results <- svd(a)
x_tilda <- svd_results$v %*% solve(svd_results$d) %*% t(svd_results$u) %*% b

raw_data <- data.frame(x = a, y = b, type = "noisy")

results <- rbind(data.frame(x = a, y = x * a, type = "true value"),
                 data.frame(x = a, y = x_tilda[1] * a, type = "linear fit"))

ggplot(results, aes(x, y, colour = type)) + 
  geom_line() + 
  geom_point(data = raw_data)


# Compare the 3 methods of Linear Regression ------------------------------

x_tilda_1 <- svd_results$v %*% solve(svd_results$d) %*% t(svd_results$u) %*% b
x_tilda_2 <- ginv(a) %*% b

# -1 for no y intercept, coef to extract the values.
x_tilda_3 <- coef(lm(b ~ -1 + a))$a

# Check all equal.
(x_tilda_1 == x_tilda_2) & (x_tilda_3 == x_tilda_2)
