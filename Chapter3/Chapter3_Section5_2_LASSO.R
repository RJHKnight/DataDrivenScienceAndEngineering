library(glmnet)
library(tidyverse)
library(MASS)

a <- matrix(rnorm(100 * 10), nrow = 100)
x <- c(0,0,1,0,0,0,-1,0,0,0)
b <- a %*% x + 2 * rnorm(100)

x_l2_fit <- ginv(a) %*% b

res_lasso <- cv.glmnet(a, b, alpha = 1, intercept = FALSE)

# Crossvalidation SE vs. Lambda
plot(res_lasso)

# Lambda plot
plot(res_lasso$glmnet.fit)

# The largest value of lambda that is within 1 s.e. of the min
res_lasso$lambda.1se

# Index of 1 s.e from min value
index_1se <- res_lasso$index[2]

# Correctly identifies 3rd and 7th predictors as important, and get the sign correct but the scale is wrong.
x_lasso_fit <- res_lasso$glmnet.fit$beta[,index_1se]

# De-bias using a simple least squares regression - only against the non-zero coefficients.
x_lasso_fit_debiased <- ginv(a[, abs(x_lasso_fit) > 0]) %*% b
