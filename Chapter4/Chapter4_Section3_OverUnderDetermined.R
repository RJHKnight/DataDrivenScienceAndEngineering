library(CVXR)
library(tidyverse)
library(patchwork)
library(MASS)

# Underdetermined ---------------------------------------------------------

n <- 20
m <- 100
a <- matrix(runif(n*m), nrow = n)
b <- runif(n)

# L2 norm
x2_var <- Variable(m)
objective <- Minimize(norm(x2_var, "2"))
constraint <- a %*% x2_var - b == 0
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
x2 <- result$getValue(x2_var)

# L1 norm
x1_var <- Variable(m)
objective <- Minimize(norm(x1_var, "1"))
constraint <- a %*% x1_var - b == 0
problem <- Problem(objective, constraints = c(constraint))
result <- solve(problem)
x1 <- result$getValue(x1_var)

under_df <- rbind(
  data.frame(x = x1, type = "l1"),
  data.frame(x = x2, type = "l2")) %>% 
  group_by(type) %>% 
  mutate(position = 1:n()) %>% 
  ungroup() + 
  theme(legend.position = "none")

bar_plot <- ggplot(under_df, aes(position, x, fill = type)) + 
  geom_col() + 
  facet_wrap(~ type, ncol = 1) + 
  theme_bw()

hist_plot <- ggplot(under_df, aes(x, fill = type)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~ type, nrow =  1) +
  theme_bw() + 
  theme(legend.position = "bottom")

bar_plot / hist_plot +
  plot_layout(heights = c(2, 1))



# Overdetermined ----------------------------------------------------------

n <- 500
m <- 100
a <- matrix(runif(n*m), nrow = n)
b <- runif(n)

x_l2 <- ginv(a) %*% b

overdet_optim <- function(lambda, a, b)
{
  x_var <- Variable(m)
  objective <- Minimize(norm(a %*% x_var - b, "2") + (lambda * norm(x_var, "1")))
  problem <- Problem(objective)
  result <- solve(problem)
  
  ret_df <- data.frame(x = result$getValue(x_var), lambda = lambda) %>% 
    mutate(position = 1:n())
  
  return (ret_df)
}

lambdas <- c(0.0, 0.1, 0.5)

over_df <- map_dfr(lambdas, overdet_optim, a = a, b = b) 
over_df <- mutate(over_df, lambda = as.factor(lambda))

bar_plot <- ggplot(over_df, aes(position, x, fill = lambda)) + 
  geom_col() + 
  facet_wrap(~ lambda, ncol = 1, scales = "free") + 
  theme_bw()

hist_plot <- ggplot(over_df, aes(x, fill = lambda)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~ lambda, nrow =  1, scales = "free") +
  theme_bw() + 
  theme(legend.position = "bottom")

bar_plot / hist_plot +
  plot_layout(heights = c(2, 1))
