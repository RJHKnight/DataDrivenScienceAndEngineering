library(tidyverse)
library(patchwork)
library(MASS)
library(CVXR)

# Solve y = Theta * s for "s"
n <- 1000     # Dim of S
p <- 200      # Number of observations (dim y)

theta <- matrix(rnorm(p * n), nrow = p)
y <- rnorm(p)

# L1 norm
s_hat <- Variable(n)
objective <- Minimize(norm(s_hat, "1"))
constraint_1 <- theta %*% s_hat == y
problem <- Problem(objective, constraints = c(constraint_1))
result <- solve(problem)
s_l1 <- result$getValue(s_hat)

# L2 norm
s_l2 <- ginv(theta) %*% y

# Plotting
s_l1_df <- data.frame(s = s_l1) %>% 
  mutate(n = 1:n()) %>% 
  mutate(type = "l1")

s_l2_df <- data.frame(s = s_l2) %>% 
  mutate(n = 1:n()) %>% 
  mutate(type = "l2")

s_df <- rbind(s_l1_df, s_l2_df)

# Raw plot of S
p1 <- ggplot(s_df, aes(n, s, colour = type)) + 
  geom_line() +
  facet_wrap(~ type, nrow = 1) +
  theme(legend.position = "none") + 
  theme_bw()

# Histogram of S
p2 <- ggplot(s_df, aes(s, fill = type)) + 
  geom_histogram() +
  facet_wrap(~ type, nrow = 1) +
  theme(legend.position = "none") + 
  theme_bw()

p1 / p2