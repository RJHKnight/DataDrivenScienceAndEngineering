library(tidyverse)

hald <- read_csv(file = "Data/hald.csv")
A <- as.matrix(select(hald, -heat))
b <- pull(hald, heat)

svd_results <- svd(A)
x <- svd_results$v %*% ginv(diag(svd_results$d)) %*% t(svd_results$u) %*% b

regression_results <- (A %*% x)[,1]

# Add regression results and convert to long format for plotting.
hald_results <- hald %>% 
  mutate(id = 1:n()) %>% 
  mutate(regression = regression_results) %>% 
  select(-contains("x")) %>% 
  pivot_longer(names_to = "type", values_to = "heat", -id)


ggplot(hald_results, aes(id, heat, colour = type)) + 
  geom_line() + 
  geom_point()


# Alternative 1 - lm
lm_coef <- coef(lm(heat ~ -1 + ., data = hald))
# Strip names from lm coef so we can compare
attributes(lm_coef)$names <- NULL

# Alternative 2- ginv
givn_coef <- ginv(A) %*% b

# Check all equal
all.equal(lm_coef, x[,1]) && all.equal(x[,1], givn_coef[,1])
