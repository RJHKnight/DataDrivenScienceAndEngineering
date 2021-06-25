library(tidyverse)
library(patchwork)
library(rgl)


ovarian_cancer <- read_csv("Data/ovariancancer.csv", col_names = FALSE)

# Last column is the outcome
ovarian_cancer <- rename(ovarian_cancer, outcome = X4001)
obs <- as.matrix(select(ovarian_cancer, contains("X")))

svd_results <- svd(obs)

singular_values <- data.frame(id = 1:length(svd_results$d), variance = svd_results$d) %>% 
  mutate(pct_total_variance = cumsum(variance) / sum(variance))


# Scree Plots -------------------------------------------------------------

variance_plot <- ggplot(singular_values, aes(id, variance)) + 
  geom_point() + 
  geom_line() + 
  scale_y_log10()

pct_total_plot <- ggplot(singular_values, aes(id, pct_total_variance)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent)

variance_plot + pct_total_plot


# Plot of first 3 Principle Comps -----------------------------------------

pc_res <- t(svd_results$v[,1:3]) %*% t(obs) 
pc_res_df <- data.frame(t(pc_res))
pc_res_df$result <- if_else(ovarian_cancer$outcome == "Cancer", "red", "green")

plot3d(pc_res_df$X1,
       pc_res_df$X2,
       pc_res_df$X3,
       col = pc_res_df$result)
