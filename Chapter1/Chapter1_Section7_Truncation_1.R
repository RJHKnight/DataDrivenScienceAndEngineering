library(tidyverse)
source("Misc/Utils.R")
library(patchwork)

# True matrix -------------------------------------------------------------

t <- seq(-3, 3, by=0.01)

u_true <- cbind(cos(17 * t) * exp(-t^2), sin(11 * t))
s_true <- diag(c(2, 0.5))
v_true <- cbind(sin(5 * t) * exp(-t^2), cos(13 * t))

x <- u_true %*% s_true %*% t(v_true)

plot_matrix(x)

# Add noise ---------------------------------------------------------------

sigma <- 1
n <- nrow(x)
m <- ncol(x)
x_noisy <- x + sigma * matrix(rnorm(n * m), ncol = m)

plot_matrix(x_noisy)

svd_res <- svd(x_noisy)


# Hard threshold with known sigma and square X ----------------------------

cutoff <- (4/sqrt(3)) * sqrt(n) * sigma

r <- max(which(svd_res$d > cutoff))
x_clean <- svd_res$u[, 1:r] %*% diag(svd_res$d[1:r]) %*% t(svd_res$v[, 1:r])

plot_matrix(x_clean)


# Cumulative Energy Cutoff ------------------------------------------------

cds <- cumsum(svd_res$d) / sum(svd_res$d)
r_90 <- min(which(cds > 0.9))

x_90 <- svd_res$u[, 1:r_90] %*% diag(svd_res$d[1:r_90]) %*% t(svd_res$v[, 1:r_90])
plot_matrix(x_90)


# Singular Value Plots ----------------------------------------------------

singular_values <- data.frame(s = svd_res$d) %>% 
  mutate(j = 1:n()) %>% 
  mutate(cum_s = cumsum(s) / sum(s)) %>% 
  mutate(type = case_when(
    s > cutoff ~ "Above Cutoff", 
    j < r_90   ~ "90th Percentile",
    TRUE       ~ "Other"))

base_plot <- ggplot(singular_values, aes(j, s)) + 
  geom_line() + 
  geom_point(aes(colour = type)) + 
  scale_y_log10() + 
  geom_hline(yintercept = cutoff, colour = "red", linetype = "dashed") + 
  theme_bw() + 
  theme(legend.position = "none")

p1 <- base_plot + 
  annotate("rect", xmin = -5, xmax = 100, ymin = 20, ymax = 200, alpha = .2) + 
  ggtitle("Singular Values")

p2 <- base_plot + 
  scale_x_continuous(limit = c(-5, 100)) +
  scale_y_log10(limits = c(20, 200)) + 
  ggtitle("Singular Values - Zoomed")

p3 <- ggplot(singular_values, aes(j, cum_s)) +
  geom_line() + 
  geom_point(aes(colour = type)) + 
  annotate("segment", x = r_90, xend = r_90, y = -Inf, yend = 0.9) + 
  annotate("segment", x = -Inf, xend = r_90, y = 0.9, yend = 0.9) +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ggtitle("90% Cutoff")
  
(p1 + p2) / p3
