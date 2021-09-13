library(control)
library(tidyverse)
library(patchwork)
source("Misc/DRSS.R")
source("Misc/Balred.R")

q <- 2
p <- 2
n <- 100

sys_full <- drss(n, p, q, strictly_proper = FALSE, type = "C")

bal_sys <- balred(sys_full$A, sys_full$B, sys_full$C, sys_full$D, nrow(sys_full$B))

hsv_df <- data.frame(hsv = bal_sys$hsv) %>% 
  mutate(rank = 1:n()) %>% 
  mutate(cum_hsv = cumsum(hsv) / sum(hsv))
 

p1 <- ggplot(hsv_df, aes(rank, hsv)) + 
  geom_line() + 
  scale_y_log10() + 
  theme_bw()

p2 <- ggplot(hsv_df, aes(rank, cum_hsv)) + 
  geom_line() + 
  theme_bw()

p1 + p2

r <- 10
sys_bt <- balred(sys_full$A, sys_full$B, sys_full$C, sys_full$D, r)
