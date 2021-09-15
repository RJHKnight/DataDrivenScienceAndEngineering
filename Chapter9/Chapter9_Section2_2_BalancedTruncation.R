library(control)
library(tidyverse)
library(patchwork)
library(rmatio)
source("Misc/DRSS.R")
source("Misc/Balred.R")


sys_full <- rmatio::read.mat("DATA/testSys_ABCD.mat")

bal_sys <- balred(sys_full$A, sys_full$B, sys_full$C,  nrow(sys_full$B))

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
sys_bt <- balred(sys_full$A, sys_full$B, sys_full$C, r)



# Compute BPOD ------------------------------------------------------------
impulse_length <- 0:((r*5)+1)
sys_full <- ss(sys_full$A, sys_full$B, sys_full$C, sys_full$D,1)
u_impulse_1 <- matrix(0, ncol = 2, nrow = length(impulse_length))
u_impulse_2 <- matrix(0, ncol = 2, nrow = length(impulse_length))
u_impulse_1[1,1] <- 1
u_impulse_2[1,2] <- 1

y <- matrix(0, nrow = nrow(u_impulse), ncol = 2)
x <- matrix(0, nrow = ncol(sys_full$A), 1)

for (k in 1 : nrow(u_impulse))
{
  y[k,] <- t((sys_full$C %*% x)  +  sys_full$D %*% u_impulse_1[k,])
  x <- (sys_full$A %*% x)  + (sys_full$B %*% u_impulse_1[k,])
}


lsim(sys_full, u_impulse_2, t = impulse_length)

impulseplot(sys_full, impulse_length)

sys_adj <- ss(t(sys_full$A), t(sys_full$C), t(sys_full$B), t(sys_full$D))
impulse_adj <- impulse(sys_adj, seq(0, (r*5)+1, 0.01))

# Hankel Matric H = OC